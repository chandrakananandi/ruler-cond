#lang racket

;; Arithmetic identities for rewriting programs.

(require "../common.rkt" "../errors.rkt" "types.rkt" "syntax.rkt" "sugar.rkt")

(provide (struct-out rule) *rules* *simplify-rules* *fp-safe-simplify-rules*)
(module+ internals (provide define-ruleset define-ruleset* register-ruleset!
                            *rulesets* generate-rules-for *templated-reprs*))

;; Rulesets
(define *rulesets* (make-parameter '()))

;; Cached rules
(define all-rules (make-parameter '()))
(define simplify-rules (make-parameter '()))
(define fp-safe-simplify-rules (make-parameter '()))

;; Exported parameters to update and access rules
(define *rules*
  (make-derived-parameter all-rules 
                          identity 
                          (λ (_) (generate-missing-rules) (all-rules))))

(define *simplify-rules*
  (make-derived-parameter simplify-rules 
                          identity 
                          (λ (_) (generate-missing-rules) (simplify-rules))))

(define *fp-safe-simplify-rules*
  (make-derived-parameter fp-safe-simplify-rules 
                          identity 
                          (λ (_) (generate-missing-rules) (fp-safe-simplify-rules))))

;; Update parameters

;; Note on rules
;; fp-safe-simplify ⊂ simplify ⊂ all
;;
;; all                    requires at least one tag of an active group of rules
;; simplify               same req. as all + 'simplify' tag
;; fp-safe-simplify       same req. as simplify + 'fp-safe' tag       ('fp-safe' does not imply 'simplify')
;;

(struct rule (name input output itypes otype)
        ;; Input and output are patterns
        ;; itypes is a mapping, variable name -> representation
        ;; otype is a representation
        #:methods gen:custom-write
        [(define (write-proc rule port mode)
           (fprintf port "#<rule ~a>" (rule-name rule)))])

(define (rule-ops-supported? rule)
  (define (ops-in-expr expr)
    (cond
      [(list? expr)
       (and (impl-exists? (car expr))
            (for/and ([subexpr (cdr expr)])
              (ops-in-expr subexpr)))]
      [else true]))
  (ops-in-expr (rule-output rule)))

(register-reset
 #:priority 10 ; Must be higher than priority for pruning operators
 (λ ()
   (*rulesets*
    (for/list ([ruleset (*rulesets*)])
      (match-define (list rules groups types) ruleset)
      (list (filter rule-ops-supported? rules) groups types)))))

(define (update-rules rules groups)
  (when (ormap (curry flag-set? 'rules) groups)       ; update all
    (all-rules (append (all-rules) rules))
    (when (set-member? groups 'simplify)              ; update simplify
      (simplify-rules (append (simplify-rules) rules))  
      (when (set-member? groups 'fp-safe)         ; update fp-safe
        (fp-safe-simplify-rules (append (fp-safe-simplify-rules) rules))))))

(define (reprs-in-expr expr)
  (remove-duplicates
    (let loop ([expr expr])
      (match expr
      [(list 'if cond ift iff)
        (append (loop cond) (loop ift) (loop iff))]
      [(list op args ...)
        (append (operator-info op 'itype) (append-map loop args))]
      [_ '()]))))

(define (type-of-rule input output ctx)
  (cond   ; special case for 'if', return the 'type-of-rule' of the ift branch
    [(list? input) 
      (if (equal? (car input) 'if)
          (type-of-rule (caddr input) output ctx)
          (operator-info (car input) 'otype))]
    [(list? output)
      (if (equal? (car output) 'if)
          (type-of-rule input (caddr output) ctx)
          (operator-info (car output) 'otype))]
    [(symbol? input) (dict-ref ctx input)]   ; fallback: if symbol, check ctx for type
    [(symbol? output) (dict-ref ctx output)]
    [else
      (error 'type-of-rule "Could not compute type of rule ~a -> ~a"
              input output)]))

;; Rulesets defined by reprs. These rulesets are unique
(define (register-ruleset! name groups var-ctx rules)
  (define rules*
    (for/list ([r rules])
      (match-define (list rname input output) r)
      (rule rname input output var-ctx
            (type-of-rule input output var-ctx))))
  (*rulesets* (cons (list rules* groups var-ctx) (*rulesets*))))
      
(define-syntax define-ruleset
  (syntax-rules ()
   [(define-ruleset name groups [rname input output] ...)
    (define-ruleset name groups #:type () [rname input output] ...)]
   [(define-ruleset name groups #:type ([var type] ...) [rname input output] ...)
    (register-ruleset! 'name 'groups `((var . ,(get-representation 'type)) ...)
                       '((rname input output) ...))]))

;; Templated rulesets defined by types. These are used to generate duplicate rules that
;; are valid in any representation of the same underlying type.

(define *templated-rulesets* (make-parameter '()))
(define *templated-reprs* (make-parameter '()))

(define-syntax define-ruleset*
  (syntax-rules ()
    [(_ name groups [rname input output] ...)
     (define-ruleset* name groups #:type () [rname input output] ...)]
    [(_ name groups #:type ([var type] ...) [rname input output] ...)
     (begin
      (define name (list (rule 'rname 'input 'output '((var . type) ...) 'unknown) ...))
      (*templated-rulesets* (cons (list name 'groups '((var . type) ...)) 
                                  (*templated-rulesets*))))]))

(define *reprs-with-rules* (make-parameter '()))

;; Add existing rules in rulesets to 'active' rules

(define (add-rules-from-rulesets repr)
  (*reprs-with-rules* (set-add (*reprs-with-rules*) repr)) ; update
  (define valid? (curry set-member? (*reprs-with-rules*)))

  (define (valid-rule r)
    (define in-reprs (reprs-in-expr (rule-input r)))
    (define out-reprs (reprs-in-expr (rule-output r)))
    (define all-reprs (set-union (list (rule-otype r)) in-reprs out-reprs))
    (and (andmap valid? all-reprs) (ormap (curry equal? repr) all-reprs)))

  (for ([set (*rulesets*)])
    (match-define `((,rules ...) (,groups ...) ((,vars . ,types) ...)) set)
    (when (andmap valid? types)
      (define rules* (filter valid-rule rules))
      (unless (empty? rules*)   ; only add the ruleset if it contains one
        (update-rules rules* groups)))))

;; Generate a set of rules by replacing a generic type with a repr
(define (generate-rules-for type repr)
  (define valid? (disjoin (curry equal? type)
                          (curry set-member? (map representation-name (*reprs-with-rules*)))))
  (*templated-reprs* (set-add (*templated-reprs*) repr)) ; update
  (for ([set (reverse (*templated-rulesets*))] ; preserve rule order
       #:when (or (empty? (third set)) ; no type ctx
                  (andmap (λ (p) (valid? (cdr p))) (third set))))
    (match-define `((,rules ...) (,groups ...) ((,vars . ,types) ...)) set)
    (define var-reprs
      (for/list ([v vars] [t types])
        (if (equal? t type) repr (get-representation t))))
    (define ctx (context vars repr var-reprs))
    (define vrhash (map cons vars var-reprs))
    (define rules*
      (for/fold ([rules* '()]) ([r rules])
        (with-handlers ([exn:fail:user:herbie:missing? (const rules*)])
          (define name* (sym-append (rule-name r) '_ (representation-name repr)))
          (define input* (desugar-program (rule-input r) ctx #:full #f))
          (define output* (desugar-program (rule-output r) ctx #:full #f))
          (define rule* (rule name* input* output* vrhash (type-of-rule input* output* vrhash)))
          (cons rule* rules*))))
    (unless (empty? rules*)   ; only add the ruleset if it contains one
      (*rulesets* (cons (list rules* groups vrhash) (*rulesets*))))))

;; Generate rules for new reprs

(define (generate-missing-rules)
  (for ([repr (*needed-reprs*)])
    (unless (set-member? (*templated-reprs*) repr)
      (generate-rules-for (representation-type repr) repr))
    (unless (set-member? (*reprs-with-rules*) repr)
      (add-rules-from-rulesets repr))))

(define-ruleset* compare-reduce (bools simplify fp-safe-nan)
  #:type ([x real] [y real])
  [lt-same      (<  x x)         (FALSE)]
  [gt-same      (>  x x)         (FALSE)]
  [lte-same     (<= x x)         (TRUE)]
  [gte-same     (>= x x)         (TRUE)]
  [not-lt       (not (<  x y))   (>= x y)]
  [not-gt       (not (>  x y))   (<= x y)]
  [not-lte      (not (<= x y))   (>  x y)]
  [not-gte      (not (>= x y))   (<  x y)])

(define-ruleset* branch-reduce (branches simplify fp-safe)
  #:type ([a bool] [b bool] [x real] [y real])
  [if-true        (if (TRUE) x y)       x]
  [if-false       (if (FALSE) x y)      y]
  [if-same        (if a x x)          x]
  [if-not         (if (not a) x y)    (if a y x)]
  [if-if-or       (if a x (if b x y)) (if (or a b) x y)]
  [if-if-or-not   (if a x (if b y x)) (if (or a (not b)) x y)]
  [if-if-and      (if a (if b x y) y) (if (and a b) x y)]
  [if-if-and-not  (if a (if b y x) y) (if (and a (not b)) x y)])
