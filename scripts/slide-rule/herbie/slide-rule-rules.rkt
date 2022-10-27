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

;; Ruler rules
(define-ruleset*
 ruler-simplify
 (ruler simplify arithmetic)
 #:type
 ((a real) (b real) (c real))
 (rules-1 (- c (- b a)) (- a (- b c)))
 (rules-2 (* c (* b a)) (* b (* c a)))
 (rules-3 (- (- c b) a) (- (- c a) b))
 (rules-4-rev (+ b (+ a c)) (+ c (+ b a)))
 (rules-4 (+ c (+ b a)) (+ b (+ a c)))
 (rules-5-rev (+ c (- b a)) (- (+ c b) a))
 (rules-5 (- (+ c b) a) (+ c (- b a)))
 (rules-6-rev (+ c (- a b)) (- c (- b a)))
 (rules-6 (- c (- b a)) (+ c (- a b)))
 (rules-7-rev (/ (/ c b) a) (/ c (* b a)))
 (rules-7 (/ c (* b a)) (/ (/ c b) a))
 (rules-8-rev (- (- c b) a) (- c (+ b a)))
 (rules-8 (- c (+ b a)) (- (- c b) a))
 (rules-9-rev (- (/ c a) (/ b a)) (/ (- c b) a))
 (rules-9 (/ (- c b) a) (- (/ c a) (/ b a)))
 (rules-10-rev (* b (- c a)) (- (* b c) (* b a)))
 (rules-10 (- (* b c) (* b a)) (* b (- c a)))
 (rules-11-rev (+ (/ b a) (/ c a)) (/ (+ c b) a))
 (rules-11 (/ (+ c b) a) (+ (/ b a) (/ c a)))
 (rules-12-rev (* b (+ c a)) (+ (* c b) (* b a)))
 (rules-12 (+ (* c b) (* b a)) (* b (+ c a)))
 (rules-13-rev (- c (/ b a)) (/ (- (* c a) b) a))
 (rules-13 (/ (- (* c a) b) a) (- c (/ b a)))
 (rules-14 (/ a (- (/ c b) a)) (/ b (- (/ c a) b)))
 (rules-15 (* b a) (* a b))
 (rules-16 (+ b a) (+ a b))
 (rules-17 (* b (+ a a)) (* a (+ b b)))
 (rules-18-rev (/ (+ b b) (+ a a)) (/ b a))
 (rules-18 (/ b a) (/ (+ b b) (+ a a)))
 (rules-19-rev (* (- b a) (+ b a)) (- (* b b) (* a a)))
 (rules-19 (- (* b b) (* a a)) (* (- b a) (+ b a)))
 (rules-20 (- (sqr (sin b)) (sqr (cos a))) (- (sqr (sin a)) (sqr (cos b))))
 (rules-21-rev (- (sqr (cos a)) (sqr (cos b))) (- (sqr (sin b)) (sqr (sin a))))
 (rules-21 (- (sqr (sin b)) (sqr (sin a))) (- (sqr (cos a)) (sqr (cos b))))
 (rules-22-rev (neg (neg a)) a)
 (rules-23-rev (cos (neg a)) (cos a))
 (rules-23 (cos a) (cos (neg a)))
 (rules-24-rev (sin (neg a)) (neg (sin a)))
 (rules-24 (neg (sin a)) (sin (neg a)))
 (rules-25-rev (tan (neg a)) (neg (tan a)))
 (rules-25 (neg (tan a)) (tan (neg a)))
 (rules-26-rev (sin (- (PI) a)) (sin a))
 (rules-26 (sin a) (sin (- (PI) a)))
 (rules-27-rev (tan (+ a (PI))) (tan a))
 (rules-27 (tan a) (tan (+ a (PI))))
 (rules-28-rev (cos (+ (PI) a)) (neg (cos a)))
 (rules-28 (neg (cos a)) (cos (+ (PI) a)))
 (rules-29-rev (* a 1) a)
 (rules-30-rev (+ a 0) a)
 (rules-31-rev (/ a 1) a)
 (rules-32-rev (- a 0) a)
 (rules-33 (- a a) 0)
 (rules-34 (/ a a) 1)
 (rules-35-rev (/ a -1) (neg a))
 (rules-35 (neg a) (/ a -1))
 (rules-36-rev (* a -1) (neg a))
 (rules-36 (neg a) (* a -1))
 (rules-37-rev (- 0 a) (neg a))
 (rules-37 (neg a) (- 0 a))
 (rules-38-rev (* 2 a) (+ a a))
 (rules-38 (+ a a) (* 2 a))
 (rules-39 (+ (sqr (sin a)) (sqr (cos a))) 1)
 (rules-40 (/ 0 a) 0)
 (rules-41 (* a 0) 0)
 (rules-42-rev (+ a 1) (- a -1))
 (rules-42 (- a -1) (+ a 1))
 (rules-43-rev (+ a -1) (- a 1))
 (rules-43 (- a 1) (+ a -1))
 (rules-44-rev (sin (PI)) (tan (PI)))
 (rules-44 (tan (PI)) (sin (PI)))
 (rules-45-rev (sin (+ (PI) (PI))) (sin (PI)))
 (rules-45 (sin (PI)) (sin (+ (PI) (PI))))
 (rules-46-rev (sin (PI)) (tan (+ (PI) (PI))))
 (rules-46 (tan (+ (PI) (PI))) (sin (PI)))
 (rules-47-rev (sin (PI)) 0)
 (rules-48-rev (cos (PI)) -1)
 (rules-49-rev (cos (+ (PI) (PI))) 1)
 (rules-50-rev (tan 0) 0)
 (rules-51-rev (sin 0) 0)
 (rules-52-rev (cos 0) 1)
 (rules-53-rev (cos (/ (PI) 2)) 0)
 (rules-54-rev (sin (/ (PI) 2)) 1))

(define-ruleset*
 ruler-non-simplify
 (ruler arithmetic)
 #:type
 ((a real) (b real) (c real))
 (rules-22 a (neg (neg a)))
 (rules-29 a (* a 1))
 (rules-30 a (+ a 0))
 (rules-31 a (/ a 1))
 (rules-32 a (- a 0))
 (rules-47 0 (sin (PI)))
 (rules-48 -1 (cos (PI)))
 (rules-49 1 (cos (+ (PI) (PI))))
 (rules-50 0 (tan 0))
 (rules-51 0 (sin 0))
 (rules-52 1 (cos 0))
 (rules-53 0 (cos (/ (PI) 2)))
 (rules-54 1 (sin (/ (PI) 2))))
