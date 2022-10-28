#lang racket

(require json)

(define counter 1)

(define (parse-string s)
  (and s (call-with-input-string s read)))

(define (parse-expr expr)
  (define vars (mutable-set))
  (values
    (let loop ([expr (parse-string expr)])
      (match expr
       [(list '~ arg)
        (list 'neg (loop arg))]
       [(list 'sqr arg)
        (define arg* (loop arg))
        (list '* arg* arg*)]
       [(list op args ...)
        (cons op (map loop args))]
       [(? number?) expr]
       [(? symbol?)
        (define str-repr (symbol->string expr))
        (if (eq? (string-ref str-repr 0) #\?)
            (begin0 (string->symbol (substring str-repr 1))
                (set-add! vars expr))
            (list expr))]))
    (set-count vars)))

(define (expr-has-op? expr)
  (match expr
   [(list op head rest ...) #t]
   [_ #f]))
   
(define (parse-rule rule)
  (define lhs (hash-ref rule 'lhs))
  (define rhs (hash-ref rule 'rhs))
  (define bi? (hash-ref rule 'bidirectional))
  (define-values (lhs* lh-varc) (parse-expr lhs))
  (define-values (rhs* rh-varc) (parse-expr rhs))
  (define forward
    (list (string->symbol (format "rules-~a" counter))
          lhs*
          rhs*
          (and (expr-has-op? lhs*) 'simplify)))
  (define backward
    (list (string->symbol (format "rules-~a-rev" counter))
          rhs*
          lhs*
          (and (expr-has-op? rhs*) 'simplify)))
  (define rules (if bi? (list forward backward) (list forward)))
  (begin0 (values rules (max lh-varc rh-varc))
    (set! counter (+ counter 1))))

(define (parse-ruler-rules file tags outp new-eqs-only? old-format?)
  (define json (call-with-input-file file read-json))
  (define json-key (if old-format? 'eqs (if new-eqs-only? 'new_eqs 'all_eqs)))
  (define rules (hash-ref json json-key))
  (set! counter 1)
  (define-values (parsed-rules max-vars)
    (let loop ([rules rules] [parsed '()] [max-var 0])
        (if (null? rules)
            (values (reverse parsed) max-var)
            (let-values ([(parse varc) (parse-rule (car rules))])
              (loop (cdr rules) (append parse parsed) (max max-var varc))))))
  (define-values (simplify non-simplify)
    (partition (λ (r) (eq? (fourth r) 'simplify)) parsed-rules))
  (pretty-print-newline outp 3)
  (pretty-display ";; Ruler rules" outp)
  (pretty-display
    (list* 'define-ruleset* 'ruler-simplify (append '(ruler simplify) tags)
      "#:type" (build-list max-vars (λ (n) (list (integer->char (+ 97 n)) 'real)))
      (map (λ (r) (take r 3)) simplify))
    outp)
  (pretty-print-newline outp 1)
  (pretty-display
    (list* 'define-ruleset* 'ruler-non-simplify (cons 'ruler tags)
      "#:type" (build-list max-vars (λ (n) (list (integer->char (+ 97 n)) 'real)))
      (map (λ (r) (take r 3)) non-simplify))
    outp))

(module+ main
  (define new-eqs-only? #f)
  (define old-format? #f)
  (define tags '())
  (command-line
   #:once-each
    [("--old-format") "old format"
      (set! old-format? #t)]
    [("--tags") _tags "tags to attach to rules"
      (set! tags (string-split _tags " "))]
    [("--new-eqs") "only extracts from new_eqs"
      (set! new-eqs-only? #t)]
   #:args (template rules out)
   (copy-file template out #t)
   (printf "Creating `rules.rkt` for Herbie...\n")
   (printf " addt. tags   : ~a\n" (string-join tags ", "))
   (printf " use new eqs? : ~a\n" new-eqs-only?)
   (printf " template     : ~a\n" template)
   (printf " rules (JSON) : ~a\n" rules)
   (printf " destination  : ~a\n" out)
   (define outp (open-output-file out #:exists 'append))
   (parse-ruler-rules rules (map string->symbol tags) outp new-eqs-only? old-format?)))
