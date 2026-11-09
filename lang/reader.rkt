#lang s-exp syntax/module-reader

fml/base

#:read        fml-read
#:read-syntax fml-read-syntax
#:whole-body-readers? #t

(require (only-in fml/parser/parse parse-fml)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in racket/port port->string)
         (only-in fml/error fml-error-diagnostic)
         (only-in fhir/diagnostic raise-diagnostic diagnostic-severity)
         (only-in fhir/model/registry type-registered?)
         (only-in fml/check check-map))

(define (source-name src) (cond [(path? src) (path->string src)] [(string? src) src] [else #f]))
(define (fml-read-syntax src in)
  (define r (parse-fml (port->string in) #:source (source-name src)))
  (cond
    [(ok? r)
     (check! (ok-value r) (source-name src))
     (list (datum->syntax #f (list 'fml-map-form (ok-value r))))]
    [else (let ([d (fml-error-diagnostic (err-error r))])
            (if d (raise-diagnostic d) (error 'fml (format "~a" (err-error r)))))]))

;; check! : map source -> void   the map checked against its structures when the module compiles
;; (`raco make`, `racket`): an element the type does not have, a value that cannot go there, an
;; unknown group — an error at its rule, as `raco fml check` reports it; nothing it cannot decide.
;; The checker's types are the registered FHIR version's (R5 when none is yet).
(define (check! m source)
  (unless (type-registered? 'Patient) (dynamic-require 'fhir/r5/schemas #f))
  (define errors (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (check-map m #:source source)))
  (when (pair? errors) (raise-diagnostic (car errors))))

(define (fml-read in) (map syntax->datum (fml-read-syntax #f in)))
