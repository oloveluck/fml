#lang s-exp syntax/module-reader

fml/base

#:read        fml-read
#:read-syntax fml-read-syntax
#:whole-body-readers? #t

(require (only-in fml/parser/parse parse-fml)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in racket/port port->string)
         (only-in fml/error fml-error-diagnostic)
         (only-in fhir/diagnostic raise-diagnostic))

(define (source-name src) (cond [(path? src) (path->string src)] [(string? src) src] [else #f]))
(define (fml-read-syntax src in)
  (define r (parse-fml (port->string in) #:source (source-name src)))
  (if (ok? r)
      (list (datum->syntax #f (list 'fml-map-form (ok-value r))))
      (let ([d (fml-error-diagnostic (err-error r))])
        (if d (raise-diagnostic d) (error 'fml (format "~a" (err-error r)))))))

(define (fml-read in) (map syntax->datum (fml-read-syntax #f in)))
