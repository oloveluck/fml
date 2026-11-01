#lang s-exp syntax/module-reader

fhir/fml/base

#:read        fml-read
#:read-syntax fml-read-syntax
#:whole-body-readers? #t

(require (only-in fhir/fml/parser/parse parse-fml)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in racket/port port->string))

(define (fml-read-syntax src in)
  (define r (parse-fml (port->string in)))
  (if (ok? r)
      (list (datum->syntax #f (list 'fml-map-form (ok-value r))))
      (error 'fml (format "~a" (err-error r)))))

(define (fml-read in) (map syntax->datum (fml-read-syntax #f in)))
