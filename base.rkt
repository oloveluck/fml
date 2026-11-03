#lang racket/base

;;; base.rkt — the module language for #lang fml: a .map file is one FML mapping; the module
;;; provides the parsed AST, the compiled StructureMap (jsexpr), and `transform`: the map as a
;;; one-argument procedure over a source resource (FHIR JSON, or a typed node), answering FHIR JSON —
;;; so a map composes with the other FHIR languages' modules. Run the file to print the StructureMap.

(require (for-syntax racket/base) json
         (only-in fml/structuremap fml->structuremap)
         (only-in fml/transform run-map)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok? ok-value)
         (only-in fhir/model/node fhir?))

;; as-node : FHIR JSON | typed node -> typed node   (read leniently into the registered tower; FML's
;; engine needs one — fhir/r5/schemas when nothing else registered a tower)
(define (as-node src)
  (cond [(fhir? src) src]
        [else (dynamic-require 'fhir/r5/schemas #f)
              (define r (from-json src #:lenient? #t))
              (if (ok? r) (ok-value r) (raise-argument-error 'transform "a FHIR resource" src))]))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [fml-module-begin #%module-begin])
         fml-map-form)

(define-syntax-rule (fml-map-form ast) (quote ast))

(define-syntax (fml-module-begin stx)
  (syntax-case stx ()
    [(_ form)
     #'(#%module-begin
        (define fml form)
        (define structuremap (fml->structuremap fml))
        (define (transform src) (run-map fml (as-node src)))
        (provide fml structuremap transform)
        (module+ main (displayln (jsexpr->string structuremap))))]))
