#lang racket/base

;;; base.rkt — the module language for #lang fhir/fml: a .map file is one FML mapping; the module
;;; provides the parsed AST, the compiled StructureMap (jsexpr), and `transform` (a one-argument
;;; runner over a typed source resource). Run the file to print the StructureMap JSON.

(require (for-syntax racket/base) json
         (only-in fhir/fml/structuremap fml->structuremap)
         (only-in fhir/fml/transform run-map))

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
        (define (transform src) (run-map fml src))
        (provide fml structuremap transform)
        (module+ main (displayln (jsexpr->string structuremap))))]))
