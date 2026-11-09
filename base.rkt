#lang racket/base

;;; base.rkt — the module language for #lang fml: a .map file is one FML mapping; the module
;;; provides the parsed AST, the compiled StructureMap (jsexpr), and `transform`: the map as a
;;; procedure over a source resource (FHIR JSON, or a typed node), answering FHIR JSON — so a map
;;; composes with the other FHIR languages' modules; `#:structures` gives the StructureDefinitions of the
;;; map's logical models (a list of jsexprs, or a data source). Run the file to print the StructureMap.

(require (for-syntax racket/base syntax/parse (only-in fml/parser/ast fml-map?)) json
         (only-in fml/structuremap fml->structuremap)
         (only-in fml/transform run-map uses-types register-structure-map!)
         (only-in fhir/registry register-resources!)
         (only-in fhir/data-source current-data-source data-source? resources->data-source)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/serialize/lenient lenient-drops drop->string)
         (only-in racket/string string-join)
         (only-in fhir/result/result ok? ok-value)
         (only-in fhir/model/node fhir?))

;; as-node : FHIR JSON | typed node -> typed node   (read leniently into the registered tower; FML's
;; engine needs one — fhir/r5/schemas when nothing else registered a tower)
(define (as-node src [strict? #f])
  (cond [(fhir? src) src]
        [else (dynamic-require 'fhir/r5/schemas #f)
              (define r (from-json src #:lenient? #t))
              (unless (ok? r) (raise-argument-error 'transform "a FHIR resource" src))
              (report-drops! src (ok-value r) strict?)
              (ok-value r)]))

;; report-drops! : JSON node strict? -> void   a value of the source the lenient read left out (a date
;; that is not a date, an element the R5 type lacks) is said — the map never sees it, so a later
;; "required element missing" would otherwise blame the wrong thing; #:strict? makes it an error
(define (report-drops! src node strict?)
  (define drops (lenient-drops src node))
  (unless (null? drops)
    (define msg (format "the source's ~a not read: not a valid value, or not an element of ~a in FHIR R5"
                        (string-join (map drop->string drops) ", ") (hash-ref src 'resourceType "its type")))
    (if strict?
        (raise (exn:fail (string-append "transform: " msg) (current-continuation-marks)))
        (eprintf "fml warning: ~a\n" msg))))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [fml-module-begin #%module-begin])
         fml-map-form)

(define-syntax-rule (fml-map-form ast) (quote ast))

;; the module body the reader produces: exactly one (fml-map-form <fml-map>) — anything else is a
;; precise error here, not a failure deep in the template
(begin-for-syntax
  (define-syntax-class map-form
    #:description "one (fml-map-form <map>) form, as the #lang fml reader writes it"
    (pattern ((~datum fml-map-form) ast)
             #:fail-unless (fml-map? (syntax->datum #'ast)) "not an FML map AST")))

(define-syntax (fml-module-begin stx)
  (syntax-parse stx
    [(_ form:map-form)
     #'(#%module-begin
        (define fml form)
        (register-structure-map! fml)                        ; resolvable by its url in this process
        (define structuremap (fml->structuremap fml))
        (register-resources! (list structuremap) #:source (variable-reference->module-source (#%variable-reference)))
        (define (transform src #:structures [structures #f] #:strict? [strict? #f])
          (parameterize ([current-data-source
                          (cond [(not structures) (current-data-source)]
                                [(data-source? structures) structures]
                                [else (resources->data-source structures)])])
            (uses-types fml)                                 ; the logical models, before the source is read
            (run-map fml (as-node src strict?))))
        (provide fml structuremap transform)
        (module+ main (displayln (jsexpr->string structuremap))))]))
