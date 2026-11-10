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
         (only-in fhir/model/node fhir?)
         (only-in fml/check validate-result profiles-from)
         (only-in fml/parser/parse parse-fml)
         (only-in fhir/diagnostic diagnostic->string diagnostic-severity)
         (only-in racket/list append-map))

;; located-map : map text|#f source|#f -> map   the map parsed again from its text, so its rules carry
;; their positions (the module's quoted AST does not); the map itself when there is no text
(define (located-map m text source)
  (define r (and text (parse-fml text #:source source)))
  (if (and r (ok? r)) (ok-value r) m))

;; check-result! : map result spec -> void   `transform #:validate`: the result held to its base type,
;; the profiles it claims and those the spec names (#t: none more; a canonical, an SD, an FSH
;; project, or a list of them); an error raises exn:fail:user with each problem at the rule that
;; wrote the element, or saying no rule writes it — in a pipeline, `ensure-valid` after the map can
;; only name the element
(define (check-result! fml out spec [source #f])
  (define profiles (cond [(eq? spec #t) '()]
                         [(list? spec) (append-map profiles-from spec)]
                         [else (profiles-from spec)]))
  (define errors (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (validate-result fml out #:profiles profiles #:source source)))
  (unless (null? errors)
    (raise-user-error (format "the map's ~a is invalid:\n  ~a" (if (hash? out) (hash-ref out 'resourceType "result") "result")
                              (string-join (map diagnostic->string errors) "\n  ")))))

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

(define-syntax fml-map-form
  (syntax-rules () [(_ ast) (quote ast)] [(_ ast text source) (quote ast)]))

;; the module body the reader produces: exactly one (fml-map-form <fml-map>) — anything else is a
;; precise error here, not a failure deep in the template
(begin-for-syntax
  (define-syntax-class map-form
    #:description "one (fml-map-form <map>) form, as the #lang fml reader writes it"
    (pattern ((~datum fml-map-form) ast (~optional (~seq text source)))
             #:fail-unless (fml-map? (syntax->datum #'ast)) "not an FML map AST")))

(define-syntax (fml-module-begin stx)
  (syntax-parse stx
    [(_ form:map-form)
     #'(#%module-begin
        (define fml form)
        (define located                                      ; the map with its rules' positions
          (let ([m #f]) (lambda () (unless m (set! m (located-map fml (~? (quote form.text) #f) (~? (quote form.source) #f)))) m)))
        (register-structure-map! fml)                        ; resolvable by its url in this process
        (define structuremap (fml->structuremap fml))
        (register-resources! (list structuremap) #:source (variable-reference->module-source (#%variable-reference)))
        (define (transform src #:structures [structures #f] #:strict? [strict? #f] #:validate [validate #f])
          (parameterize ([current-data-source
                          (cond [(not structures) (current-data-source)]
                                [(data-source? structures) structures]
                                [else (resources->data-source structures)])])
            (uses-types fml)                                 ; the logical models, before the source is read
            (define out (run-map fml (as-node src strict?)))
            (when validate (check-result! (located) out validate (~? (quote form.source) #f)))
            out))
        (provide fml structuremap transform)
        (module+ main (displayln (jsexpr->string structuremap))))]))
