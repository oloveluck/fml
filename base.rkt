#lang racket/base

;;; base.rkt — the module language for #lang fml: a .map file is one FML mapping; the module
;;; provides the parsed AST, the compiled StructureMap (jsexpr), and `transform`: the map as a
;;; procedure over a source resource (FHIR JSON, or a typed node), answering FHIR JSON — so a map
;;; composes with the other FHIR languages' modules; `#:structures` gives the StructureDefinitions of the
;;; map's logical models (a list of jsexprs, or a data source). Run the file to print the StructureMap.

(require (for-syntax racket/base syntax/parse (only-in fml/parser/ast fml-map?))
         (only-in fml/structuremap fml->structuremap)
         (only-in fml/transform run-map uses-types register-structure-map! current-empty-targets)
         (only-in fhir/registry register-resources!)
         (only-in fhir/data-source current-data-source)
         (only-in fml/registry structures-source resource-node)
         (only-in fhir/serialize/fhir-json write-fhir-json)
         (only-in fhir/serialize/lenient lenient-drops drops-message)
         (only-in racket/string string-join string-replace)
         (only-in fhir/result/result ok? ok-value)
         (only-in fhir/model/node fhir?)
         (only-in fml/check validate-result profiles-from)
         (only-in fml/parser/parse parse-fml)
         (only-in fhir/diagnostic diagnostic->string diagnostic-severity)
         (only-in fhir/operation exn:fail:operation)
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
(define (check-result! fml out spec [source #f] [drops '()])
  (define profiles (cond [(eq? spec #t) '()]
                         [(list? spec) (append-map profiles-from spec)]
                         [else (profiles-from spec)]))
  ;; `drops`: what the lenient read left out of the source, so a rule that "matched nothing" because
  ;; its value was unreadable is said so (check.rkt's validate-result, which `raco fml check
  ;; --examples` uses too)
  (define errors (filter (lambda (d) (eq? (diagnostic-severity d) 'error))
                         (validate-result fml out #:profiles profiles #:source source #:drops drops)))
  ;; raised as `exn:fail:operation` (an exn:fail:user carrying an OperationOutcome), as `ensure-valid`
  ;; raises, so one handler — `exn:fail:operation?` — catches an invalid result wherever it is found
  ;; (a new user's documented handler caught nothing: this raised a plain exn:fail:user)
  (unless (null? errors)
    (define lines (map (lambda (d) (cwd-relative (diagnostic->string d))) errors))
    (raise (exn:fail:operation
            (format "the map's ~a is invalid against ~a (FHIR ~a):\n  ~a"
                    (if (hash? out) (hash-ref out 'resourceType "result") "result")
                    (against profiles out) (validated-in)
                    (string-join lines "\n  "))
            (current-continuation-marks)
            (hasheq 'resourceType "OperationOutcome"
                    'issue (for/list ([l (in-list lines)])
                             (hasheq 'severity "error" 'code "invalid" 'diagnostics l)))))))

;; cwd-relative : text -> text   a file named in full in a message, named from the current directory
;; (as `raco fsh check` names its files) when it is below it
(define (cwd-relative text)
  (define cwd (path->string (path->directory-path (simplify-path (current-directory)))))
  (string-replace text cwd ""))

;; against : profiles result -> string   what the result was held to: "IntakePatient (http://…)", or its base type
(define (against profiles out)
  (define mine (filter (lambda (sd) (equal? (hash-ref sd 'type #f) (and (hash? out) (hash-ref out 'resourceType #f)))) profiles))
  (if (null? mine)
      (format "the base type ~a" (if (hash? out) (hash-ref out 'resourceType "?") "?"))
      (string-join (for/list ([sd (in-list mine)]) (format "~a (~a)" (hash-ref sd 'name (hash-ref sd 'id "?")) (hash-ref sd 'url "?"))) ", ")))

;; validated-in : -> string   the FHIR version validation reads: the core package in context, else R5
(define (validated-in)
  (define pkgs (map (lambda (p) (if (path? p) (path->string p) (format "~a" p)))
                    (or ((dynamic-require 'fhir/sd/sd-index 'current-sd-packages)) '())))
  (cond [(ormap (lambda (p) (regexp-match? #rx"r4[.]core" p)) pkgs) "R4 (4.0.1)"]
        [(ormap (lambda (p) (regexp-match? #rx"r6[.]core" p)) pkgs) "R6"]
        [else "R5 (5.0.0)"]))

;; as-node : FHIR JSON | typed node -> typed node   read leniently into the registered types (FML's
;; engine needs them), what the read left out reported
(define (as-node src [strict? #f])
  (define node (resource-node src 'transform))
  (unless (fhir? src) (report-drops! src node strict?))
  node)

;; report-drops! : JSON node strict? -> void   a value of the source the lenient read left out (a date
;; that is not a date, an element the R5 type lacks) is said — the map never sees it, so a later
;; "required element missing" would otherwise blame the wrong thing; #:strict? makes it an error
(define (report-drops! src node strict?)
  (define drops (lenient-drops src node))
  (unless (null? drops)
    (define msg (format "the source's ~a" (drops-message drops (hash-ref src 'resourceType "its type"))))
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
          (parameterize ([current-data-source (structures-source structures)])
            (uses-types fml)                                 ; the logical models, before the source is read
            (define node (as-node src strict?))
            (parameterize ([current-empty-targets (make-hasheq)])   ; which values came out empty
              (define out (run-map (located) node))          ; positioned, so a rule error names its line
              (when validate
                (check-result! (located) out validate (~? (quote form.source) #f)
                               (if (fhir? src) '() (lenient-drops src node))))
              out)))
        (provide fml structuremap transform)
        (module+ main (write-fhir-json structuremap #:indent 2) (newline)))]))
