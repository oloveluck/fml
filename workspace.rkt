#lang racket/base

;;; workspace.rkt — what FML offers a workspace (fhir/lang/workspace): each map as its StructureMap
;;; with the checker's findings, a map run over the data source's resources of its source type (each
;;; result validated), and the explanation of one transform: the result and every problem in it,
;;; traced to the rule that wrote the element.
;;;
;;; Resources are typed by the workspace's FHIR version. A map's `uses` resolve as anywhere else:
;;; the towers, then StructureDefinitions in the data source, the workspace (fhir/registry), the packages.
;;; A Row of a run is {source: "Type/id", target, errors, resource} (or {source, error} when the
;;; transform fails).

(require racket/contract racket/list racket/string racket/match racket/runtime-path racket/file json
         fhir/lang/workspace
         (only-in fhir/diagnostic diagnostic loc exn:fail:diagnostic exn:fail:diagnostic? exn:fail:diagnostic-diagnostic
                  diagnostic->jsexpr)
         (only-in fhir/data-source current-data-source data-source-all data-source-read)
         (only-in fhir/registry registered-resources)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in fhir/serialize/from-json from-json)
         (only-in "parser/parse.rkt" parse-fml)
         (only-in "error.rkt" fml-error-diagnostic fml-error-string)
         (only-in "structuremap.rkt" fml->structuremap)
         (only-in "transform.rkt" run-map uses-types)
         (only-in "registry.rkt" structure-map smap-source)
         (only-in "check.rkt" check-map check-map/examples))

(provide (contract-out [workspace-support ws-support?]
                       ;; guide : the language guide (guide.md) a hub serves to agents (learn://fml)
                       [guide string?]))

(define-runtime-path guide-file "guide.md")
(define guide (file->string guide-file))

(define (path-string p) (if (path? p) (path->string p) p))

;; parse : text path|#f -> fml-map   raises the positioned diagnostic
(define (parse text path)
  (define r (parse-fml (regexp-replace #px"^#lang[^\n]*" text "") #:source (and path (path-string path))))
  (if (ok? r) (ok-value r)
      (let* ([e (err-error r)]
             [d (or (fml-error-diagnostic e) (diagnostic 'fml 'parse 'error (fml-error-string e) (loc (and path (path-string path)) #f #f #f #f)))])
        (raise (exn:fail:diagnostic (fml-error-string e) (current-continuation-marks) d)))))

(define (load-tower! ws) (dynamic-require (if (workspace-r4? ws) 'fhir/r4/schemas 'fhir/r5/schemas) #f))

(define (artifacts ws files)
  (load-tower! ws)
  (for/fold ([rs '()] [ds '()] #:result (values (reverse rs) ds)) ([f (in-list files)])
    (with-handlers ([exn:fail:diagnostic? (lambda (e) (values rs (append ds (list (exn:fail:diagnostic-diagnostic e)))))]
                    [exn:fail? (lambda (e) (values rs (append ds (list (diagnostic 'fml 'compile 'error (exn-message e) (loc (path-string (car f)) #f #f #f #f))))))])
      (define m (parse (cdr f) (car f)))
      (values (cons (fml->structuremap m) rs) (append ds (check-map m #:source (car f)))))))

;; sources : map options -> (listof resource)   `over` (a type or Type/id), else the map's source type
(define (sources m opts)
  (define ds (current-data-source))
  (define over (or (hash-ref opts 'over #f)
                   (let ([t (smap-source (structure-map m))]) (and t (symbol->string t)))
                   (error 'fml "the map's source type is not known: say `over` a resource type or Type/id")))
  (match over
    [(pregexp #px"^([A-Za-z]+)/(.+)$" (list _ type id))
     (define r ((data-source-read ds) type id))
     (unless r (error 'fml (format "no ~a/~a in the data source" type id)))
     (list r)]
    [type ((data-source-all ds) type)]))

(define (typed r)
  (define n (from-json r #:lenient? #t))
  (unless (ok? n) (error 'fml (format "~a/~a does not read as FHIR" (hash-ref r 'resourceType "?") (hash-ref r 'id "?"))))
  (ok-value n))
(define (reference r) (format "~a/~a" (hash-ref r 'resourceType "?") (hash-ref r 'id "?")))

;; validation-errors : resource -> count|'null   by $validate (fhir-validation) when installed
(define (validation-errors r)
  (with-handlers ([exn:fail? (lambda (_) 'null)])
    (dynamic-require 'fhir/conformance/operations #f)
    (define oo ((dynamic-require 'fhir/operation 'apply-operation) "$validate" r))
    (for/sum ([i (in-list (hash-ref oo 'issue '()))]) (if (member (hash-ref i 'severity #f) '("error" "fatal")) 1 0))))

(define (run ws path text opts)
  (load-tower! ws)
  (define m (parse text path))
  (uses-types m)
  (define rows
    (for/list ([src (in-list (sources m opts))])
      (with-handlers ([exn:fail? (lambda (e) (hasheq 'source (reference src) 'error (exn-message e)))])
        (define out (run-map m (typed src)))
        (hasheq 'source (reference src)
                'target (if (hash-ref out 'id #f) (reference out) (hash-ref out 'resourceType 'null))
                'errors (validation-errors out)
                'resource out))))
  (run-result "transform" '("source" "target" "errors" "resource") rows
              (hasheq 'sources (length rows)
                      'failed (count (lambda (r) (hash-has-key? r 'error)) rows)
                      'invalid (count (lambda (r) (let ([e (hash-ref r 'errors 0)]) (and (number? e) (positive? e)))) rows))))

(define (explain ws path text subject opts)
  (load-tower! ws)
  (define m (parse text path))
  (uses-types m)
  (define src (let ([rs (sources m (if (regexp-match? #rx"/" subject) (hash-set opts 'over subject) opts))])
                (or (if (regexp-match? #rx"/" subject) (car rs) (findf (lambda (r) (equal? (hash-ref r 'id #f) subject)) rs))
                    (error 'fml (format "no source resource with id ~a" subject)))))
  (define out (with-handlers ([exn:fail:diagnostic? (lambda (e) (hasheq 'error (diagnostic->jsexpr (exn:fail:diagnostic-diagnostic e))))]
                              [exn:fail? (lambda (e) (hasheq 'error (exn-message e)))])
                (run-map m (typed src))))
  (hasheq 'source (reference src)
          'result out
          'problems (if (hash-has-key? out 'error) '()
                        (map diagnostic->jsexpr
                             (check-map/examples m (list src) #:profiles (registered-resources "StructureDefinition")
                                                 #:source (and path (path-string path)))))))

(define workspace-support (ws-support "fml" artifacts run explain))

(module+ test
  (require rackunit (only-in fhir/data-source resources->data-source))
  (define-values (ws _) (open-workspace (find-system-path 'temp-dir)))
  (define map-text (string-append
                    "map \"http://example.org/StructureMap/qr2patient\" = \"QR2Patient\"\n"
                    "uses \"http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse\" as source\n"
                    "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" as target\n"
                    "group main(source src : QuestionnaireResponse, target tgt : Patient) {\n"
                    "  src.item as item where linkId = 'gender' -> tgt.gender = (item.answer.value.code);\n"
                    "}\n"))
  (define-values (rs ds) (artifacts ws (list (cons "/tmp/qr.map" map-text) (cons "/tmp/bad.map" "map \"x\" = "))))
  (check-equal? (map (lambda (r) (hash-ref r 'url)) rs) '("http://example.org/StructureMap/qr2patient"))
  (check-true (pair? ds))
  (define qr (hasheq 'resourceType "QuestionnaireResponse" 'id "q1" 'status "completed"
                     'item (list (hasheq 'linkId "gender" 'answer (list (hasheq 'valueCoding (hasheq 'code "female")))))))
  (parameterize ([current-data-source (resources->data-source (list qr))])
    (define r (run ws #f map-text (hasheq)))
    (check-equal? (length (run-result-rows r)) 1)
    (define row (car (run-result-rows r)))
    (check-equal? (hash-ref row 'source) "QuestionnaireResponse/q1")
    (check-equal? (hash-ref (hash-ref row 'resource) 'gender) "female")
    (define e (explain ws #f map-text "q1" (hasheq)))
    (check-equal? (hash-ref (hash-ref e 'result) 'gender) "female")
    (check-exn #rx"no source resource" (lambda () (explain ws #f map-text "zz" (hasheq))))))
