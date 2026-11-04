#lang racket/base

;;; matchbox-harness.rkt — the second FML corpus: the maps the matchbox engine (ahdis) tests itself
;;; with, vendored under tests/vendor/matchbox (see its SOURCE.txt). Three kinds of check:
;;;
;;;   parse     every .map in the corpus parses to a StructureMap (top level + the tutorial's)
;;;   tutorial  the FHIR mapping-language TUTORIAL (step1 .. step13): each step's logical models
;;;             (logical/*.json) are the data source the map's `uses` resolve in; each
;;;             result/<map>.<source>.json is the map run over source/<source>.json; an expected
;;;             OperationOutcome means the run must FAIL (a `check` that does not hold)
;;;   runs      the top-level maps over the sources that fit them run without an error (no
;;;             expected output in the corpus: the check is that the engine takes them)

(require racket/contract racket/file racket/list racket/string racket/path json
         (only-in fhir/r5/schemas)                       ; the tower the sources parse into
         (only-in fhir/data-source current-data-source resources->data-source)
         (only-in "../parser/parse.rkt" parse-fml)
         (only-in "../transform.rkt" run-map uses-types)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok? ok-value err-error))

;; A Tally is (tally passed total failures) — failures: (listof (list name reason))
(struct tally (passed total failures) #:transparent)

(provide (contract-out
          [struct tally ([passed exact-nonnegative-integer?] [total exact-nonnegative-integer?]
                         [failures list?])]
          ;; result files the reference's own tests do not use as expectations (with the evidence)
          [reference-overrides (listof (list/c string? (or/c 'skip 'error) string?))]
          ;; parse-corpus : dir -> tally   every .map under dir (recursively) parses
          [parse-corpus (-> path-string? tally?)]
          ;; run-tutorial : matchbox-dir -> tally   the tutorial steps' expected results
          [run-tutorial (-> path-string? tally?)]
          ;; run-smoke : matchbox-dir -> tally   the top-level maps run over their sources
          [run-smoke (-> path-string? tally?)]))

;; ---- parse ------------------------------------------------------------------
(define (map-files dir)
  (sort (for/list ([f (in-directory dir)] #:when (regexp-match? #px"\\.map$" (path->string f))) f)
        string<? #:key path->string))

(define (parse-corpus dir)
  (for/fold ([t (tally 0 0 '())]) ([f (in-list (map-files dir))])
    (define name (path->string (find-relative-path (simple-form-path dir) (simple-form-path f))))
    (define r (with-handlers ([exn:fail? (lambda (e) (format "error: ~a" (exn-message e)))])
                (parse-fml (file->string f))))
    (cond [(and (not (string? r)) (ok? r)) (tally (add1 (tally-passed t)) (add1 (tally-total t)) (tally-failures t))]
          [else (tally (tally-passed t) (add1 (tally-total t))
                       (append (tally-failures t) (list (list name (if (string? r) r (format "parse: ~a" (err-error r)))))))])))

;; ---- tutorial ---------------------------------------------------------------
(define (read-json-file f) (call-with-input-file f read-json))

;; run-one : map-file source-file -> jsexpr | (list 'error message)   inside the step's data source
(define (run-one map-file source-file)
  (with-handlers ([exn:fail? (lambda (e) (list 'error (exn-message e)))])
    (define mr (parse-fml (file->string map-file)))
    (unless (ok? mr) (error 'parse "~a" (err-error mr)))
    (define m (ok-value mr))
    (uses-types m)                                   ; registers the step's logical models
    (define sr (from-json (read-json-file source-file) #:lenient? #t))
    (unless (ok? sr) (error 'source "~a does not parse as its type" source-file))
    (run-map m (ok-value sr))))

(define (expected-error? j) (and (hash? j) (equal? (hash-ref j 'resourceType #f) "OperationOutcome")))

;; Two result files are not expectations of the reference engine. The evidence is matchbox's own
;; test class (matchbox-engine/src/test/java/ch/ahdis/matchbox/engine/tests/FhirMappingLanguageTests.java
;; at the vendored commit):
;;   step4b.map is never run there — the map's own comment says `a.isInteger()` "will not work, isInteger
;;   is not a FHIRPath function"; the result file is a leftover, so the case is skipped;
;;   step6b over source6b: the test expects the transform to THROW (`only_one` over two elements —
;;   "Rule rule_a23b: Check condition failed: the collection has more than one item"), so the run must
;;   fail here too, whatever the stale result file says.
(define reference-overrides
  '(("step4/step4b.source4.json" skip
     "the reference never runs step4b.map; its comment says isInteger() is not a FHIRPath function")
    ("step6/step6b.source6b.json" error
     "the reference test expects an exception: only_one over two elements")))
(define (override-of name) (let ([o (assoc name reference-overrides)]) (and o (cadr o))))

(define (run-tutorial dir)
  (define steps (sort (for/list ([d (directory-list (build-path dir "tutorial") #:build? #t)]
                                 #:when (directory-exists? (build-path d "result")))
                        d)
                      < #:key (lambda (d) (string->number (regexp-replace #rx"^step" (path->string (file-name-from-path d)) "")))))
  (for*/fold ([t (tally 0 0 '())])
             ([step (in-list steps)]
              [result (in-list (sort (directory-list (build-path step "result")) string<? #:key path->string))])
    (define step-name (path->string (file-name-from-path step)))
    (define sds (for/list ([f (directory-list (build-path step "logical") #:build? #t)]
                           #:when (regexp-match? #px"\\.json$" (path->string f)))
                  (read-json-file f)))
    (define parts (regexp-match #px"^(.*)\\.(.*)\\.json$" (path->string result)))
    (define map-file (build-path step "map" (string-append (cadr parts) ".map")))
    (define source-file (build-path step "source" (string-append (caddr parts) ".json")))
    (define name (format "~a/~a" step-name (path->string result)))
    (define expected (let ([j (read-json-file (build-path step "result" result))])
                       (if (eq? (override-of name) 'error)
                           (hasheq 'resourceType "OperationOutcome" 'issue (list (hasheq 'diagnostics (caddr (assoc name reference-overrides)))))
                           j)))
    (define actual (if (eq? (override-of name) 'skip)
                       'skipped
                       (parameterize ([current-data-source (resources->data-source sds)])
                         (run-one map-file source-file))))
    (define outcome
      (cond [(eq? actual 'skipped) 'skipped]
            [(expected-error? expected)
             (if (and (pair? actual) (eq? (car actual) 'error)) 'pass
                 (format "expected an error (~a), got ~v"
                         (hash-ref (car (hash-ref expected 'issue)) 'diagnostics "") actual))]
            [(and (pair? actual) (eq? (car actual) 'error)) (cadr actual)]
            [(jsexpr=? actual expected) 'pass]
            [else (format "mismatch\n    expected ~v\n    actual   ~v" expected actual)]))
    (case outcome
      [(skipped) t]
      [(pass) (tally (add1 (tally-passed t)) (add1 (tally-total t)) (tally-failures t))]
      [else (tally (tally-passed t) (add1 (tally-total t)) (append (tally-failures t) (list (list name outcome))))])))

(define (jsexpr=? a b)
  (cond [(and (hash? a) (hash? b))
         (and (= (hash-count a) (hash-count b))
              (for/and ([(k v) (in-hash a)]) (and (hash-has-key? b k) (jsexpr=? v (hash-ref b k)))))]
        [(and (list? a) (list? b)) (and (= (length a) (length b)) (andmap jsexpr=? a b))]
        [(and (number? a) (number? b)) (= a b)]
        [else (equal? a b)]))

;; ---- the top-level maps -------------------------------------------------------
;; each map with the corpus source its `uses … as source` fits (the Java tests pair them the same way)
(define smoke-pairs
  '(("bundle-resolve.map" "bundle-resolve.src.json")
    ("bundleobs.map" "copyaliasingbug-source.json")
    ("bundlets.map" "qr.json")
    ("cast.map" "qr.json")
    ("conformsto.map" "pat.json")
    ("conformstoneg.map" "pat.json")
    ("copyaliasingbug.map" "copyaliasingbug-source.json")
    ("memberof.map" "pat.json")
    ("narrative.map" "pat.json")
    ("qr2patfordates.map" "qr.json")
    ("qr2patgender.map" "qr.json")
    ("qr2patgender_pin.map" "qr.json")
    ("quantity.map" "qr.json")
    ("stringtocoding.map" "qr.json")
    ("stringtocodingwithcc.map" "qr.json")
    ("translatecoding.map" "qr.json")
    ("whereclause.map" "capabilitystatement-example.json")))

(define (run-smoke dir)
  (for/fold ([t (tally 0 0 '())]) ([p (in-list smoke-pairs)])
    (define actual (parameterize ([current-data-source (resources->data-source '())])
                     (run-one (build-path dir (car p)) (build-path dir (cadr p)))))
    (if (and (pair? actual) (eq? (car actual) 'error))
        (tally (tally-passed t) (add1 (tally-total t)) (append (tally-failures t) (list (list (car p) (cadr actual)))))
        (tally (add1 (tally-passed t)) (add1 (tally-total t)) (tally-failures t)))))

(module+ test
  (require rackunit racket/runtime-path)
  (define-runtime-path vendor "vendor/matchbox")
  (define p (parse-corpus vendor))
  (check-equal? (tally-total p) (length (map-files vendor)))
  (check-true (> (tally-total p) 40))
  ;; step1 is the simplest map there is: one rule, one string copied
  (define t (run-tutorial vendor))
  (check-false (assoc "step1/step1.source1.json" (tally-failures t)) "tutorial step 1")
  ;; every case the reference asserts agrees (the overrides above are the reference's own)
  (check-equal? (tally-failures t) '())
  (check-equal? (tally-total t) 37)
  (check-true (>= (tally-passed (run-smoke vendor)) 17)))
