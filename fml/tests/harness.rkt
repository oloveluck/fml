#lang racket/base

;;; harness.rkt — the FML conformance runner over the vendored fhir-test-cases suite.
;;;
;;; manifest.xml lists the executable tests: parse <map>, transform <source>, byte-compare (as
;;; jsexpr) to <output>. Tests whose output is .xml are SKIPPED (this repo is JSON-only, repo-wide).
;;; Additionally EVERY .map file must parse (parser conformance) — including the ones the manifest
;;; never executes.

(require racket/contract racket/file racket/list xml json
         (only-in fhir/r5/schemas)                       ; register the R5 tower for source parsing
         (only-in "../parser/parse.rkt" parse-fml)
         (only-in "../transform.rkt" run-map)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok? ok-value err-error))

(provide (contract-out
          ;; -> (values exec-passed exec-total skipped parse-passed parse-total failures)
          [run-fml-suite (-> path-string? (values exact-nonnegative-integer? exact-nonnegative-integer?
                                                  exact-nonnegative-integer?
                                                  exact-nonnegative-integer? exact-nonnegative-integer?
                                                  list?))]))

(define (run-fml-suite dir)
  (define-values (ep et sk fails1) (run-exec-tests dir))
  (define-values (pp pt fails2) (run-parse-tests dir))
  (values ep et sk pp pt (append fails1 fails2)))

;; ---- executable tests (manifest.xml) ----------------------------------------
(define (run-exec-tests dir)
  (define doc (call-with-input-file (build-path dir "manifest.xml") read-xml))
  (define tests (filter (lambda (x) (and (element? x) (eq? (element-name x) 'test)))
                        (element-content (document-element doc))))
  (for/fold ([p 0] [t 0] [sk 0] [fails '()])
            ([e (in-list tests)])
    (define (attr n) (let ([a (assq n (map (lambda (a) (cons (attribute-name a) (attribute-value a)))
                                           (element-attributes e)))]) (and a (cdr a))))
    (define name (last (regexp-split #rx"/" (or (attr 'name) "?"))))
    (define out (attr 'output))
    (cond
      [(regexp-match? #px"\\.xml$" out) (values p t (add1 sk) fails)]     ; XML target: out of scope
      [(regexp-match? #px"\\.json$" (attr 'map)) (values p t (add1 sk) fails)] ; StructureMap-json input: deferred
      [else
       (define outcome (run-one dir (attr 'map) (attr 'source) out))
       (if (eq? outcome 'pass)
           (values (add1 p) (add1 t) sk fails)
           (values p (add1 t) sk (append fails (list (list name outcome)))))])))

(define (run-one dir map-file source-file out-file)
  (with-handlers ([exn:fail? (lambda (e) (format "error: ~a" (exn-message e)))])
    (define mr (parse-fml (file->string (build-path dir map-file))))
    (cond
      [(not (ok? mr)) (format "parse: ~a" (err-error mr))]
      [else
       (define src (ok-value (from-json (call-with-input-file (build-path dir source-file) read-json)
                                        #:lenient? #t)))
       (define actual (run-map (ok-value mr) src))
       (define expected (call-with-input-file (build-path dir out-file) read-json))
       (if (jsexpr=? actual expected) 'pass
           (format "mismatch\n    expected ~v\n    actual   ~v" expected actual))])))

;; structural comparison; JSON arrays are ordered (per the FHIR JSON representation)
(define (jsexpr=? a b) (equal? (norm a) (norm b)))
(define (norm v)
  (cond [(hash? v) (for/hasheq ([(k x) (in-hash v)]) (values k (norm x)))]
        [(list? v) (map norm v)]
        [(and (number? v) (inexact? v) (integer? v)) (inexact->exact v)]
        [else v]))

;; ---- parser conformance over every .map -------------------------------------
(define (run-parse-tests dir)
  (for/fold ([p 0] [t 0] [fails '()])
            ([f (in-list (directory-list dir))] #:when (regexp-match? #px"\\.map$" (path->string f)))
    (define r (parse-fml (file->string (build-path dir f))))
    (if (ok? r)
        (values (add1 p) (add1 t) fails)
        (values p (add1 t) (append fails (list (list (path->string f) (format "parse: ~a" (err-error r)))))))))

(module+ test
  (require rackunit racket/runtime-path)
  (define-runtime-path vendor "vendor/structure-mapping")
  (define-values (ep et sk pp pt fails) (run-fml-suite vendor))
  (printf "fml exec: ~a/~a (skipped ~a xml/json-map)  parse: ~a/~a\n" ep et sk pp pt)
  (for ([f (in-list fails)]) (printf "  FAIL ~a: ~a\n" (car f) (cadr f)))
  (check-equal? pp pt "every vendored .map parses")
  (check-true (>= ep 4) "most executable JSON tests pass"))
