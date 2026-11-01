#lang racket/base

;;; run.rkt — the `raco fhir fml-tck` entry point over the vendored fhir-test-cases suite.

(require racket/runtime-path (only-in "harness.rkt" run-fml-suite))
(define-runtime-path vendor "vendor/structure-mapping")

(module+ main
  (define-values (ep et sk pp pt fails) (run-fml-suite vendor))
  (for ([f (in-list fails)]) (printf "  FAIL ~a: ~a\n" (car f) (cadr f)))
  (printf "FML conformance: exec ~a/~a (skipped ~a xml-target/json-map)  parse ~a/~a\n" ep et sk pp pt)
  ;; floors: every runnable JSON test and every .map must stay green
  (unless (and (>= ep 6) (= pp pt))
    (eprintf "FML conformance regressed\n") (exit 1)))
