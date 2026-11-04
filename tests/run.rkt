#lang racket/base

;;; run.rkt — the `raco fml --tck` entry point: both vendored corpora, each with a floor that only
;;; rises (fhir-test-cases structure-mapping; the matchbox maps and the FML tutorial).

(require racket/runtime-path
         (only-in "harness.rkt" run-fml-suite)
         (only-in "matchbox-harness.rkt" parse-corpus run-tutorial run-smoke tally-passed tally-total tally-failures))
(define-runtime-path vendor "vendor/structure-mapping")
(define-runtime-path matchbox "vendor/matchbox")

(define (report label t)
  (for ([f (in-list (tally-failures t))]) (printf "  FAIL ~a: ~a\n" (car f) (cadr f)))
  (printf "~a: ~a/~a\n" label (tally-passed t) (tally-total t)))

(module+ main
  (define-values (ep et sk pp pt fails) (run-fml-suite vendor))
  (for ([f (in-list fails)]) (printf "  FAIL ~a: ~a\n" (car f) (cadr f)))
  (printf "fhir-test-cases structure-mapping: exec ~a/~a (skipped ~a xml-target/json-map)  parse ~a/~a\n" ep et sk pp pt)
  (define mp (parse-corpus matchbox))
  (report "matchbox parse" mp)
  (define mt (run-tutorial matchbox))
  (report "FML tutorial (matchbox)" mt)
  (define ms (run-smoke matchbox))
  (report "matchbox maps run" ms)
  ;; floors: every runnable JSON test and every .map stay green; the corpus floors ratchet up
  (define floors (list (list "structure-mapping exec" ep 6) (list "structure-mapping parse" pp pt)
                       (list "matchbox parse" (tally-passed mp) 43)
                       (list "tutorial" (tally-passed mt) 37)
                       (list "matchbox runs" (tally-passed ms) 17)))
  (define regressed (filter (lambda (f) (< (cadr f) (caddr f))) floors))
  (unless (null? regressed)
    (for ([f regressed]) (eprintf "FML conformance regressed: ~a ~a < floor ~a\n" (car f) (cadr f) (caddr f)))
    (exit 1)))
