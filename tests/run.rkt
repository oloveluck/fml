#lang racket/base

;;; run.rkt — the `raco fml --tck` entry point: both vendored corpora, each with a floor that only
;;; rises (fhir-test-cases structure-mapping; the matchbox maps and the FML tutorial).

(require racket/runtime-path
         (only-in "harness.rkt" run-fml-suite)
         (only-in "matchbox-harness.rkt" parse-corpus run-tutorial run-smoke roundtrip-corpus check-corpus
                  run-tutorial/structuremap compare-reference-structuremaps tally-passed tally-total tally-failures))
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
  (define rt1 (roundtrip-corpus vendor))
  (define rt2 (roundtrip-corpus matchbox))
  (report "StructureMap round trip (fhir-test-cases)" rt1)
  (report "StructureMap round trip (matchbox)" rt2)
  (define ts (run-tutorial/structuremap matchbox))
  (report "FML tutorial from the reference's StructureMaps" ts)
  (define cs (compare-reference-structuremaps matchbox))
  (report "our StructureMap vs the reference's, by rule" cs)
  (define ck1 (check-corpus vendor))
  (define ck2 (check-corpus matchbox))
  (report "check: maps without an error finding (fhir-test-cases)" ck1)
  (report "check: maps without an error finding (matchbox)" ck2)
  ;; floors: every runnable JSON test and every .map stay green; the corpus floors ratchet up
  (define floors (list (list "structure-mapping exec" ep 6) (list "structure-mapping parse" pp pt)
                       (list "matchbox parse" (tally-passed mp) 43)
                       (list "tutorial" (tally-passed mt) 37)
                       (list "matchbox runs" (tally-passed ms) 17)
                       (list "round trip" (+ (tally-passed rt1) (tally-passed rt2)) 0)
                       (list "tutorial from StructureMaps" (tally-passed ts) 0)
                       (list "StructureMap vs reference" (tally-passed cs) 0)
                       (list "check" (+ (tally-passed ck1) (tally-passed ck2)) 0)))
  (define regressed (filter (lambda (f) (< (cadr f) (caddr f))) floors))
  (unless (null? regressed)
    (for ([f regressed]) (eprintf "FML conformance regressed: ~a ~a < floor ~a\n" (car f) (cadr f) (caddr f)))
    (exit 1)))
