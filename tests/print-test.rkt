#lang racket/base

;;; print-test.rkt — print.rkt held to the corpora: every map of fhir-test-cases and matchbox, and
;;; every StructureMap resource matchbox ships, written as the specified language (the laws are
;;; matchbox-harness.rkt's `printed-outcome`: the text parses, means the map, is written the same
;;; again, and has nothing the specified language lacks but what the map itself holds); and the
;;; command line's `--decompile`.

(require rackunit racket/runtime-path racket/port racket/file json
         (only-in "matchbox-harness.rkt" print-corpus print-structuremaps printed-outcome tally-total tally-failures)
         (only-in fhir/spec-package spec-package-path)
         (only-in fhir/package read-package)
         (only-in "../cli.rkt" decompile-file)
         (only-in "../parser/parse.rkt" parse-fml specification-notes)
         (only-in "../structuremap.rkt" fml->structuremap structuremap->fml)
         (only-in fhir/result/result ok-value))
(define-runtime-path structure-mapping "vendor/structure-mapping")
(define-runtime-path matchbox "vendor/matchbox")

(define (all-pass t) (check-equal? (tally-failures t) '()) (tally-total t))

;; HL7's own StructureMap resources — the examples of the R4 and R5 core packages and of CRMI,
;; when fhir-spec is installed — hold the same laws (supplyrequest-transform did not: a constant
;; written as an `evaluate` of a quoted string came back as a `copy`, and its `(now())` as a
;; transform named `now`)
(for ([package '("hl7.fhir.r4.core.tgz" "hl7.fhir.r5.core.tgz" "hl7.fhir.uv.crmi-2.0.0.tgz")])
  (define tgz (spec-package-path package #:optional? #t))
  (when tgz
    (define-values (_manifest resources examples)
      (read-package tgz #:select (lambda (name) (regexp-match? #rx"StructureMap-[^/]*[.]json$" name))))
    (define maps (for/list ([e (in-list (append resources examples))]
                            #:when (and (hash? (cdr e)) (equal? (hash-ref (cdr e) 'resourceType #f) "StructureMap")))
                   e))
    (check-true (pair? maps) package)
    (for ([e (in-list maps)])
      (check-equal? (printed-outcome (structuremap->fml (cdr e))) 'pass (format "~a ~a" package (car e))))))
(check-equal? (all-pass (print-corpus structure-mapping)) 15)
(check-equal? (all-pass (print-corpus matchbox)) 43)
(check-equal? (all-pass (print-structuremaps matchbox)) 25)

;; raco fml --decompile: a StructureMap resource as text — here the tutorial's first step, as the
;; reference emitted it (an R4 resource): the specified language and nothing else, the same map
(define step1 (build-path matchbox "tutorial" "step1" "map" "step1.json"))
(define text (with-output-to-string (lambda () (decompile-file step1))))
(check-regexp-match #rx"^/// url = 'http://hl7.org/fhir/StructureMap/tutorial-step1'\n/// name = 'tutorial-step1'\n" text)
(check-regexp-match #rx"\nuses \"http://hl7.org/fhir/StructureDefinition/tutorial-left-1\" alias TLeft as source\n" text)
(check-regexp-match #rx"\ngroup tutorial[(]source src : TLeft, target tgt : TRight[)] [{]\n  src.a as a -> tgt.a = a \"rule_a\";\n[}]\n$" text)
(define read-back (ok-value (parse-fml text)))
(check-equal? (specification-notes read-back) '())
(check-equal? (fml->structuremap read-back)
              (fml->structuremap (structuremap->fml (call-with-input-file step1 read-json))))
