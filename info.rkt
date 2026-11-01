#lang info

;; fhir-fml — part of the `fhir` family of Racket packages (FHIR as a family of #langs).
;; Every package in the family declares `(define collection "fhir")`, so they splice into the
;; one `fhir` collection: module paths (fhir/...) and #lang names are the same as in the monorepo.

(define collection "fhir")
(define version "0.2")
(define pkg-desc "FHIR Mapping Language for Racket: brag port of the official mapping grammar with real FHIRPath ASTs, StructureMap emission, a transform engine and #lang fhir/fml")
(define pkg-authors '(oloveluck))
(define deps '("base" "fhir-core" "fhir-fhirpath" "fhir-cql" "brag-lib" "br-parser-tools-lib"))
(define build-deps '("rackunit-lib"))
