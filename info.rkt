#lang info

(define collection "fml")
(define version "0.3")
(define pkg-desc "FHIR Mapping Language for Racket: brag port of the official mapping grammar with real FHIRPath ASTs, StructureMap emission, a transform engine, #lang fml and raco fml")
(define pkg-authors '(oloveluck))
(define license 'MIT)
(define deps '("base" "fhir" "fhirpath" "cql" "brag-lib" "br-parser-tools-lib"))
(define build-deps '("rackunit-lib"))
(define raco-commands '(("fml" (submod fml/cli main) "compile FML to a StructureMap; transform a resource; run the structure-mapping suite" #f)))
(define test-omit-paths '("tests/vendor"))
