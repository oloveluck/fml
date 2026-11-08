# fml — FHIR Mapping Language

The [FHIR Mapping Language](https://hl7.org/fhir/R5/mapping-language.html): a port of the official
`mapping.g4` with the FHIRPath grammar inlined, so embedded expressions are real FHIRPath; StructureMap
emission and reading; a transform engine over FHIR resources and logical models; and a checker that
positions each finding in the map. Measured on four corpora:

| Corpus | Result |
|---|---|
| fhir-test-cases structure-mapping | 6/6 executable tests, 15/15 maps parse |
| the matchbox engine's maps | 43/43 parse, 17/17 run |
| the FHIR mapping-language tutorial | 37/37 results, and 37/37 again run from the StructureMaps emitted here |
| every corpus map through its StructureMap and back | 58/58 |
| the FHIR R3/R4 inter-version maps (CI, not vendored) | 332/332 parse |

Part of the [fhir](https://github.com/oloveluck/fhir) family of FHIR languages and tools.

## Command line

`fml` is one of the family's command-line tools, which run without Racket:
[build them once](https://github.com/oloveluck/fhir#getting-the-tools) and put `dist/bin` on your
`PATH`. With Racket and the packages installed, `raco fml` runs the same commands.

```sh
fml qr2pat.map                                      # the StructureMap, as JSON
fml qr2pat.map --transform qr.json                  # run the map over an R5 resource
fml map.map --transform source.json --structures sds/   # over logical models, given their StructureDefinitions
fml check map.map [--structures sds/] [--examples ex/] [--profile sd.json]   # findings on their lines
```

With `qr2pat.map`, which reads a QuestionnaireResponse answer into a Patient:

```
map "http://github.com/hapifhir/org.hl7.fhir.core/org.hl7.fhir.r4.tests/qr2patgender" = "qr2patgender"

uses "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse" alias QuestionnaireResponse as source
uses "http://hl7.org/fhir/StructureDefinition/Patient" alias Patient as target

group QuestionnaireResponse(source src : QuestionnaireResponse, target tgt : Patient) {
  src.item as item -> tgt as patient then item(item, patient);
}

group item(source src, target tgt: Patient) {
  src.item as item where linkId.value in ('patient.sex') -> tgt.gender = (item.answer.valueString);
}
```

```
$ fml qr2pat.map --transform qr.json
{"gender":"female","resourceType":"Patient"}
```

`fml check` holds a map to its structures: an element a type lacks, a value that cannot go into an
element, a group called with the wrong arity, a required target never assigned. With `--examples` it
also transforms the examples and validates the results. Errors are positioned, with exit status 1:

```
$ fml broken.map
broken.map:4:1: fml parse error: unexpected `}`
 4 | }
   | ^
```

## In Racket

`#lang fml` makes a map a module. Requiring it compiles the map and registers its StructureMap by
canonical, so `$transform` and other maps find it:

```racket
#lang racket/base
(require json "qr2pat.map")       ; qr2pat.map starts with `#lang fml`

(transform (call-with-input-file "qr.json" read-json))   ; '#hash((gender . "female") (resourceType . "Patient"))
```

The module provides `fml` (the map's syntax tree), `structuremap` (the resource) and `(transform
source #:structures sds)`. `racket qr2pat.map` prints the StructureMap.

A map is also a value: `(structure-map "http://…/StructureMap/x")` (`fml/registry`) resolves one by
canonical, from the registered maps, the data source or the packages, into a procedure from its source
type to its target type, and `compose-maps` joins two whose types meet. `StructureMap/$transform` is an
operation: `(apply-operation "StructureMap/$transform" qr #:srcMap fml-text)`. Libraries: `run-map`
(`fml/transform`), `check-map` (`fml/check`), `structuremap->fml` (`fml/structuremap`). A project of FSH
structures checks its `.map` files with `fsh check`.

## Develop

From source, with the family's repositories side by side:

```sh
raco pkg install --link --auto ../fhir/fhir ../fhir/fhir-spec ../fhirpath ../vcl ../fhir/fhir-tx .
raco test .                       # unit tests
raco fml --tck                    # the corpora, with floors
raco fml --parse-corpus dir/      # parse every .map under a directory
```

Reference documentation: [oloveluck.github.io/fml](https://oloveluck.github.io/fml/). Architecture
notes: `CLAUDE.md`; the family's conventions and conformance table are in the fhir repository's
`CLAUDE.md`. License: MIT (vendored test corpora keep their own terms, see `tests/vendor`).
