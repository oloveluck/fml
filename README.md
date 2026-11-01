# fml — FHIR Mapping Language for Racket

The FHIR Mapping Language (hl7.org/fhir/R5/mapping-language.html): a brag port of the official `mapping.g4` with the FHIRPath grammar inlined (embedded expressions are real FHIRPath ASTs), StructureMap emission, a transform engine, and `#lang fml`. fhir-test-cases structure-mapping: 6/6 executable JSON tests, 15/15 parses.

Part of **fhir — FHIR as a family of Racket `#lang`s** ([oloveluck/fhir](https://github.com/oloveluck/fhir)).
Collection `fml`. `#lang fml` — a `.map` file is a module; `racket file` prints the compiled JSON.

## Install

```sh
raco pkg install fml          # once published on pkgs.racket-lang.org
# from source, with the family repos side by side:
raco pkg install --link --auto ../fhir/fhir .
```

Depends on `fhir`, `fhirpath`, `cql` (for its error struct only — being removed).

## Use

```racket
(require fml/parser/parse fml/structuremap fml/transform)   ; parse-fml, fml->structuremap, run-map
```

```sh
raco fml map.map                          # StructureMap JSON
raco fml map.map --transform source.json  # run the map over an R5 resource
raco fml --tck
```

## Develop

```sh
raco test .          # unit tests (the conformance runners are explicit: see the --tck commands above)
```

Architecture notes: `CLAUDE.md`. Conventions, design style and the family-wide conformance table: the
main repo's `CLAUDE.md`. License: MIT (vendored test corpora keep their own terms — see `tests/vendor`).
