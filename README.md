# fml — FHIR Mapping Language for Racket

The FHIR Mapping Language (hl7.org/fhir/R5/mapping-language.html): a brag port of the official `mapping.g4` with the FHIRPath grammar inlined (embedded expressions are real FHIRPath ASTs), StructureMap emission, a transform engine over FHIR resources and logical models, and `#lang fml`.

Corpora (`raco fml --tck`): fhir-test-cases structure-mapping 6/6 executable JSON tests, 15/15 parses; the matchbox engine's maps 43/43 parse, 17/17 run; the FHIR mapping-language tutorial 37/37 of the results its reference asserts (two stale result files are set aside with the evidence in `tests/matchbox-harness.rkt`), and 37/37 again run from the StructureMap resources we emit; every corpus map round-trips through its StructureMap (58/58); `check` reports nothing on the maps that fit their structures (55/58, the three findings real); the FHIR R3/R4 inter-version maps 332/332 parse (CI, not vendored).

A map is a value: `(structure-map "http://…/StructureMap/x")` resolves a StructureMap by canonical (registered maps, the data source, the packages) into a typed procedure; `raco fml check map.map` checks a map against its structures and positions each finding in the FML; a project of FSH structures checks its `.map` files the same way under `raco fsh check`.

Part of **fhir — FHIR as a family of Racket `#lang`s** ([oloveluck/fhir](https://github.com/oloveluck/fhir)).
Collection `fml`. `#lang fml` — a `.map` file is a module; `racket file` prints the compiled JSON.
Reference: [oloveluck.github.io/fml](https://oloveluck.github.io/fml/).

## Install

```sh
raco pkg install fml          # once published on pkgs.racket-lang.org
# from source, with the family repos side by side:
raco pkg install --link --auto ../fhir/fhir .
```

Depends on `fhir`, `fhir-tx` (`translate` over the terminology service) and `fhirpath`.

## Use

```racket
(require fml/parser/parse fml/structuremap fml/transform)   ; parse-fml, fml->structuremap, run-map
```

```sh
raco fml map.map                          # StructureMap JSON
raco fml map.map --transform source.json  # run the map over an R5 resource
raco fml map.map --transform source.json --structures sds/   # with the logical models' StructureDefinitions
raco fml --parse-corpus dir/              # parse every .map under a directory
raco fml --tck                            # the corpora, with floors
```

In a program, `(transform src #:structures sds)` from a `#lang fml` module; `run-map` and `uses-types` from `fml/transform`; `structure-map`, `smap-source`/`smap-target` and `compose-maps` from `fml/registry`; `check-map` from `fml/check`; `structuremap->fml` from `fml/structuremap`.

```sh
raco fml check map.map [--structures sds/] [--examples ex/] [--profile sd.json]   # findings on their FML lines
```

## Develop

```sh
raco test .          # unit tests (the conformance runners are explicit: see the --tck commands above)
```

Architecture notes: `CLAUDE.md`. Conventions, design style and the family-wide conformance table: the
main repo's `CLAUDE.md`. License: MIT (vendored test corpora keep their own terms — see `tests/vendor`).
