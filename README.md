# fhir-fml

FHIR Mapping Language for Racket: brag port of the official mapping grammar with real FHIRPath ASTs, StructureMap emission, a transform engine and #lang fhir/fml.

Part of **fhir — FHIR as a family of Racket `#lang`s** (main repo: `oloveluck/fhir`). This package
contributes `fml/` to the shared `fhir` collection. `#lang fhir/fml`.

## Depends on

`fhir-core`, `fhir-fhirpath`, `fhir-cql`
(Racket packages; link them first) plus `brag-lib`, `br-parser-tools-lib`.

## Develop

```sh
raco pkg install --link --auto .     # link into the `fhir` collection
raco test fml/
```

The `raco fhir` CLI and the conformance-suite runners are driven from the main `fhir` package:

```
raco fhir fml-tck   (fhir-test-cases structure-mapping suite)
```

Architecture notes: `CLAUDE.md` here; conventions and the ratchet table: the main repo's `CLAUDE.md`.
