# CLAUDE.md — fml

Conventions, HTDP design style, and the conformance ratchet table live in the main `fhir` repo's
CLAUDE.md (`~/Projects/fhir`); this file holds only the notes for this package. This repo root IS the
`fml` collection (`(define collection "fml")`): modules are `fml/compile` etc., and imports of the
foundation use collection paths (`fhir/model/node`, never a relative path into another package).
The CLI is `cli.rkt` (`raco fml`); the conformance runner(s) live in `tests/` behind `--tck`.

## FML / StructureMap lang (`#lang fml`, `fml/`)
The FHIR Mapping Language as a lang: `fml/parser/grammar.rkt` is a `#lang brag` port of the
**official `hl7.org/fhir/R5/mapping.g4`** (vendored alongside as `fml/parser/mapping.g4`). Where the
g4 stubs FHIRPath ("insert reference to FhirPath grammar here"), the repo's FHIRPath cascade is
INLINED with the same rule names — so `fhirpath/parser/ast.rkt`'s `lower` turns embedded expressions
into REAL FHIRPath ASTs at parse time (`fml-fhirpath` carries the AST; nothing re-parses at
runtime). Documented g4 deviations follow the reference implementation + the vendored suite: bare
(unparenthesized) `where`/`check` expressions, `share <listRuleId>`, string rule-names,
double-quoted strings inside expressions, and the fp `is`/`as` OPERATOR level omitted (`create('X')
as var` must read as a target alias; the `.is()`/`.as()` function forms remain).
`fml/fhirpath-print.rkt` prints ASTs back to canonical text (print∘parse fixpoint-tested) for the
StructureMap emission. Two consumers: `fml/structuremap.rkt` (AST → the StructureMap resource
jsexpr — the canonical compiled form, printed by `#lang fml`'s `main`) and `fml/transform.rkt`
(the executor): a rule matches its source element(s) (where-filtered FHIRPath over typed nodes),
binds `as` vars, then runs comma-chained target ops / `then` dependent group calls / nested rules. Targets build through TSLOTs — closures that mutate the jsexpr under
construction up to the root — whose shape is **registry-driven** (`full-field-specs`: max>1 appends
arrays, max-1 sets scalars, and a `create('T')` on a `value[x]` element picks the `value<T>` key).
`share v` reuses one target element across rules. FML's FHIRPath dialect bridges: rule variables
become `%externals` via a chain-head AST rewrite, and element-model `.value` steps retry stripped.
Conformance (`raco fhir fml-tck` → `fml/tests/run.rkt`): the vendored
`fhir-test-cases/r5/structure-mapping` suite — **6/6 executable JSON tests, 15/15 `.map` parses**;
the 4 CDA/XML-output tests + the one StructureMap-as-JSON input are skipped (XML is out of scope
repo-wide, documented in the vendor SOURCE.txt).

`operations.rkt`: StructureMap/$transform as an operation (fhir/operation) — `(apply-operation
"StructureMap/$transform" content #:srcMap fml-text)`. It executes FML (the R5 definition's `srcMap`);
a StructureMap given only as `source`/`sourceMap` JSON is refused with that explanation.
