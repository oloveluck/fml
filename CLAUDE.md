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

Transforms beyond the TCK, added for real maps (fhir-examples' screening/intake.fml): the spec's datatype
constructors `c`, `cc`, `qty`, `id`, `cp`, `append` (a `constructors` table in transform.rkt); a choice
SOURCE reads whichever variant is present (`ans.value` -> `valueDecimal`); a choice TARGET takes its key
from the assigned value's type — a primitive variable's own type, a constructor's (`qty` -> Quantity) or
`create('T')` (`tgt.effective = a` -> `effectiveDateTime`); primitives render as their JSON form.
`#lang fml`'s `transform` takes FHIR JSON (or a typed node) and answers FHIR JSON. TCK unchanged: 6/6 +
15/15.

**Second corpus: matchbox + the FML tutorial** (`tests/vendor/matchbox`, ahdis/matchbox's
mapping-language test resources, Apache-2.0; `tests/matchbox-harness.rkt`; `raco fml --tck` runs both
corpora with floors in `tests/run.rkt`): every map parses (43/43); the tutorial's 13 steps run over their
logical models — each step's StructureDefinitions are the data source the map's `uses` resolve in — and
compare to the expected results (**36/38**; an expected OperationOutcome means the run must fail); the
top-level maps run over their sources (17/17; the Java tests assert single values, so no expected
outputs). The two tutorial disagreements are NAMED in the harness (`tutorial-disagreements`): `only_one`
over two elements is an error per spec (the reference keeps the last), and `where a.isInteger()` is false
here (not a FHIRPath function; the reference copies anyway). CI also parses the FHIR R3/R4 inter-version
maps (FHIR/interversion, 332 maps, no license so not vendored): 332/332.
What the corpus drove in the engine (transform.rkt): types from `uses` — a url the towers lack is resolved
as a StructureDefinition (data source, then packages) and registered through `fhir/sd/logical`
(`uses-types`; a later definition of the same type name replaces the earlier — the tutorial's TLeft per
step; tower types are never replaced); target slots carry the element's type and primitive code, so an
assigned value is COERCED ("12345" -> 12345 into an integer element); implicit copy and `types`/`type+`
group dispatch by the parameters' declared types; multi-source rules (cartesian), `check` (an error naming
the rule), `log` (`current-fml-log`), all five source list modes, `extends`; `where` sees the alias bound
to the candidate and may precede the alias; `cast` errors when the value does not convert; bare
invocation targets (`evaluate(result, resolve()) as observation`, the value becomes a source); `create`
of a resource type sets `resourceType`; `reference(var)` of a built target; `translate` over an embedded
`conceptmap` block (parsed into `fml-conceptmap`, emitted as a contained ConceptMap) or via
`fhir/terminology/service`'s `translate/json` through `current-terminology-resolver`; `uuid`. Grammar:
`conceptmap` anywhere among the declarations, `unmapped for s = provided`, the FHIRPath `is` operator
(only `as` stays out — it is the target alias), the mode words as identifiers, a map with no groups (a
conceptmap library). `raco fml <map> --transform <src> --structures <file|dir>`, `--parse-corpus <dir>`;
`#lang fml`'s `transform` takes `#:structures`.
