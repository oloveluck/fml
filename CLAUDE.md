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
compare to the expected results (**37/37**; an expected OperationOutcome means the run must fail); the
top-level maps run over their sources (17/17; the Java tests assert single values, so no expected
outputs). Two result files are NOT expectations of the reference, and the harness says so with the
evidence from matchbox's test class (`reference-overrides`): step4b.map is never run there (its own
comment: `isInteger()` is not a FHIRPath function — the where is false here), and step6b over source6b
is expected to THROW (`only_one` over two elements — an error here too; the result file is stale).
Lesson: a corpus's result files are not all assertions — read the reference's test before calling a
difference a disagreement. CI also parses the FHIR R3/R4 inter-version maps (FHIR/interversion, 332
maps, no license so not vendored): 332/332.
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

**The StructureMap as the interchange form; maps as values; check** (the family's ELM analogy made
real). `structuremap.rkt` goes both ways: `structuremap->fml` reads a StructureMap resource into the
AST the engine runs (FHIRPath strings parsed and memoized, typed literal parameters, R4 `equivalence`
and `dependent.variable` understood) — the law `structuremap->fml ∘ fml->structuremap = id` holds on
every corpus map (58/58) up to the FML-only surface (`///` metadata, conceptmap prefixes, `rule-N`
names; `copy(v)` lowers as `v`; a numeric/boolean literal is a copy of a typed value as the reference
writes it; target list modes first/last/single are kept). `transform.rkt`: `register-structure-map!`,
`resolve-structure-map` (registered maps — a required `#lang fml` module registers its own — then
StructureMaps in `current-data-source`, then the packages: `sd-index` now indexes StructureMap and
ConceptMap), `map-imports`; a group is found in the map or any map it imports, transitively.
`registry.rkt`: `(structure-map ref)` — canonical, AST or resource — is a typed procedure (`smap-source`/
`smap-target` from the entry group's parameters and `uses`), `compose-maps` checks the types;
`$transform` takes `source` (canonical) and `sourceMap` (resource) besides `srcMap`. Oracles: the
tutorial run from OUR emitted StructureMaps by canonical (37/37); from the REFERENCE's `map/*.json`
(30/37 — several of those resources predate their .map: stale conditions, a missing rule; named,
floor 30); our StructureMap vs the reference's rule by rule, names/documentation/R4-isms normalised
(21/31; the rest are stale conditions and the reference's synthetic variables).
`check.rkt` (`raco fml check`, `check-map`, `check-map/examples`): rules, sources, targets and groups
remember their positions (parse.rkt keeps brag's syntax, `syntax->datum/locs`, `attach-loc!`); the
check reports what the two StructureDefinitions decide — an element the type lacks (choice bases
through the schema's `choice-base`, a `raw` leaf is a string), a value whose type cannot go into the
element, a group call's arity or an unknown group, a required target element never assigned (warning,
unless a dependent call receives the whole target) — fail-open on anything unknown; with examples,
each result is validated (fhir-validation on demand) and an issue traced to the rule that wrote the
element (`target-paths`/`trace-target`). The corpora check clean but for three REAL findings (55/58):
`contributor` and `Composition.code` are R4 elements R5 dropped, `identifer` is a typo in syntax.map.
`project.rkt` is what fsh loads on demand (`parse-map-file`, `map-structuremap`, `check-project-map`)
so a project of FSH structures holds its maps; the worked project is fhir-examples/examples/intake-maps.
Quality pass: `schema.rkt` is the ONE reading of the registry's schema for the engine and the checker
(a Ty: type symbol | `(prim code)` | #f; `element-of type elem [forced]` -> `(element key list? ty)` or
#f, choice bases through the schema's `choice-base`, a type without a schema open; `path-type`); the
transform catalogue is DATA — `transforms` maps a name to `(transform run result-type)` via
`define-transform`/`define-constructor`, the result type (what picks a choice target's key) a name or a
procedure of the args, and an unknown transform is reported with the names there are.
A `#lang fml` module registers its StructureMap with `fhir/registry` (the module system as the FHIR
registry), and `resolve-structure-map` looks there after the data source; a rule that fails at run time
(`rule-error`: check, only_one, cast, an unknown group, an unbound variable) raises `exn:fail:diagnostic`
positioned at the rule, which `$transform` returns as an OperationOutcome carrying the line.

**Editor and agent support** (`support.rkt`, through `fhir/lang/support`): diagnostics (parse, then
`check-map`), the maps beside a document as its project, the declarations as an outline (the map with
its url as id, uses aliases, imports, concept maps, groups), hover, go to definition (an `imports`
url leads to the map that declares it), completion of declared names and keywords; `$transform` is
already an operation, so no tool. The LSP and MCP hubs load it by collection name.

**Workspace support** (`workspace.rkt`): artifacts = each map's StructureMap with `check-map`'s
findings; `run` = every data-source resource of the map's source type (or `over`) transformed and
validated by $validate when fhir-validation is installed; `explain` = one source's result and
`check-map/examples`'s problems traced to rules, against the workspace's profiles.
