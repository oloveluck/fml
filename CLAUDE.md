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
runtime). What is read beyond the specified language, and what the specification's two halves disagree on, is
in "The specified language" at the end of this file. `fml/fhirpath-print.rkt` prints ASTs back to canonical text (print∘parse fixpoint-tested) for the
StructureMap emission. Two consumers: `fml/structuremap.rkt` (AST → the StructureMap resource
jsexpr — the canonical compiled form, printed by `#lang fml`'s `main`) and `fml/transform.rkt`
(the executor): a rule matches its source element(s) (where-filtered FHIRPath over typed nodes),
binds `as` vars, then runs comma-chained target ops / `then` dependent group calls / nested rules. Targets build through TSLOTs — closures that mutate the jsexpr under
construction up to the root — whose shape is **registry-driven** (`full-field-specs`: max>1 appends
arrays, max-1 sets scalars, and a `create('T')` on a `value[x]` element picks the `value<T>` key).
`share v` reuses one target element across rules. FML's FHIRPath dialect bridges: rule variables
become `%externals` via a chain-head AST rewrite, and element-model `.value` steps retry stripped.
Conformance (`raco fml --tck` → `fml/tests/run.rkt`): the vendored
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
(29/37 — several of those resources predate their .map: stale conditions, a missing rule; named,
floor 29: it was 30 until conditions were read by FHIRPath's singleton evaluation, see the pass of
2026-11-13 below); our StructureMap vs the reference's rule by rule, names/documentation/R4-isms normalised
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
url leads to the map that declares it), completion of declared names and keywords. The language
server loads it by collection name.

**Workspace support** (`workspace.rkt`): artifacts = each map's StructureMap with `check-map`'s
findings; `run` = every data-source resource of the map's source type (or `over`) transformed and
validated by $validate when fhir-validation is installed; `explain` = one source's result and
`check-map/examples`'s problems traced to rules, against the workspace's profiles. It also provides `guide` (below).

**Documentation for users and agents** (2026-11-08; the family's pattern, cql first — see the main
repo's CLAUDE.md). `guide.md` is the language for agents — cheat sheet, worked example, what the
checker reports, running it (command line, module, by canonical/operation, workspace), logical
models, common problems — provided by `fml/workspace` as `guide` (fhir-mcp's `learn://fml`).
`tests/guide-test.rkt` parses and checks every ```fml block, runs the cheat sheet and the worked
example over the sources the text describes and compares the results it states, and checks the
quoted findings (`subjct`; the unassigned `Observation.code` warning) word for word.
`.claude/skills/fml/SKILL.md` is the workflow without MCP (every command in it was run).
`scribblings/fml.scrbl` is the manual (now current: fml/workspace, the StructureMap round trip;
`smap?` was referenced but undocumented); `tests/docs-test.rkt` renders it and fails on a broken
reference to its own sections or `fml/…` bindings. Found writing them, then fixed: a map did not
check its source's type (`$transform` ran the Observation map over a QuestionnaireResponse).
`run-map` now refuses a source that is not of the entry group's declared source type or a type
derived from it (`check-source-type!`, through fhirpath/types' `fhir-ancestors`; nothing is checked
when the group declares no type the map knows), a diagnostic at the group; `$transform` returns it as
an OperationOutcome and `raco fml --transform` prints it positioned (it printed a stack trace for
any run-time rule error). Every corpus floor holds.


**Checked when it compiles** (2026-11-09): the `#lang fml` reader runs `check-map` on the parsed map
(the R5 types when no FHIR version is registered yet): an error-level finding fails `raco make` at its
rule. A new user's map writing `tgt.gendr` compiled and silently produced `{"gendr": …}`.

**Says what it could not read** (2026-11-09): a source read leniently loses what does not parse (a
date written `01/05/1950`, an R4-only element); `fhir/serialize/lenient`'s `lenient-drops` names those
values by path, and `#lang fml`'s `transform` warns (`#:strict?` raises), `raco fml --transform` warns
on stderr, a workspace run lists them as `unread`. `tests/drops-test.rkt`.

**Names the rule in a pipeline** (2026-11-10): `#lang fml`'s `(transform src #:validate spec)` holds
the result to its base type and the profiles `spec` names (`#t`, a canonical, an SD, an FSH project,
a list) and raises `exn:fail:user` with each problem at the rule that wrote the element, or `no rule of
<group> writes `x`` — `ensure-valid` after the map can only name the element. The reader now puts the
map's text and file into the module (`(fml-map-form ast text source)`): the quoted AST loses its
positions, so the module parses the text again on first validation. `check.rkt`'s `validate-result`
is the one place a result is validated and traced (check-map/examples uses it); `profiles-from`
resolves `--profile` as a JSON file, an FSH project (fsh/project on demand; its definitions
registered) or a canonical; a profile applies to results of its type. `tests/validate-test.rkt`.

**Says why nothing is there** (2026-11-10): when the rule an issue traces to exists but its element is
absent from the result, `validate-result` adds `(the rule here wrote nothing: its source matched nothing
in the input)` — a missing item, a `where` that held for none — beside `(no rule of <group> writes …)`.

**Names what it held the result to** (2026-11-10): `transform #:validate`'s error header is `the map's
Patient is invalid against IntakePatient (<url>) (FHIR R5 (5.0.0)):` — the profiles of the result's type
(else its base type) and the version validation read.

**Says what it could not read, at the rule** (2026-11-10): when the rule an issue traces to wrote nothing
and the lenient read dropped source values, `transform #:validate` says `its source matched nothing the
map could read — <path> ("…") could not be read`; problem paths are relative to the current directory.
guide.md shows a `#lang fml` file and the error Racket gives without the line.

**One error for an invalid result** (2026-11-10): `transform #:validate` raises `exn:fail:operation`
(fhir/operation; its OperationOutcome one issue per problem), as `ensure-valid` does — one handler,
`exn:fail:operation?`, catches both (a new user's documented handler caught nothing). guide.md says so.

**Prints FHIR JSON as FHIR writes it** (2026-11-10): `raco fml --transform` prints with
`fhir/serialize/fhir-json` (`resourceType` first).

**A primitive element takes a primitive** (2026-11-10): `check-primitive-target!` (transform.rkt) refuses an
object assigned to a primitive element, at the rule, naming the part that fits (`(a.value.code)`; an empty
`{}` — a value the lenient read dropped — is said elsewhere); a `#lang fml` module runs its positioned
parse (`located`), so a rule error names its line. Every corpus floor holds. `tests/primitive-target-test.rkt`.

**Names each example** (2026-11-10): `raco fml check --examples` names an example by its file in each
finding and in the summary, which says what the results were held to (`against IntakePatient (url)`,
else the base type and the profiles a result claims); `check-map/examples #:names`.

**Says which part wrote nothing** (2026-11-10): `current-empty-targets` (transform.rkt; a mutable hasheq
when set) records each target whose value expression came out empty, with the source value the rule
read; `transform #:validate` and `check-map/examples` set it, and `validate-result` says `(the rule here
wrote nothing: its source matched, but `(a.value.code)` was empty for {"valueString":"female"})` instead
of "its source matched nothing in the input". guide.md filters gender by its code system, tested.

**Prints indented on request** (2026-11-10): `raco fml <map> --transform <src> --indent`.

**Says when a `where` dropped it** (2026-11-11): `source-matches` records, for the targets of a rule (and
its nested rules) whose source matched items its `where` held for none of, the condition and the first
item; `validate-result` says `its source matched 1 item, but `where (linkId = 'gender')` held for none
(the first: …)` instead of blaming the input.

**Names what the `where` tested** (2026-11-11): `source-matches` keeps every item the source matched;
`validate-result`'s `where-evidence` lists the values the tested field held, with the nearest to the
literal (`held for none (the linkId there: name, gendr — did you mean 'gendr'?)`); the first item only
when the condition compares no field to a literal.


**Says a `where` at its own line** (2026-11-11): the `filtered` record `source-matches` keeps carries the
rule whose `where` held for none; `validate-result` positions the problem there (the fix is there), not
at the nested rule that would have written the element. `tests/validate-test.rkt`.

**Says why a date is unread** (2026-11-12): the lenient-read warning comes from `fhir/serialize/lenient`'s
`drops-message` — a date its month lacks gets the calendar's reason (`is no date: February 1990 has 28
days`), anything else the old wording. `tests/drops-test.rkt`.

**The specified language; the printer** (2026-11-13). The rule: this language is the one the
specification defines, read in full, and what is read beyond it is said.
- **The specification is two documents that disagree**: its text (mapping-language.html) and its
  formal grammar (`parser/mapping.g4`, vendored). The header is `/// url = '…'` and `/// name = '…'`
  metadata lines in the text (SHALL) and `map url = name` in the g4; a url is "double-quoted" in the
  text and `backticked` in the g4; a rule's name "double-quoted" in the text and a bare id in the
  g4; the text writes strings in either quote, the g4 single-quoted only; the metadata lines and the
  short form `src -> tgt: a, b;` are in the text alone. BOTH forms are read; `print.rkt` writes the
  text's (it is what the specification's examples and every published map are written in, and the g4
  stubs FHIRPath as `literal`). An earlier note here called quoted rule names a deviation: they are
  the text's form.
- **Forms the specification has that the engine lacked or dropped silently, now read, run and
  carried by the StructureMap** (`tests/spec-test.rkt`, 48 checks): metadata lines (any element of
  the resource, `name.property` paths, `"""` markdown, a complex element begun with no value —
  `fml-map-metadata`; `structuremap.rkt` `metadata->elements`/`elements->metadata`), the status
  default `draft` (it was `active`), a source's `: type`, `min..max` (fewer or more values is an error
  at the rule) and `default (…)` (`fml-source` gained type/min/max/default), constants (`let name =
  expr;`, lazy, in terms of one another, hidden by a variable — `constant-values`, StructureMap
  `const`; `check.rkt` has them in scope), the short form, `then g(a), h(b) { rules }`, bare `share`
  as a target list mode, backticked element names.
- **`print.rkt`** (`fml->text : fml-map -> string`) writes a map — parsed, or read from a StructureMap
  by `structuremap->fml` — in the specified language only: the metadata header, urls and rule names
  in double quotes, `where`/`check`/`log`/`default` always in parentheses, strings as FHIRPath
  literals, an element name that is a g4 keyword or no plain name in backticks, one declaration and
  one rule to a line. Two things a StructureMap can hold have NO specified syntax (the g4:
  "Syntax for embedded ConceptMaps excluded for now"; a target's `listRuleId`): a map that has one is
  written with the reference implementation's `conceptmap "…" { … }` block under a comment saying
  so, and `share <id>`. `raco fml --decompile <StructureMap.json>`; fhir-mcp's edit (`write … ref=`,
  `describe … as=source`) hands a stored StructureMap with no kept text back with it.
- **What is read beyond the specification is said** (`parser/parse.rkt` `specification-notes`,
  appended by `check-map`: warnings at the form, so the editor, `raco fml check` and the workspace
  show them): `where x`/`check x` without parentheses ("MUST be surrounded by a set of
  parentheses"), `where` before the alias (the R3/R4 inter-version maps), `share <id>`, a
  `conceptmap` block, a `///` line that states no element. `parenthesized?` reads the clause's own
  text (positions kept for where-clause, check-clause, target-list-mode, conceptmap, metadata).
  Our own maps, guide, manual, tests and messages are in the specified form (`where (linkId =
  'gender')`, the metadata header); a message quotes a condition as `` `where (…)` ``.
- **Laws, over the corpora** (`tests/matchbox-harness.rkt` `printed-outcome`; `raco fml --tck`,
  `tests/print-test.rkt`): the printed text parses, means the map (the same StructureMap), printed
  again is the same text, and `specification-notes` finds in it nothing but `share <id>` and
  `conceptmap` — 58/58 corpus maps, 25/25 of matchbox's StructureMap RESOURCES (resource -> text ->
  resource), and the 332 inter-version R4 maps in CI (`--parse-corpus` now writes each back too).
- `support.rkt` reads the map's name and url from the metadata lines as well as from `map`.

**Found writing the manual** (2026-11-13; `tests/run-errors-test.rkt`). A call to a group nothing
defines was a plain `error` at run time (a stack trace from `raco fml --transform`, exit 0, and no
line in `$transform`'s OperationOutcome): `run-call` takes the rule and raises `rule-error`, naming
an import that did not resolve. `unresolved-imports` (transform.rkt) is that list; `check-map` warns
once per unresolved import (`check-imports`) and decides "no group …" whenever every import
resolved — any `imports` line used to switch that check off. A transform the engine does not run
(`escape`, `dateOp`, `pointer`; `transform-names` is the catalogue) is an error of the check at its
rule (`unsupported-transform`); it checked clean and failed when run. `validate-result #:drops`
says a rule wrote nothing because the lenient read dropped its value, for `raco fml check
--examples` as for `transform #:validate` (base.rkt passes its drops instead of rewriting the
message). The fhir-test-cases suite skips 4 tests (an XML expected output, or a map given as
StructureMap JSON): the tool's count, where the note above says "4 + 1".

**The editor checks against the structures** (2026-11-13): `support.rkt`'s `diagnose` loads the R5
types when no FHIR version is registered (as the `#lang fml` reader does). In the language server's
process none is, so `check-map` knew no type and a map writing `tgt.gendr` had no finding in the
editor or from `raco fhir-lsp check` (found running the manual's editor configurations in Neovim).

**Quality, performance and specification pass** (2026-11-13; `tests/alignment-test.rkt`, 75 checks,
each failing without its change). Held to a dump of everything the package produces over its corpora
(each map's StructureMap, text and findings; every tutorial map over every source of its step, from
the FML, from our StructureMap and from the reference's; the matchbox maps over six sources; the
guide's examples — 2,166 lines) and over the 332 inter-version maps (17,360 lines): identical after
each refactoring, every other difference one of the changes below. Tests 234 -> 318.
- **Quality** (no behaviour change). An EmptyReason is data: `empty-value` | `empty-where` structs
  (transform.rkt; it was a tagged list read with `caddr`/`list-ref`), recorded by `note-empty!`;
  `validate-result` is a `match` on it through `unwritten-note`/`nothing-written` (check.rkt; a
  90-line body). One walk shape for the FHIRPath AST (`fp-parts`, `fp-map`: `free-names`,
  `rewrite-vars` and `strip-value-steps` were three copies of the same recursion). `source-matches`
  is its steps (`source-values`, `check-cardinality!`, `in-list-mode`); `translate` its two lookups.
  One definition each: `run-error` (every failure of a run; `rule-error` names the rule),
  `group-param`, `url-leaf` and fhir/canonical's `canonical-url` (five spellings of "the type a url
  names"), `structures-source` and `resource-node` (registry.rkt: base.rkt, cli.rkt twice and
  registry.rkt each built the data source of `--structures`), `loc-of` (parse.rkt), `parameters`
  and `argument` (structuremap.rkt: a transform's and a group invocation's arguments cross the
  same way). `target-paths` is a fold (it `set!` an accumulator); `child-slot!` lost a box; the
  checker computes the unresolved imports once (`cx-unresolved`); unused requires dropped.
- **Defects found on the way**, each real and none reported before:
  a date, dateTime or time literal in a map carried a syntax object (`syntax->datum/locs` took
  `syntax-e` of a struct token): the StructureMap said `@#<syntax …>` and the expression evaluated
  to nothing, silently. On a file with Windows line ends every specification note quoted text from
  some lines up (`` `where then {` ``), and parenthesized conditions were flagged: a position
  counts `\r\n` as one character — all 332 inter-version maps and several tutorial maps are such
  files (`parse-fml` reads positions against the text with `\n` line ends). `(a+1)` did not parse
  (the lexer took `a+` as a name, for the flag `type+`: `type` is a keyword now, usable as a name).
  A parenthesized call — `= (now())`, `= (iif(a, b, c))` — was read as a transform of that name,
  which there is not (`in-parentheses?`, from the datum: parentheses are FHIRPath). `<<any words>>`
  was read as a type mode and dropped; a cardinality `1.5..2` was read (both parse errors now).
  `uuid()` wrote `"parameter": []`, which is not FHIR JSON. A `uses …|version as target` with an
  untyped target parameter found no target type. A group called with the wrong number of
  arguments, a constant defined by itself and an undeterminable target type were plain `error`s.
- **The specification's transforms** (its table is the code system `http://hl7.org/fhir/map-transform`;
  hl7.org was not reachable from this session, the definitions were read from the R5 core
  package): `escape(source, fmt1, fmt2)` between plain, java, xml and json (`escapings`, a table of
  read/write pairs); `pointer(resource)` — `urn:uuid:<id>`, as the reference implementation writes
  it; `cast(source)` with the target element's own type ("Target type can be left as implicit");
  `reference`/`pointer` of a resource being built give it a random id when it has none
  (`resource-identity`; it wrote `Basic/`), of a source without an id an error (it wrote
  `Patient/#f`), of an element an error; `cp(value)` infers the system from the value
  (`contact-system`); `id(…, type)` is a coding of the identifier-type code system (it was
  `type.text`). `dateOp` stays out: the specification says "Parameters to be documented" and the
  reference implementation does not run it.
- **The engine.** A `where`/`check` is read as FHIRPath reads a collection where a Boolean is
  expected (`fp-true?`: one Boolean is its value, one other item is true, nothing or several is
  not): every condition not exactly `true` was false, so matchbox's
  `where $this.resource.ofType(FHIR.Patient)` maps (copyaliasingbug, bundle-resolve, bundleobs) ran
  and wrote nothing. What is created into an element of an abstract type has its own type's shape
  (`narrowed`: a Patient created into `Bundle.entry.resource` had a `name` that did not repeat). An
  expression's value picks a choice element's key as a variable's does (`tgt.deceased = (d - 5
  days)` wrote the key `deceased`; qr2patfordates.map).
- **The StructureMap.** A group invocation's parameters are typed as a transform's (`valueId`, a
  string's own text as `valueString`, `valueInteger`; it wrote FHIRPath text: `"'lit'"`); R4's
  `dependent.variable` is read (the call lost its arguments — this file said it was); the
  reference's form of the simple rule (`source.variable` = `target.variable`, `create` of nothing)
  is read as the simple rule (run as written it copied nothing); an `evaluate` of one string
  literal is that literal (HL7's own supplyrequest-transform example did not print to the same map;
  the eight StructureMaps of the R4/R5 core packages and CRMI now hold the printer's laws,
  `tests/print-test.rkt`).
- **Read beyond the specification, now said** (`specification-notes`): an expression as a transform
  without parentheses (`= a & b`), a rule's name in single quotes, an expression given to a group
  (a StructureMap has no place for it). And a `uses` that names another FHIR version than the
  registered one is a warning at its line (`check-versions`): one tower to a process, so the map
  is checked and run against the registered version's type — it was silent.
- **One floor went down, on purpose**: the tutorial from the reference's stale StructureMaps, 30 ->
  29. `step4b.json`'s condition is the quoted string `'a21.isInteger()'`; it counted as a pass while
  a non-Boolean condition was false. The reference never runs step4b. Everything else: 6/6 + 15/15,
  43/43, 17/17, 37/37 (both ways), 58/58 round trip, 21/31, 55/58, 58/58 + 25/25 printed, 332/332.
- **Performance.** Measured, same machine, best of 3: parsing is what costs (58 corpus maps 252 ms,
  4.3 ms a map, brag's parser; unchanged), a run is 11–130 µs, a check ~0.1 ms after the
  foundation's package index is loaded once (~1 s, when a `uses` or `imports` is not registered).
  `raco fml --tck` 3.9 -> 3.1 s: 82% of it was `parse-fml` — nine checks each parsed the same files
  — and the harness parses a file once (`parse-map-file`). The sampling profiler put a third of a
  run in `regexp-split` under `uses-types`; timed directly `uses-types` is 1 µs a call and replacing
  the regexps changed nothing measurable (kept as the cleanup it is). Left alone: FHIRPath's
  static pass runs on every evaluation (~14% of a run; caching it by type is unsafe while a
  logical model can be registered again under the same name), and the `#lang fml` module's second
  parse for positions (once per process, ~5 ms).
- **Left, named.** How the specification's text encodes the simple form in a StructureMap could
  not be read offline; we read the reference's encoding and still write a source and a target
  with no variable and no transform — writing the reference's is one clause in `rule->jsexpr`.
  Our StructureMap against the reference's stays 21/31: nine are its stale conditions or missing
  rules, one that encoding. A map pinned to another FHIR version needs two towers in one process
  (the foundation registers types by name). The formal grammar wants two parameters to a group;
  one is read (the StructureMap allows it) and not remarked on.
