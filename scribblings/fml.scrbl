#lang scribble/manual
@(require (for-label racket/base racket/contract fml/parser/parse fml/structuremap fml/transform fml/operations
                     fml/registry fml/check fml/project))

@title{fml: the FHIR Mapping Language for Racket}

The @hyperlink["https://hl7.org/fhir/R5/mapping-language.html"]{FHIR Mapping Language}: a port of
the official @tt{mapping.g4} with the FHIRPath grammar inlined, so embedded expressions are real
FHIRPath ASTs; StructureMap emission; a transform engine over typed resources and logical models;
and a @tt{.map} file as a module. Corpora: the fhir-test-cases structure-mapping suite (6/6
executable JSON tests, 15/15 parses), the matchbox engine's maps (43/43 parse, 17/17 run) and the
FHIR mapping-language tutorial (37/37 of the results its reference asserts), and the FHIR R3/R4
inter-version maps (332/332 parse).

Part of the @hyperlink["https://github.com/oloveluck/fhir"]{fhir} family; depends on @tt{fhir},
@tt{fhir-tx} (@tt{translate} over the terminology service), @tt{fhirpath} and (for its error struct only) @tt{cql}.

@table-of-contents[]

@section{Install}

@verbatim{
raco pkg install --link --auto fhir/fhir fhirpath cql fml
    # from source, with the repos alongside
}

@section{Writing a map}

A map names its source and target types, then groups of rules. A rule reads source elements
(@tt{src.item as item where …}), binds them (@tt{as}), and writes targets (@tt{-> tgt.x = …});
@tt{then} calls another group or opens nested rules; the string at the end names the rule.

@verbatim|{#lang fml
map "http://example.org/StructureMap/bp-intake" = "BpIntake"
uses "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse" alias QuestionnaireResponse as source
uses "http://hl7.org/fhir/StructureDefinition/Observation" alias Observation as target

group BpIntake(source src : QuestionnaireResponse, target tgt : Observation) {
  src -> tgt.status = 'final' "status";
  src -> tgt.code = cc('http://loinc.org', '85354-9', 'Blood pressure panel') "code";
  src.subject as s -> tgt.subject = s "subject";
  src.item as item where linkId = 'systolic' -> tgt.component as c,
      c.code = cc('http://loinc.org', '8480-6', 'Systolic blood pressure') then reading(item, c) "systolic";
}

group reading(source item, target c) {
  item.answer as ans then {
    ans.value as v -> c.value = qty(v, 'mm[Hg]', 'http://unitsofmeasure.org', 'mm[Hg]') "value";
  } "answer";
}}|

Target values: a source variable (@tt{= s}), a literal, a FHIRPath expression in parentheses
(@tt{= (item.answer.valueString)}), or a transform: @tt{create('Type')}, @tt{copy}, @tt{reference},
@tt{cast}, @tt{evaluate}, @tt{truncate}, and the datatype constructors @tt{c} (Coding), @tt{cc}
(CodeableConcept), @tt{qty} (Quantity), @tt{id}, @tt{cp} (ContactPoint), @tt{append}. A choice
element on either side (@tt{ans.value}, @tt{tgt.effective}) reads whichever variant is present and
writes the variant of the value's type. A repeating target appends; @tt{share v} reuses one target
element across rules.

@section{A mapping problem, solved}

The problem: a clinic's front desk records blood-pressure readings on an intake form of its own
design, and the clinic's record keeps them as Observations that must conform to its screening
profile. Three things, one project (@tt{fhir-examples/examples/intake-maps}): the two structures
in FSH, the map between them in FML, and an example form.

@bold{1. The structures.} The form is a logical model, not a FHIR resource; the record is a
profile of Observation.

@verbatim|{Logical: IntakeForm
Id: IntakeForm
* patientId 1..1 string "The patient's record id"
* measured 1..1 dateTime "When the reading was taken"
* systolic 1..1 integer "Systolic pressure, mm[Hg]"
* diastolic 1..1 integer "Diastolic pressure, mm[Hg]"

Profile: ScreeningBP
Parent: Observation
* status = #final
* code = http://loinc.org#85354-9
* subject 1..1
* effective[x] 1..1
* effective[x] only dateTime
* component 2..*

Instance: intake-1
InstanceOf: IntakeForm
* patientId = "p1"
* measured = "2024-03-01T09:00:00Z"
* systolic = 120
* diastolic = 80}|

@bold{2. The map.} Its @tt{uses} name the two structures by canonical; the engine resolves the
logical model from the project and the profile's base type from the tower. Each rule reads one
form element and writes the Observation element it feeds.

@verbatim|{/// url = 'http://example.org/intake/StructureMap/IntakeToBP'
/// name = 'IntakeToBP'
uses "http://example.org/intake/StructureDefinition/IntakeForm" alias Intake as source
uses "http://example.org/intake/StructureDefinition/ScreeningBP" alias BP as target

group IntakeToBP(source src : Intake, target tgt : BP) {
  src -> tgt.status = 'final', tgt.code = cc('http://loinc.org', '85354-9', 'Blood pressure panel') "panel";
  src.patientId as id -> tgt.subject as s, s.reference = append('Patient/', id) "subject";
  src.measured as m -> tgt.effective = m "when";
  src.systolic as v -> tgt.component as c, c.code = cc('http://loinc.org', '8480-6', 'Systolic blood pressure'),
                       c.value = qty(v, 'mm[Hg]', 'http://unitsofmeasure.org', 'mm[Hg]') "systolic";
  src.diastolic as v -> tgt.component as c, c.code = cc('http://loinc.org', '8462-4', 'Diastolic blood pressure'),
                        c.value = qty(v, 'mm[Hg]', 'http://unitsofmeasure.org', 'mm[Hg]') "diastolic";
}}|

@bold{3. Check before running.} @tt{raco fsh check examples/intake-maps} compiles the structures,
checks the map against them and transforms the example form, validating the result against the
profile. A mistake is reported on its line, as the FSH checker reports an instance rule. Misspell
an element, assign a string where a Quantity goes, or forget a rule for @tt{code}, and the check
says so before any data moves:

@verbatim|{IntakeToBP.map:11:3: fml check error: `systolc` is not an element of IntakeForm
  src.systolc as v -> tgt.component as c, c.code = cc(...)
  ^
IntakeToBP.map:7:1: fml check warning: Observation.code is required (min 1) but no rule of IntakeToBP assigns it}|

Without a project, the same check is @tt{raco fml check IntakeToBP.map --structures sds/
--examples forms/ --profile ScreeningBP.json}.

@bold{4. Run it.} From the command line, @tt{raco fml IntakeToBP.map --transform intake-1.json
--structures sds/} prints the Observation:

@verbatim|{{"resourceType": "Observation", "status": "final",
 "code": {"coding": [{"system": "http://loinc.org", "code": "85354-9", "display": "Blood pressure panel"}]},
 "subject": {"reference": "Patient/p1"},
 "effectiveDateTime": "2024-03-01T09:00:00Z",
 "component": [{"code": {"coding": [{"system": "http://loinc.org", "code": "8480-6", "display": "Systolic blood pressure"}]},
                "valueQuantity": {"value": 120, "unit": "mm[Hg]", "system": "http://unitsofmeasure.org", "code": "mm[Hg]"}},
               {"code": {"coding": [{"system": "http://loinc.org", "code": "8462-4", "display": "Diastolic blood pressure"}]},
                "valueQuantity": {"value": 80, "unit": "mm[Hg]", "system": "http://unitsofmeasure.org", "code": "mm[Hg]"}}]}}|

The integers stayed integers inside the quantities, @tt{measured} landed on @tt{effectiveDateTime}
because the value is a dateTime, and each @tt{component} rule appended an entry.

@bold{5. As a value in a program.} Packed (@tt{raco fsh examples/intake-maps --pack intake.tgz}),
the project is a package holding both structures and the map, and the map is a typed procedure
resolved by its canonical:

@racketblock[
(require fml/registry fhir/sd/sd-index fhir/conformance/validate)
(parameterize ([current-sd-packages (list "intake.tgz" core)])
  (define to-bp (structure-map "http://example.org/intake/StructureMap/IntakeToBP"))
  (smap-source to-bp)                          (code:comment "'IntakeForm")
  (smap-target to-bp)                          (code:comment "'Observation")
  (define obs (to-bp form))                    (code:comment "the form above, as JSON")
  (validate-issues obs #:profiles (list screening-bp)))  (code:comment "'()")
]

Over HTTP, a server generated from that package serves the same map as
@tt{StructureMap/$transform} with @tt{source} naming the canonical. The test that does all of this
is @tt{fhir-examples/tests/intake-maps-test.rkt}.

@section{Using it}

@bold{As a module.} @racket[(require "intake.fml")] gives @racket[fml] (the AST),
@racket[structuremap] (the StructureMap jsexpr) and @racket[transform], the map as a procedure:
FHIR JSON (or a typed node) in, FHIR JSON out. A parse error is a compile error.
@tt{racket intake.fml} prints the StructureMap.

@bold{Logical models.} A @tt{uses} url the FHIR towers do not define (a logical model, as in the
mapping tutorial) is resolved as a StructureDefinition, from @racket[current-data-source] and then
the packages, and registered as a type; its instances are typed nodes like any other.
@racket[(transform src #:structures sds)] takes the StructureDefinitions (a list of jsexprs or a
data source); @litchar{--structures} does the same on the command line. A later definition of the
same type name (the tutorial's @tt{TLeft}, one per step) replaces the earlier one.

@racketblock[
(require (only-in "intake.fml" [transform intake->reading])
         fhir/operation (only-in fhir/conformance/operations ensure-valid))
(code:comment "map, then validate against a profile")
(~> form intake->reading (ensure-valid #:profile profile))
]

@bold{As an operation.} With the map as text, @tt{StructureMap/$transform} runs it; loading
@racket[fml/operations] registers it.

@racketblock[
(require fhir/operation fml/operations)
(apply-operation "StructureMap/$transform" qr #:srcMap fml-text)
(define transform (operation "StructureMap/$transform"))
(~> qr (transform #:srcMap fml-text) (ensure-valid))
]

@bold{From Racket.}

@racketblock[
(require fml/parser/parse fml/structuremap fml/transform fhir/result/result
         (code:comment "the engine needs a tower")
         fhir/r5/schemas fhir/serialize/from-json)
(define m (ok-value (parse-fml text)))
(code:comment "the StructureMap jsexpr")
(fml->structuremap m)
(code:comment "a typed source node -> the target jsexpr")
(run-map m (ok-value (from-json qr #:lenient? #t)))
]

@bold{A map as a value.} The StructureMap resource is the interchange form: @racket[fml->structuremap]
emits it and @racket[structuremap->fml] reads one back into the AST the engine runs, so a map from a
package or a data source runs without its FML. @racket[(structure-map ref)] turns a canonical, a
parsed map or a StructureMap resource into a typed procedure, found the way @tt{fhir/operation}
finds an OperationDefinition: maps registered in the process (a required @hash-lang[] @tt{fml}
module registers its own), StructureMaps in @racket[current-data-source], then the packages.

@racketblock[
(require fml/registry)
(define to-bp (structure-map "http://example.org/intake/StructureMap/IntakeToBP"))
(smap-source to-bp)   (code:comment "'IntakeForm")
(smap-target to-bp)   (code:comment "'Observation")
(to-bp form)          (code:comment "FHIR JSON in, FHIR JSON out")
((compose-maps a b) x) (code:comment "b after a, when a's target structure is b's source")
]

@tt{imports} resolve the same way: a group called by name is found in the map or any map it imports.
@tt{$transform} takes @tt{source} (a canonical) and @tt{sourceMap} (a StructureMap) as well as
@tt{srcMap}.

@bold{Checking a map against its structures.} @tt{raco fml check} (and @racket[check-map]) reports
what the two StructureDefinitions decide, positioned in the FML: an element the source or target
type lacks, a value whose type cannot go into the element, a group call with the wrong arity or no
such group, a required target element the entry group never assigns (a warning). With
@litchar{--examples}, each example is transformed and the result validated against its base type and
the @litchar{--profile}s; an issue is traced to the rule that wrote the element. Anything the
structures do not decide is left alone. In a project of FSH structures the same check runs under
@tt{raco fsh check} (the fsh package loads @tt{fml/project} on demand).

@bold{Command line.}

@verbatim{
raco fml intake.fml                          the StructureMap JSON (a StructureMap .json is read wherever a .map is)
raco fml intake.fml --transform form.json    run the map over an R5 resource; print the target
    [--structures <file|dir>]                the StructureDefinitions of the map's logical models
raco fml check intake.fml [--structures <file|dir>] [--examples <file|dir>] [--profile <sd.json>]...
                                             the map against its structures; examples transformed and validated
raco fml --parse-corpus <dir>                parse every .map under a directory, list the failures
raco fml --tck                               the vendored corpora, with floors
}

@litchar{--format text|json|outcome} chooses how an error is reported. Exit codes: 0, 1 on an FML error or
a suite regression, 2 on usage.

@section{What the engine does}

A rule runs for every combination of its sources' matches (each source's elements, @tt{where}
filtered with the alias bound to the candidate, in its list mode @tt{first}, @tt{last},
@tt{not_first}, @tt{not_last}, @tt{only_one}; @tt{check} must hold or the rule fails naming
itself; @tt{log} goes to @racket[current-fml-log]), then its targets in order, the dependent group
calls and the nested rules. A target without a transform creates its element; with one source and
one target the source value is carried over: a primitive is copied and coerced to the element's
type (@racket["12345"] into an integer element is @racket[12345]), a complex value runs the
@tt{types}/@tt{type+} group whose parameter types match, or is copied whole. @tt{extends} runs the
parent group first. Transforms: literals, @tt{(fhirpath)} over the rule's variables, @tt{create}
(a resource type gets its @tt{resourceType}), @tt{copy}, @tt{reference} (@tt{Type/id} of a built
target), @tt{cast} (an error when the value does not convert), @tt{evaluate}, @tt{truncate},
@tt{translate} (an embedded @tt{conceptmap} block, or a ConceptMap the terminology service can
see), @tt{uuid}, and the constructors @tt{c}, @tt{cc}, @tt{qty}, @tt{id}, @tt{cp}, @tt{append}.
A target may be a bare invocation (@tt{evaluate(result, resolve()) as obs}) whose value becomes a
source for dependent calls. Errors name the rule and, for an unknown transform or group, what is
supported.

@section{Limits}

@itemlist[
@item{@bold{Transforms.} @tt{escape}, @tt{dateOp} and @tt{pointer} raise @tt{unsupported
transform}. Target list modes other than @tt{share} are accepted and ignored (the tutorial's
expected results agree).}
@item{@bold{Declarations.} @tt{imports} resolve by canonical (registered maps, the data source,
the packages); one that resolves nowhere is skipped. @tt{let} constants, source types,
cardinalities and @tt{default} are parsed and dropped; @tt{unmapped for} in a @tt{conceptmap} is
parsed only. In embedded FHIRPath the @tt{as} operator is read as the target alias; use @tt{.as()}.}
@item{@bold{Against the reference.} Every tutorial result the reference engine's own tests assert
is reproduced. Two result files in the corpus are not among them: one for a map the reference never
runs (its comment says @tt{isInteger()} is not a FHIRPath function), and one where the reference
expects the run to throw (@tt{only_one} over two elements, which this engine also makes an error);
@tt{tests/matchbox-harness.rkt} names both with the evidence.}
@item{@bold{Versions and formats.} The executor targets R5 (sources from other versions are read
leniently); @tt{$transform} runs FML text only. The suite's four CDA tests (XML output) and the one
StructureMap-as-JSON input are skipped.}]

@section{Reference}

@subsection[#:style 'unnumbered]{@hash-lang[] @tt{fml}}
@defmodulelang[fml]

Provides @racket[fml], @racket[structuremap] and @racket[(transform src #:structures sds)], as
above. @racket[src] that is not a resource raises an argument error.

@subsection[#:style 'unnumbered]{@tt{fml/parser/parse}}
@defmodule[fml/parser/parse]

@defproc[(parse-fml [src string?] [#:source source (or/c string? path? #f) #f]) (or/c ok? err?)]{
@racket[(ok fml-map)], or @racket[(err cql-error)] carrying a diagnostic positioned in
@racket[source]. The AST (@tt{fml/parser/ast}) is prefab: @racket[fml-map] (url, name, metadata,
uses, imports, groups, conceptmaps), @racket[fml-group], @racket[fml-rule] (sources, targets,
dependents, nested rules), @racket[fml-source], @racket[fml-target] (context @racket[#f] for a bare
invocation), @racket[fml-conceptmap] (name, prefixes, mappings); a transform is
@racket[fml-literal], @racket[fml-fhirpath] (a FHIRPath AST), @racket[fml-invoke] or
@racket[fml-var].}

@subsection[#:style 'unnumbered]{@tt{fml/structuremap}}
@defmodule[fml/structuremap]

@defproc[(fml->structuremap [m fml-map?]) hash?]{
The StructureMap resource, the canonical compiled form of a mapping: status @racket["active"],
unnamed rules named @tt{rule-N}, @tt{conceptmap} blocks as contained ConceptMaps.}

@defproc[(structuremap->fml [sm hash?]) fml-map?]{
The inverse: a StructureMap resource read into the AST (embedded FHIRPath parsed, typed literal
parameters, R4 @tt{equivalence} as well as R5 @tt{relationship} in contained ConceptMaps). The
round trip returns the parsed AST up to the FML-only surface (@tt{///} metadata, conceptmap
prefixes) and the @tt{rule-N} names; every corpus map is checked.}

@subsection[#:style 'unnumbered]{@tt{fml/transform}}
@defmodule[fml/transform]

@defproc[(run-map [m fml-map?] [source fhir?]) hash?]{
Runs the map's first group over a typed source node and returns the target resource as a jsexpr.
Targets are shaped by the registered type schema (a repeating element appends, a @tt{value[x]} takes
the key of the value's type, a primitive element coerces what is assigned to it). Errors name the
rule.}

@defproc[(uses-types [m fml-map?]) hash?]{
The map's @tt{uses} resolved to type symbols, keyed by alias and type name; a structure the towers
lack is resolved as a StructureDefinition (the current data source, then the packages) and
registered. Call it before reading a source instance of a logical model.}

@defparam[current-fml-log log (-> string? any?)]{
Where a rule's @tt{log(…)} goes; nowhere by default.}

@defproc[(register-structure-map! [m fml-map?]) void?]{Resolvable by its url in this process.}
@defproc[(resolve-structure-map [url string?]) (or/c fml-map? #f)]{
Registered maps, then StructureMaps in @racket[current-data-source], then the packages; a
@tt{|version} is ignored.}
@defproc[(map-imports [m fml-map?]) (listof fml-map?)]{The map and its imports, transitively.}

@subsection[#:style 'unnumbered]{@tt{fml/registry}}
@defmodule[fml/registry]

@defproc[(structure-map [ref (or/c string? fml-map? hash?)] [#:structures structures (or/c #f data-source? (listof hash?)) #f]) smap?]{
A map as a typed procedure: by canonical (resolved as @racket[resolve-structure-map] does), a parsed
map, or a StructureMap resource. @racket[structures] are the StructureDefinitions of its logical
models. Applying it: @racket[(m src)], FHIR JSON or a typed node in, FHIR JSON out.}
@defproc[(smap-source [m smap?]) (or/c symbol? #f)]{The entry group's source structure type.}
@defproc[(smap-target [m smap?]) (or/c symbol? #f)]{Its target structure type.}
@defproc[(smap-ast [m smap?]) fml-map?]{The AST.}
@defproc[(compose-maps [a smap?] [b smap?]) smap?]{
@racket[b] after @racket[a]; an error unless @racket[a]'s target structure is @racket[b]'s source.}

@subsection[#:style 'unnumbered]{@tt{fml/check}}
@defmodule[fml/check]

@defproc[(check-map [m fml-map?] [#:source source (or/c string? path? #f) #f]) (listof diagnostic?)]{
The static findings, each a @tt{fhir/diagnostic} at its rule, source or target.}
@defproc[(check-map/examples [m fml-map?] [examples (listof hash?)]
                             [#:profiles profiles (listof hash?) '()] [#:source source (or/c string? path? #f) #f])
         (listof diagnostic?)]{
Each example transformed and validated (fhir-validation, loaded on demand) against its base type and
@racket[profiles]; an issue is positioned at the rule that wrote the element.}

@subsection[#:style 'unnumbered]{@tt{fml/project}}
@defmodule[fml/project]

What a project of structures loads on demand (@tt{fsh/project}, @tt{fsh/check}):
@defproc[(parse-map-file [file path-string?]) (values (or/c fml-map? #f) (listof diagnostic?))]
@defproc[(map-structuremap [m fml-map?]) hash?]
@defproc[(check-project-map [m fml-map?] [examples (listof hash?)] [profiles (listof hash?)]
                            [#:source source (or/c string? path? #f) #f]) (listof diagnostic?)]{
The static check, then the examples whose type is the map's source structure transformed and
validated against the target structure (the profile among @racket[profiles] when the map targets
one).}

@subsection[#:style 'unnumbered]{@tt{fml/operations}}
@defmodule[fml/operations]

@defthing[$transform procedure?]{
@tt{StructureMap/$transform} as a @tt{fhir/operation} value, registered when the module loads: the
subject is the content, @tt{#:srcMap} the FML text. The content is read leniently into the R5
tower. A StructureMap given only as @tt{source} or @tt{sourceMap} JSON is refused with an
OperationOutcome saying so.}
