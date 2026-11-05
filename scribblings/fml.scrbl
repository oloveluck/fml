#lang scribble/manual
@(require (for-label racket/base racket/contract fml/parser/parse fml/structuremap fml/transform fml/operations
                     fml/registry fml/check fml/project))

@title{fml: the FHIR Mapping Language for Racket}

The @hyperlink["https://hl7.org/fhir/R5/mapping-language.html"]{FHIR Mapping Language} for Racket.
You write a map from one structure to another in a @tt{.map} (or @tt{.fml}) file; @tt{raco fml}
checks it against the two StructureDefinitions, runs it over a resource, and emits the StructureMap.
A map file is a Racket module whose @racket[transform] is a function from source JSON to target JSON.

Held to the published corpora: the fhir-test-cases structure-mapping suite (6 of 6 executable
tests), the matchbox engine's maps and the FHIR mapping tutorial (43 of 43 parse, 37 of 37 tutorial
results), and the FHIR R3/R4 inter-version maps (332 of 332 parse). Part of the
@hyperlink["https://github.com/oloveluck/fhir"]{fhir} family.

@table-of-contents[]

@section{Quick start}

@bold{1. Install.} The packages are not on the Racket package server yet; clone the repositories
side by side and install from source:

@verbatim{
raco pkg install --link --auto fhir/fhir fhirpath vcl fhir/fhir-tx fml
}

@bold{2. Write a map.} From a QuestionnaireResponse to an Observation:

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

@bold{3. Check it, then run it.}

@verbatim{
$ raco fml check intake.fml
$ raco fml intake.fml --transform response.json
{"resourceType": "Observation", "status": "final",
 "code": {"coding": [{"system": "http://loinc.org", "code": "85354-9", "display": "Blood pressure panel"}]},
 "subject": {"reference": "Patient/p1"},
 "component": [{"code": {"coding": [{"system": "http://loinc.org", "code": "8480-6", ...}]},
                "valueQuantity": {"value": 120, "unit": "mm[Hg]", "system": "http://unitsofmeasure.org", "code": "mm[Hg]"}}]}
}

@tt{check} reports what the two structures decide, on the rule's line: an element neither type has
(@tt{intake.fml:9:3: fml check error: `subjct` is not an element of QuestionnaireResponse}), a
value whose type cannot go into the element, a wrong group call, a required target element no rule
assigns (a warning). A rule that fails at run time (a @tt{check} that does not hold, a @tt{cast}
that does not convert) is reported the same way.

@section{Writing a map}

A map names its source and target types (@tt{uses}), then groups of rules. A rule reads source
elements (@tt{src.item as item where …}), binds them (@tt{as}), and writes targets
(@tt{-> tgt.x = …}); @tt{then} calls another group or opens nested rules; the string at the end
names the rule. Target values: a variable, a literal, a FHIRPath expression in parentheses, or a
transform: @tt{create('Type')}, @tt{copy}, @tt{reference}, @tt{cast}, @tt{evaluate},
@tt{truncate}, @tt{translate}, @tt{uuid}, and the constructors @tt{c} (Coding), @tt{cc}
(CodeableConcept), @tt{qty} (Quantity), @tt{id}, @tt{cp}, @tt{append}. A choice element on either
side (@tt{ans.value}, @tt{tgt.effective}) reads whichever variant is present and writes the variant
of the value's type; a repeating target appends; @tt{share v} reuses one target element across
rules. The language reference is the
@hyperlink["https://hl7.org/fhir/R5/mapping-language.html"]{FHIR specification}.

@bold{Logical models.} A @tt{uses} url the FHIR towers do not define (a form of your own design) is
resolved as a StructureDefinition from @racket[current-data-source] and the packages and registered
as a type. Give the definitions with @litchar{--structures <file|dir>} on the command line or
@racket[#:structures] from Racket. In a project of FSH structures, @tt{raco fsh check} does this for
every @tt{.map} file; the worked project is @tt{fhir-examples/examples/intake-maps}, walked through
in @secref["intake"].

@section{Using it}

@subsection{Command line}

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @tt{raco fml map.fml} "the StructureMap JSON")
      (list @tt{raco fml map.fml --transform src.json [--structures sds/]} "run the map; print the target")
      (list @tt{raco fml check map.fml [--structures sds/] [--examples ex/] [--profile sd.json]} "the map against its structures; examples transformed and the results validated")
      (list @tt{raco fml --parse-corpus dir/} @elem{parse every @tt{.map} under a directory})
      (list @tt{raco fml --tck} "the vendored corpora, with floors"))]

A StructureMap @tt{.json} is read wherever a @tt{.map} is. @litchar{--format text|json|outcome}
chooses how an error is reported. Exit codes: 0, 1 on an FML error or a suite regression, 2 on usage.

@subsection{As a module}

@racket[(require "intake.fml")] gives @racket[fml] (the AST), @racket[structuremap] (the
StructureMap jsexpr) and @racket[transform], the map as a procedure: FHIR JSON (or a typed node) in,
FHIR JSON out, with @racket[#:structures] for logical models. A parse error is a compile error.
Requiring the module registers the StructureMap, so @racket[structure-map] and @tt{$transform} find
it by canonical. @tt{racket intake.fml} prints the StructureMap.

@racketblock[
(require (only-in "intake.fml" [transform intake->reading])
         fhir/operation (only-in fhir/conformance/operations ensure-valid))
(code:comment "map, then validate against a profile")
(~> response intake->reading (ensure-valid #:profile profile-url))
]

@subsection{As a value or an operation}

@racket[(structure-map ref)] turns a canonical, a parsed map or a StructureMap resource into a typed
procedure, found among the maps registered in the process, then @racket[current-data-source], then
the packages. @tt{StructureMap/$transform} is the same as an operation, with the map as text
(@tt{srcMap}), a canonical (@tt{source}) or a resource (@tt{sourceMap}).

@racketblock[
(require fml/registry fml/operations fhir/operation)
(define to-bp (structure-map "http://example.org/StructureMap/bp-intake"))
(smap-source to-bp)                                    (code:comment "'QuestionnaireResponse")
(to-bp response)                                       (code:comment "FHIR JSON in, FHIR JSON out")
((compose-maps a b) x)                                 (code:comment "b after a")
(apply-operation "StructureMap/$transform" response #:srcMap fml-text)
]

@subsection{From Racket}

@racketblock[
(require fml/parser/parse fml/structuremap fml/transform fml/check fhir/result/result
         fhir/r5/schemas fhir/serialize/from-json)             (code:comment "the engine needs a tower")
(define m (ok-value (parse-fml text #:source "intake.fml")))
(fml->structuremap m)                                           (code:comment "the StructureMap jsexpr")
(check-map m)                                                   (code:comment "(listof diagnostic)")
(run-map m (ok-value (from-json response #:lenient? #t)))      (code:comment "typed source -> target jsexpr")
(structuremap->fml sm)                                          (code:comment "a StructureMap back to the AST")
]

@section[#:tag "intake"]{A mapping problem, solved}

The problem: a clinic's front desk records blood-pressure readings on an intake form of its own
design, and the clinic's record keeps them as Observations that must conform to its screening
profile. One project (@tt{fhir-examples/examples/intake-maps}) holds the two structures in FSH, the
map between them, and an example form.

@bold{1. The structures.} The form is a logical model; the record is a profile of Observation.

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

@bold{2. The map.} Its @tt{uses} name the two structures by canonical; each rule reads one form
element and writes the Observation element it feeds.

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

@bold{3. Check, run, use.} @tt{raco fsh check examples/intake-maps} compiles the structures, checks
the map against them and transforms the example form, validating the result against the profile;
misspell an element or forget the rule for @tt{code} and the check says so on the line. @tt{raco fsh
examples/intake-maps --pack intake.tgz} packs both structures and the StructureMap into one
package, from which the map is a typed procedure by canonical:

@racketblock[
(require fml/registry fhir/sd/sd-index)
(parameterize ([current-sd-packages (list "intake.tgz" core)])
  (define to-bp (structure-map "http://example.org/intake/StructureMap/IntakeToBP"))
  (to-bp form))                                (code:comment "the Observation")
]

Over HTTP, a server generated from that package serves the same map as
@tt{StructureMap/$transform}. The test that does all of this is
@tt{fhir-examples/tests/intake-maps-test.rkt}.

@section{What the engine does}

A rule runs for every combination of its sources' matches (@tt{where} filtered, in its list mode
@tt{first}, @tt{last}, @tt{not_first}, @tt{not_last}, @tt{only_one}; @tt{check} must hold or the
rule fails naming itself; @tt{log} goes to @racket[current-fml-log]), then its targets in order,
the dependent group calls and the nested rules. A target without a transform creates its element;
with one source and one target the value is carried over: a primitive is copied and coerced to the
element's type (@racket["12345"] into an integer element is @racket[12345]), a complex value runs
the @tt{types}/@tt{type+} group whose parameter types match, or is copied whole. @tt{extends} runs
the parent group first. @tt{translate} uses an embedded @tt{conceptmap} block or a ConceptMap the
terminology service can see. Errors name the rule and, for an unknown transform or group, what is
supported.

@section{In an editor, for an agent}

@tt{fml/support} is what the family's language server (fhir-lsp) and MCP server (fhir-mcp) load for
FML: a map's diagnostics (parse, then the check against its structures), the maps beside it as its
project, the declarations as an outline, hover, go to definition (an @tt{imports} url leads to the
map that declares it), and completion of declared names and keywords. It is a @tt{fhir/lang/support}
value; @tt{$transform} is already an operation, so no tool is added.

@section{Limits}

@itemlist[
@item{@bold{Transforms.} @tt{escape}, @tt{dateOp} and @tt{pointer} raise @tt{unsupported
transform}. Target list modes other than @tt{share} are accepted and ignored.}
@item{@bold{Declarations.} @tt{imports} resolve by canonical (registered maps, the data source, the
packages); one that resolves nowhere is skipped. @tt{let} constants, source types, cardinalities and
@tt{default} are parsed and dropped. In embedded FHIRPath the @tt{as} operator is read as the
target alias; use @tt{.as()}.}
@item{@bold{Against the reference.} Every tutorial result the reference engine's own tests assert is
reproduced; two result files in the corpus are not among them (one for a map the reference never
runs, one where it expects the run to fail, as this engine does), named in
@tt{tests/matchbox-harness.rkt}.}
@item{@bold{Versions and formats.} The executor targets R5 (sources from other versions are read
leniently). The suite's four CDA tests (XML output) are skipped.}]

@section{Reference}

@subsection[#:style 'unnumbered]{@hash-lang[] @tt{fml}}
@defmodulelang[fml]

Provides @racket[fml], @racket[structuremap] and @racket[(transform src #:structures sds)], as
above. @racket[src] that is not a resource raises an argument error.

@subsection[#:style 'unnumbered]{@tt{fml/parser/parse}}
@defmodule[fml/parser/parse]

@defproc[(parse-fml [src string?] [#:source source (or/c string? path? #f) #f]) (or/c ok? err?)]{
@racket[(ok fml-map)], or @racket[(err fml-error)] carrying a diagnostic positioned in
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
the key of the value's type, a primitive element coerces what is assigned to it). A failure while
running a rule (a @tt{check} that does not hold, @tt{only_one} over several elements, a @tt{cast}
that cannot convert, an unknown group) raises @tt{exn:fail:diagnostic} positioned at the rule, as a
parse error is.}

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
tower; @tt{sourceMap} is a StructureMap resource and @tt{source} a canonical (registered maps, the
data source, the registry, the packages). A rule that fails at run time comes back as an
OperationOutcome whose issue carries the rule's file and line (the standard
@tt{operationoutcome-issue-line} extensions).}
