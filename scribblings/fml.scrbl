#lang scribble/manual
@(require (for-label racket/base racket/contract fml/parser/parse fml/structuremap fml/transform fml/operations))

@title{fml: the FHIR Mapping Language for Racket}

The @hyperlink["https://hl7.org/fhir/R5/mapping-language.html"]{FHIR Mapping Language}: a port of
the official @tt{mapping.g4} with the FHIRPath grammar inlined, so embedded expressions are real
FHIRPath ASTs; StructureMap emission; a transform engine over typed resources and logical models;
and a @tt{.map} file as a module. Corpora: the fhir-test-cases structure-mapping suite (6/6
executable JSON tests, 15/15 parses), the matchbox engine's maps (43/43 parse, 17/17 run) and the
FHIR mapping-language tutorial (36/38 expected results; the two differences are named in
@tt{tests/matchbox-harness.rkt}), and the FHIR R3/R4 inter-version maps (332/332 parse).

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

@bold{Command line.}

@verbatim{
raco fml intake.fml                          the StructureMap JSON
raco fml intake.fml --transform form.json    run the map over an R5 resource; print the target
    [--structures <file|dir>]                the StructureDefinitions of the map's logical models
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
@item{@bold{Declarations.} @tt{imports} is emitted but never loaded (a group an @tt{extends}
names outside the map is skipped); @tt{let} constants, source types, cardinalities and
@tt{default} are parsed and dropped; @tt{unmapped for} in a @tt{conceptmap} is parsed only. In
embedded FHIRPath the @tt{as} operator is read as the target alias; use @tt{.as()}.}
@item{@bold{Against the reference.} Two tutorial cases differ by design: @tt{only_one} over two
elements is an error here (the reference keeps the last value), and a @tt{where} calling a function
FHIRPath does not have is false here (the reference copies the value).}
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

@subsection[#:style 'unnumbered]{@tt{fml/operations}}
@defmodule[fml/operations]

@defthing[$transform procedure?]{
@tt{StructureMap/$transform} as a @tt{fhir/operation} value, registered when the module loads: the
subject is the content, @tt{#:srcMap} the FML text. The content is read leniently into the R5
tower. A StructureMap given only as @tt{source} or @tt{sourceMap} JSON is refused with an
OperationOutcome saying so.}
