#lang scribble/manual
@(require (for-label racket/base racket/contract fml/parser/parse fml/structuremap fml/transform fml/operations))

@title{fml: the FHIR Mapping Language for Racket}

The @hyperlink["https://hl7.org/fhir/R5/mapping-language.html"]{FHIR Mapping Language}: a port of
the official @tt{mapping.g4} with the FHIRPath grammar inlined, so embedded expressions are real
FHIRPath ASTs; StructureMap emission; a transform engine over typed resources; and a @tt{.map} file
as a module. The fhir-test-cases structure-mapping suite passes: 6/6 executable JSON tests, 15/15
parses.

Part of the @hyperlink["https://github.com/oloveluck/fhir"]{fhir} family; depends on @tt{fhir},
@tt{fhirpath} and (for its error struct only) @tt{cql}.

@table-of-contents[]

@section{Install}

@verbatim{
raco pkg install --link --auto fhir/fhir fhirpath cql fml      # from source, with the repos alongside
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

@racketblock[
(require (only-in "intake.fml" [transform intake->reading])
         fhir/operation (only-in fhir/conformance/operations ensure-valid))
(~> form intake->reading (ensure-valid #:profile profile))   (code:comment "map, then validate against a profile")
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
         fhir/r5/schemas fhir/serialize/from-json)          (code:comment "the engine needs a tower")
(define m (ok-value (parse-fml text)))
(fml->structuremap m)                                      (code:comment "the StructureMap jsexpr")
(run-map m (ok-value (from-json qr #:lenient? #t)))        (code:comment "a typed source node -> the target jsexpr")
]

@bold{Command line.}

@verbatim{
raco fml intake.fml                          the StructureMap JSON
raco fml intake.fml --transform form.json    run the map over an R5 resource; print the target
raco fml --tck                               the structure-mapping suite
}

@tt{--format text|json|outcome} chooses how an error is reported. Exit codes: 0, 1 on an FML error or
a suite regression, 2 on usage.

@section{Reference}

@subsection[#:style 'unnumbered]{@hash-lang[] @tt{fml}}
@defmodulelang[fml]

Provides @racket[fml], @racket[structuremap] and @racket[(transform src)], as above. @racket[src]
that is not a resource raises an argument error.

@subsection[#:style 'unnumbered]{@tt{fml/parser/parse}}
@defmodule[fml/parser/parse]

@defproc[(parse-fml [src string?] [#:source source (or/c string? path? #f) #f]) (or/c ok? err?)]{
@racket[(ok fml-map)], or @racket[(err cql-error)] carrying a diagnostic positioned in
@racket[source]. The AST (@tt{fml/parser/ast}) is prefab: @racket[fml-map] (url, name, metadata,
uses, imports, groups), @racket[fml-group], @racket[fml-rule] (sources, targets, dependents, nested
rules), @racket[fml-source], @racket[fml-target]; a transform is @racket[fml-literal],
@racket[fml-fhirpath] (a FHIRPath AST), @racket[fml-invoke] or @racket[fml-var].}

@subsection[#:style 'unnumbered]{@tt{fml/structuremap}}
@defmodule[fml/structuremap]

@defproc[(fml->structuremap [m fml-map?]) hash?]{
The StructureMap resource, the canonical compiled form of a mapping: status @racket["active"],
unnamed rules named @tt{rule-N}.}

@subsection[#:style 'unnumbered]{@tt{fml/transform}}
@defmodule[fml/transform]

@defproc[(run-map [m fml-map?] [source fhir?]) hash?]{
Runs the map's first group over a typed source node and returns the target resource as a jsexpr.
Targets are shaped by the registered type schema (a repeating element appends, a @tt{value[x]} takes
the key of the value's type). Errors are raised by @racket['run-map].}

@subsection[#:style 'unnumbered]{@tt{fml/operations}}
@defmodule[fml/operations]

@defthing[$transform procedure?]{
@tt{StructureMap/$transform} as a @tt{fhir/operation} value, registered when the module loads: the
subject is the content, @tt{#:srcMap} the FML text. The content is read leniently into the R5
tower. A StructureMap given only as @tt{source} or @tt{sourceMap} JSON is refused with an
OperationOutcome saying so.}
