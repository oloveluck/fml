#lang scribble/manual
@(require (for-label racket/base racket/contract fml/parser/parse fml/structuremap fml/transform fml/operations))

@title{fml: the FHIR Mapping Language for Racket}

The @hyperlink["https://hl7.org/fhir/R5/mapping-language.html"]{FHIR Mapping Language}: a port of
the official @tt{mapping.g4} with the FHIRPath grammar inlined, so embedded expressions are real
FHIRPath ASTs; StructureMap emission; a transform engine over typed resources; and @hash-lang[]
@tt{fml}. The fhir-test-cases structure-mapping suite passes: 6/6 executable JSON tests, 15/15 parses.

Part of the @hyperlink["https://github.com/oloveluck/fhir"]{fhir} family; depends on @tt{fhir},
@tt{fhirpath} and (for its error struct only) @tt{cql}.

@table-of-contents[]

@section{Install}

@verbatim{
raco pkg install --link --auto fhir/fhir fhirpath cql fml      # from source, with the repos alongside
}

@section{@hash-lang[] @tt{fml}}
@defmodulelang[fml]

A @tt{.map} file is one mapping. The module provides @racket[fml] (the AST), @racket[structuremap]
(the StructureMap jsexpr) and @racket[transform]: the map as a procedure from a source resource (FHIR
JSON or a typed node) to the target resource as FHIR JSON. Running the file prints the StructureMap.

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

@section{Parsing}
@defmodule[fml/parser/parse]

@defproc[(parse-fml [src string?] [#:source source (or/c string? path? #f) #f]) (or/c ok? err?)]{
@racket[(ok fml-map)], or @racket[(err cql-error)] carrying a diagnostic positioned in
@racket[source]. The AST (@tt{fml/parser/ast}) is prefab: @racket[fml-map] (url, name, metadata,
uses, imports, groups), @racket[fml-group], @racket[fml-rule] (sources, targets, dependents, nested
rules), @racket[fml-source], @racket[fml-target]; a transform is @racket[fml-literal],
@racket[fml-fhirpath] (a FHIRPath AST), @racket[fml-invoke] or @racket[fml-var].}

@section{StructureMap}
@defmodule[fml/structuremap]

@defproc[(fml->structuremap [m fml-map?]) hash?]{
The StructureMap resource: the canonical compiled form of a mapping, status @racket["active"],
unnamed rules named @tt{rule-N}.}

@section{Transforming}
@defmodule[fml/transform]

@defproc[(run-map [m fml-map?] [source fhir?]) hash?]{
Runs the map's first group over a typed source node and returns the target resource as a jsexpr.
A rule matches its source elements (filtered by @tt{where}), binds its variables, then runs its
targets, @tt{then} calls and nested rules. Targets are shaped by the registered type schema: a
repeating element appends, a @tt{value[x]} takes the key of the value's type. The transforms:
@tt{create}, @tt{copy}, @tt{reference}, @tt{cast}, @tt{evaluate}, @tt{truncate}, and the constructors
@tt{c}, @tt{cc}, @tt{qty}, @tt{id}, @tt{cp}, @tt{append}.}

@section{As an operation}
@defmodule[fml/operations]

@defthing[$transform procedure?]{
@tt{StructureMap/$transform} as a @tt{fhir/operation} value, registered when the module loads. It
runs FML given as @tt{srcMap}: @racket[($transform content #:srcMap fml-text)], or
@racket[(apply-operation "StructureMap/$transform" qr #:srcMap fml)], or in a pipeline
@racket[(~> qr ($transform #:srcMap fml) (ensure-valid))]. @racket[content] is FHIR JSON, read
leniently into the R5 tower. A StructureMap given only as @tt{source} or @tt{sourceMap} JSON is
refused with an OperationOutcome saying so.}

@section{Command line}

@verbatim{
raco fml <file.map>                          the StructureMap JSON
raco fml <file.map> --transform <src.json>   run the map over an R5 resource; print the target
raco fml --tck                               the structure-mapping suite
}

@tt{--format text|json|outcome} chooses how an error is reported. Exit codes: 0, 1 on an FML error or
a suite regression, 2 on usage.
