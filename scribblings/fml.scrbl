#lang scribble/manual
@(require scribble/core
          (for-label racket/base racket/contract fml/parser/parse fml/structuremap fml/print fml/transform fml/operations
                     fml/registry fml/check fml/project fml/workspace))
@; code as written: no curly quotes, no dashes joined (what @tt would do to 'x' and --option)
@(define (c . strs) (element 'tt strs))

@title{fml: the FHIR Mapping Language}

Health data rarely arrives in the shape you need it in. A patient fills in a questionnaire, and the
record wants a Patient. A clinic's front desk has an intake form of its own, and the clinic keeps
blood-pressure readings as Observations. A system speaks one version of FHIR and its neighbour
another. Each time, someone has to say which piece of the one becomes which piece of the other.

The @hyperlink["https://hl7.org/fhir/R5/mapping-language.html"]{FHIR Mapping Language} (FML) is the
standard way to write that down. A @italic{map} is a text file of rules, and each rule reads "take
this from the source, write that to the target". It is part of FHIR, the health-data standard, so a
map written here can be read and run by other FHIR tools.

This package checks a map against the two shapes it converts between, runs it over your data, and
points at the line when something is wrong.

Part of the @hyperlink["https://oloveluck.github.io/fhir/"]{fhir} family of FHIR languages and tools.

@table-of-contents[]

@section{What you can do with it}

@itemlist[
@item{@bold{Convert one piece of data}: a filled-in form into a Patient (@secref["first"]).}
@item{@bold{Check a map before you run it}: a misspelt element, a value of the wrong kind or a
missing required element is reported at its rule (@secref["checking"]).}
@item{@bold{Test a map on examples}: convert each one, and be told which rule produced an invalid
result (@secref["checking"]).}
@item{@bold{Convert data that is not FHIR}: a shape of your own, described once
(@secref["logical"]).}
@item{@bold{Use a map from a program}: as a function from source data to target data
(@secref["module"]).}
@item{@bold{Exchange maps with other tools}: as StructureMap resources, and read a StructureMap
back as text (@secref["structuremap"]).}]

@section[#:tag "first"]{A first example}

@bold{1. Install} the tools as the @hyperlink["https://oloveluck.github.io/fhir/"]{family's front
page} describes. The command is then @tt{raco fml} (or @tt{fml}, if you built the standalone tools).

@bold{2. Have something to convert.} Here it is the answers to a two-question form, as FHIR records
them: a @italic{QuestionnaireResponse}, with one @tt{item} per question. Each item names its question
(@tt{linkId}) and holds the answer. Save it as @tt{qr.json}:

@verbatim|{{"resourceType": "QuestionnaireResponse", "status": "completed",
 "item": [{"linkId": "gender", "answer": [{"valueCoding": {"code": "female"}}]},
          {"linkId": "birth",  "answer": [{"valueDate": "1980-02-03"}]}]}}|

This is the @italic{source}. What you want from it is a Patient with a gender and a birth date: the
@italic{target}.

@bold{3. Write the map}, @tt{qr2pat.map}:

@verbatim|{/// url = 'http://example.org/StructureMap/qr2patient'
/// name = 'QR2Patient'

uses "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse" as source
uses "http://hl7.org/fhir/StructureDefinition/Patient" as target

group main(source src : QuestionnaireResponse, target tgt : Patient) {
  src.item as item where (linkId = 'gender') -> tgt.gender = (item.answer.value.code) "gender";
  src.item as item where (linkId = 'birth') then {
    item.answer as a -> tgt.birthDate = (a.value) "birthDate";
  } "birth";
}}|

Reading it from the top:

@itemlist[
@item{The two @c{///} lines are the map's header. @tt{url} is the address the map is known by, so
that other maps and tools can refer to it; it does not have to exist on the web. @tt{name} is a
short name for it.}
@item{Each @tt{uses} line names one of the two shapes by the address of its definition, and says
which side it is on. Every FHIR type has such an address under
@tt{http://hl7.org/fhir/StructureDefinition/}.}
@item{A @tt{group} is a set of rules. This one takes a QuestionnaireResponse, which the rules call
@tt{src}, and fills in a Patient, which they call @tt{tgt}. The names are yours to choose.}
@item{The first rule reads: for each @tt{item} of the source whose @tt{linkId} is @c{'gender'},
write the code of its answer to the target's @tt{gender}. The left of the arrow is what to take,
the right is where to put it. The text in parentheses is
@hyperlink["https://hl7.org/fhirpath/"]{FHIRPath}, FHIR's small language for picking values out of
data.}
@item{The second rule finds the @c{'birth'} item, and @tt{then} opens a rule inside it: for each
answer, write its value to @tt{birthDate}.}
@item{The quoted word at the end of a rule is its name. Messages use it to say which rule they are
about.}]

@bold{4. Check it.} The check compares the map with the two shapes, without any data:

@verbatim{
$ raco fml check qr2pat.map
qr2pat.map: 0 findings
}

No findings means every element the map names exists, and every value fits where it is put.

@bold{5. Run it} over the form:

@verbatim|{
$ raco fml qr2pat.map --transform qr.json
{"resourceType":"Patient","gender":"female","birthDate":"1980-02-03"}
}|

That is the target: a Patient with the two values the rules wrote. Add @c{--indent} to have it
laid out over several lines.

@bold{6. Make a mistake.} Write @tt{tgt.gendr} in the first rule and check again:

@verbatim|{
$ raco fml check bad.map
bad.map:8:49: fml check error: `gendr` is not an element of Patient (did you mean `gender`?)
 8 |   src.item as item where (linkId = 'gender') -> tgt.gendr = (item.answer.value.code) "gender";
   |                                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
bad.map: 1 finding
}|

The message gives the file, the line and the column, and the name it thinks you meant. Running a
map does not check it first: run this one and you get a Patient with a @tt{gendr} element, which no
FHIR tool will accept. So check, then run.

@bold{7. See what it compiles to.} Run with no option and the map is printed as a
@italic{StructureMap}, the FHIR resource that holds a map as data:

@verbatim|{
$ raco fml qr2pat.map
{"group":[{"input":[{"mode":"source","name":"src","type":"QuestionnaireResponse"}, … ],
 "name":"QR2Patient","resourceType":"StructureMap","status":"draft", … }
}|

You rarely need to look at it. It is the form in which maps are stored on FHIR servers and passed
between tools; the text is the form you read and edit (@secref["structuremap"]).

@section[#:tag "writing"]{Writing a map}

This section goes through the parts of a map in the order you write them. The language itself is
defined by the @hyperlink["https://hl7.org/fhir/R5/mapping-language.html"]{FHIR specification}, and
HL7's @hyperlink["https://hl7.org/fhir/R5/mapping-tutorial.html"]{mapping tutorial} builds it up
step by step.

@subsection{The header, the shapes, the groups}

A map starts by saying what it is and what it converts:

@verbatim|{/// url = 'http://example.org/StructureMap/bp-intake'
/// name = 'BpIntake'
/// title = 'Blood pressure from the intake questionnaire'

uses "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse" alias QuestionnaireResponse as source
uses "http://hl7.org/fhir/StructureDefinition/Observation" alias Observation as target}|

@itemlist[
@item{@bold{The header} is one @c{/// element = value} line for each fact about the map. @tt{url}
and @tt{name} are required. Others may follow, such as @tt{title} or @tt{status}; a map that states
no status is a @tt{draft}.}
@item{@bold{@tt{uses}} names a shape and its side, @tt{source} or @tt{target}. @tt{alias} gives it
a short name for the rest of the file; without one, the last part of the address is the name.}
@item{@bold{@tt{imports "<url>"}} lets a map call the groups of another map, named by its
@tt{url}.}
@item{@bold{A group} is a named list of rules with its inputs in parentheses:
@c{group main(source src : QuestionnaireResponse, target tgt : Observation) { … }}. The first group
is where a run starts.}]

@subsection{Rules}

A rule has a left side that reads the source and, after @c{->}, a right side that writes the
target. It ends with a semicolon, and may carry a name in double quotes before it. Here is a map
with most of what a rule can do, a blood-pressure reading from a form:

@verbatim|{group BpIntake(source src : QuestionnaireResponse, target tgt : Observation) {
  src -> tgt.status = 'final' "status";
  src -> tgt.code = cc('http://loinc.org', '85354-9', 'Blood pressure panel') "code";
  src.subject as s -> tgt.subject = s "subject";
  src.item as item where (linkId = 'systolic') -> tgt.component as c,
      c.code = cc('http://loinc.org', '8480-6', 'Systolic blood pressure') then reading(item, c) "systolic";
}

group reading(source item, target c) {
  item.answer as ans then {
    ans.value as v -> c.value = qty(v, 'mm[Hg]', 'http://unitsofmeasure.org', 'mm[Hg]') "value";
  } "answer";
}}|

@itemlist[
@item{@bold{Reading.} @c{src.subject as s} takes the source's @tt{subject} and calls it @tt{s}.
When the element repeats, as @tt{item} does, the rule runs once for each one. A bare @tt{src}
reads nothing in particular: the rule simply runs once.}
@item{@bold{Filtering.} @c{where (linkId = 'systolic')} keeps only the items for which the
condition holds. The condition is FHIRPath, in parentheses, and is asked of the element just read.}
@item{@bold{Writing.} @c{tgt.status = 'final'} writes a fixed value; @c{tgt.subject = s} writes
what the left side read; @c{tgt.gender = (item.answer.value.code)} writes what a FHIRPath
expression in parentheses finds. Several writes are separated by commas.}
@item{@bold{Making a new part.} @c{tgt.component as c} adds a component to the Observation and
calls it @tt{c}, so the writes after it can fill it in. An element that can repeat gets a new entry
each time; one that cannot is set.}
@item{@bold{Built-in helpers}, called @italic{transforms}, make the values FHIR uses most. @tt{cc}
makes a coded concept from a code system, a code and a display text; @tt{qty} makes a quantity
from a number and a unit. The full list is in @secref["transforms"].}
@item{@bold{Going deeper.} @c{then { … }} opens rules inside a rule, which can use everything the
outer rule named. @c{then reading(item, c)} hands the work to another group.}
@item{@bold{Elements that come in several kinds.} FHIR writes an answer's value as
@tt{valueInteger}, @tt{valueString}, @tt{valueCoding} and so on, depending on its kind, and an
Observation's value likewise. In a map you write just @tt{value}: reading takes whichever is
there, and writing picks the name from the kind of value, so the quantity above lands in
@tt{valueQuantity}.}]

Over a response whose subject is @tt{Patient/p1} and whose @tt{systolic} item is answered
@tt{120}, the map gives this (the middle is left out here):

@verbatim|{
$ raco fml intake.map --transform response.json --indent
{
  "resourceType": "Observation",
  "status": "final",
  "code": {
    "coding": [
      {
        "system": "http://loinc.org",
        "code": "85354-9",
        "display": "Blood pressure panel"
      }
    ]
  },
  "subject": {
    "reference": "Patient/p1"
  },
  "component": [
    {
      …
      "valueQuantity": {
        "value": 120,
        "unit": "mm[Hg]",
        "system": "http://unitsofmeasure.org",
        "code": "mm[Hg]"
      }
    }
  ]
}
}|

@subsection{More that a rule can say}

These are the remaining forms of the language. Each of them runs here.

@verbatim|{/// url = 'http://example.org/StructureMap/forms'
/// name = 'Forms'

uses "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse" as source
uses "http://hl7.org/fhir/StructureDefinition/Observation" as target

let loinc = 'http://loinc.org';

group main(source src : QuestionnaireResponse, target tgt : Observation) {
  src.status : code 1..1 as s -> tgt.status = 'final' "status";
  src.language default ('en') as l -> tgt.language = l "language";
  src.item first as item where (linkId = 'weight') check (item.answer.exists()) log (item.linkId) ->
    tgt.code = cc(loinc, '29463-7'), tgt.value = qty(item.answer.value, 'kg') "weight";
  src -> tgt: id, implicitRules;
}}|

@itemlist[
@item{@c{let loinc = 'http://loinc.org';} is a constant: a name for a value that every group can
use.}
@item{@c{src.status : code 1..1} says what kind of value the source element is and how many there
must be. Fewer or more is an error at the rule.}
@item{@c{default ('en')} is the value to use when the source has none.}
@item{@tt{first} takes only the first of a repeating element. @tt{last}, @tt{not_first},
@tt{not_last} and @tt{only_one} are the others; @tt{only_one} is an error when there are several.}
@item{@c{check (…)} must hold, or the run stops with an error naming the rule. @c{log (…)}
hands a value to the program that runs the map as the rule runs, for finding out what a map did
(@racket[current-fml-log]); the command line does not show it.}
@item{@c{src -> tgt: id, implicitRules;} is short for copying each named element across as it
is.}
@item{@c{tgt.name as n share} lets several rules fill in the same new part, where each would
otherwise add its own.}
@item{An element whose name is also a word of the language is written in backticks:
@c|{src.`group`}|.}]

@subsection{Two things that catch newcomers}

@itemlist[
@item{@bold{A code is not a coding.} A form answers a coded question with a small object, a system
and a code. A Patient's @tt{gender} holds only the code. Assign the object and the run stops, with
the part to use:
@verbatim|{
coding.map:9:5: fml transform error: Rule "gender": `tgt.gender` is a code, but the rule assigns an object {code}: assign its `code`, `(a.value.code)`
}|}
@item{@bold{@tt{as} inside FHIRPath.} In a rule, @tt{as} always names something, so FHIRPath's own
@tt{as} operator cannot be written inside the parentheses. Use the function form,
@c{(v.as(Quantity))}.}]

@subsection[#:tag "older"]{Maps written the older way}

Many published maps were written for an earlier version of the language. They leave the parentheses
off a condition (@c{where linkId = 'x'}), put @tt{where} before the name, start with @c{map "url" =
"name"} in place of the header lines, or hold a table of code translations in a @tt{conceptmap}
block. All of these are read and run. The check points out the ones the specification no longer
has, as warnings:

@verbatim|{
$ raco fml check old.map
old.map:8:20: fml check warning: `where linkId = 'gender'`: the specification writes the expression in parentheses — `where (linkId = 'gender')`
 8 |   src.item as item where linkId = 'gender' -> tgt.gender = (item.answer.value.code) "gender";
   |                    ^^^^^^^^^^^^^^^^^^^^^^^
old.map: 1 finding
}|

@section[#:tag "checking"]{Checking a map}

A map can be wrong in two ways: it can disagree with the shapes it converts between, or it can
produce a result that is not valid. @tt{raco fml check} looks for both.

@subsection{Against the two shapes}

With no data at all, the check reads the definitions of the source and target types and reports, at
the rule:

@itemlist[
@item{an element the type does not have, with the nearest name:
@c|{`subjct` is not an element of QuestionnaireResponse (did you mean `subject`?)}|;}
@item{a value of a kind the element cannot hold;}
@item{a call to a group that does not exist, or with the wrong number of inputs;}
@item{a required target element that no rule writes. This one is a warning, since another map may
fill it in:
@verbatim|{
$ raco fml check nocode.map
nocode.map:7:1: fml check warning: Observation.code is required (min 1) but no rule of BpIntake assigns it
 7 | group BpIntake(source src : QuestionnaireResponse, target tgt : Observation) {
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
nocode.map: 1 finding
}|}
@item{a way of writing the specification no longer has (@secref["older"]), also a warning.}]

The command ends with status 1 when there is an error and 0 when there are only warnings. What the
check cannot decide, it does not report: a type whose definition it cannot find is left alone.

@subsection{On examples}

Give the check some source data with @c{--examples} (a file or a folder) and it converts each one
and @italic{validates} the result: it asks whether the result is correct FHIR of its type. A
problem is traced back to the rule that wrote the element.

Often the result must meet stricter rules than FHIR's own. Those are written as a
@italic{profile}: a statement of what a resource must contain in your setting. This one, in
@hyperlink["https://oloveluck.github.io/fsh/"]{FHIR Shorthand}, says an intake Patient must have a
gender and a birth date (@tt{fsh/intake.fsh}):

@verbatim{Profile: IntakePatient
Parent: Patient
Id: intake-patient
* gender 1..1
* birthDate 1..1}

Name the profile with @c{--profile} and the results are held to it. Here @tt{forms/} holds the
first example's form as @tt{ann.json}, and a second, @tt{bo.json}, where the gender was typed as
text rather than chosen from a list:

@verbatim|{
$ raco fml check qr2pat.map --examples forms/ --profile fsh/
qr2pat.map:8:49: fml validate error: forms/bo.json: Patient.gender: minimum required = 1, but found 0 (the rule here wrote nothing: its source matched, but `(item.answer.value.code)` was empty for {"answer":[{"valueString":"male"}],"linkId":"gender"})
 8 |   src.item as item where (linkId = 'gender') -> tgt.gender = (item.answer.value.code) "gender";
   |                                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
qr2pat.map: 1 finding, 2 examples validated (forms/ann.json, forms/bo.json) against IntakePatient (http://example.org/StructureDefinition/intake-patient)
}|

The message says which example failed, what is missing, which rule should have written it, and why
it did not: the rule asked for the answer's code, and a text answer has none. The last line says
what the results were held to.

@c{--profile} takes a profile as a JSON file, a folder or file of FHIR Shorthand, or the address of
a profile in an installed package. Repeat it for several.

@subsection{When a result is missing something}

The messages try to say why an element was not written:

@itemlist[
@item{@c|{(no rule of main writes `birthDate`)}|: the map has no rule for it. Add one.}
@item{@c{(the rule here wrote nothing: its source matched nothing in the input)}: the rule is
there, but the data had nothing for it to read.}
@item{@c|{its source matched 2 items, but `where (linkId = 'gender')` held for none (the linkId
there: gendr, birth — did you mean 'gendr'?)}|: the data was there and the condition turned it
away. The message lists the values the condition was tested against, and the nearest to the one
you wrote. Here the form spelt the question @tt{gendr}.}
@item{@c{was empty for …}, as above: the rule ran, and the value it asked for was not there.}]

One more thing can hide data from a map. A value that is not valid FHIR, such as a date written
@tt{01/05/1950}, cannot be read at all, so no rule sees it. The run says so:

@verbatim|{
$ raco fml qr2pat.map --transform cy.json
cy.json: warning: item[1].answer[0].valueDate ("01/05/1950") not read: not a valid value, or not an element of QuestionnaireResponse in FHIR R5
{"resourceType":"Patient","gender":"male"}
}|

@section[#:tag "logical"]{Shapes of your own: logical models}

The source of a map does not have to be FHIR. A @italic{logical model} describes a shape of your
own, such as a spreadsheet row, a message from an older system or a paper form, in the same way
FHIR describes its own types: a @italic{StructureDefinition} that lists the elements, what kind of
value each holds and how many there may be. Once the shape is described, a map can read it or write
it like any other.

@subsection[#:tag "intake"]{A worked example}

A clinic's front desk records blood-pressure readings on an intake form of its own design. The
clinic's record keeps them as Observations that must meet its screening profile. One project,
@tt{fhir-examples/examples/intake-maps}, holds the two shapes, the map between them and an example
form.

@bold{1. The shapes}, in FHIR Shorthand. The form is a logical model; the record is a profile of
Observation; the last block is one filled-in form.

@verbatim|{Logical: IntakeForm
Id: IntakeForm
* patientId 1..1 string "The patient's record id"
* measured 1..1 dateTime "When the reading was taken"
* systolic 1..1 integer "Systolic pressure, mm[Hg]"
* diastolic 1..1 integer "Diastolic pressure, mm[Hg]"

Profile: ScreeningBP
Parent: Observation
Id: ScreeningBP
* status = #final
* code = http://loinc.org#85354-9
* subject 1..1
* effective[x] 1..1
* effective[x] only dateTime
* component 2..*

Instance: intake-1
InstanceOf: IntakeForm
Usage: #example
* patientId = "p1"
* measured = "2024-03-01T09:00:00Z"
* systolic = 120
* diastolic = 80}|

@bold{2. The map.} Its @tt{uses} lines name the two shapes by their addresses. Each rule reads one
element of the form and writes the part of the Observation it feeds.

@verbatim|{/// url = 'http://example.org/intake/StructureMap/IntakeToBP'
/// name = 'IntakeToBP'
/// title = 'Intake form to screening blood pressure'

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

@bold{3. Check everything at once.} @tt{raco fsh check}, the FHIR Shorthand tool's check, also
checks every map in the project against the shapes beside it, converts the example form and
validates the result against the profile:

@verbatim{
$ raco fsh check examples/intake-maps
fsh check: 1 example, 0 with errors; 0 errors, 0 warnings
  canonical: http://example.org/intake (…/examples/intake-maps/sushi-config.yaml)
}

Misspell an element, or leave out the rule for @tt{code}, and it says so at the line.

@subsection{From the command line}

@tt{raco fml} needs to be told where your shapes are defined, since they are not part of FHIR.
@c{--structures} names a file or a folder of StructureDefinitions as JSON. In the project above,
@c{raco fsh .} writes them to @tt{fsh-generated/}:

@verbatim|{
$ raco fsh .
  StructureDefinition-IntakeForm.json
  StructureDefinition-ScreeningBP.json
  IntakeForm-instance.json
  StructureMap-instance.json
fsh: wrote 4 artifact(s) to ./fsh-generated
$ raco fml IntakeToBP.map --transform form.json --structures fsh-generated
{"resourceType":"Observation","status":"final","code":{"coding":[{"system":"http://loinc.org","code":"85354-9","display":"Blood pressure panel"}]},"subject":{"reference":"Patient/p1"},"effectiveDateTime":"2024-03-01T09:00:00Z","component":[ … ]}
}|

The data says which shape it is in the same place a FHIR resource does:

@verbatim|{{"resourceType": "IntakeForm", "patientId": "p1", "measured": "2024-03-01T09:00:00Z",
 "systolic": 120, "diastolic": 80}}|

@c{raco fml check} takes @c{--structures} too. Without it, a map over shapes the check cannot find
gets no findings, because nothing can be decided about them.

@subsection{As one package}

@c{raco fsh examples/intake-maps --pack intake.tgz} packs both shapes and the map into one FHIR
package, the file format FHIR definitions are published in. (It also writes the compiled files to
the project's @tt{fsh-generated/} folder.) A program can then use the map by its address alone:

@racketblock[
(require fml/registry fhir/sd/sd-index fhir/spec-package)
(parameterize ([current-sd-packages (list "intake.tgz" (spec-package-path "hl7.fhir.r5.core.tgz"))])
  (define to-bp (structure-map "http://example.org/intake/StructureMap/IntakeToBP"))
  (to-bp form))                                (code:comment "the Observation")
]

Over HTTP, a server generated from that package serves the same map as
@tt{StructureMap/$transform}. The test that does all of this is
@tt{fhir-examples/tests/intake-maps-test.rkt}.

@section[#:tag "structuremap"]{The StructureMap, and back to text}

A @italic{StructureMap} is the FHIR resource a map compiles to: the same rules, as data rather
than text. It is what a FHIR server stores and what tools pass to each other. You write and read
the text; the StructureMap is made from it when needed.

@itemlist[
@item{@c{raco fml map.map} prints the StructureMap as JSON.}
@item{A StructureMap @tt{.json} file can be given wherever a @tt{.map} file can: to run it, or to
check it.}
@item{@c{raco fml --decompile StructureMap.json} writes a StructureMap as text, so a map you
received as a resource can be read and edited:}]

@verbatim|{
$ raco fml qr2pat.map > sm.json
$ raco fml --decompile sm.json
/// url = 'http://example.org/StructureMap/qr2patient'
/// name = 'QR2Patient'
/// status = 'draft'

uses "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse" as source
uses "http://hl7.org/fhir/StructureDefinition/Patient" as target

group main(source src : QuestionnaireResponse, target tgt : Patient) {
  src.item as item where (linkId = 'gender') -> tgt.gender = (item.answer.value.code) "gender";
  src.item as item where (linkId = 'birth') then {
    item.answer as a -> tgt.birthDate = (a.value) "birthDate";
  } "birth";
}
}|

The text comes back in the language as the specification writes it today: the header lines,
conditions in parentheses, rule names in double quotes, one rule to a line. Comments and layout are
not part of a StructureMap, so they do not come back. Two things a StructureMap can hold have no
agreed way to be written: a table of code translations inside the map, and a shared target named
by a rule. A map with either is written in the older form for that part, under a comment that says
so.

@section{Running it}

@subsection{Command line}

This table is every form of the command.

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @c{raco fml map.map} "the StructureMap, as JSON")
      (list @c{raco fml map.map --transform src.json} "run the map over one piece of data; print the result")
      (list @c{  … --structures sds/} "where the definitions of your own shapes are (a file or a folder)")
      (list @c{  … --indent} "print the result over several lines")
      (list @c{raco fml check map.map} "the map against its shapes")
      (list @c{  … --structures sds/} "as above")
      (list @c{  … --examples ex/} "convert each example and validate the result")
      (list @c{  … --profile sd.json} "hold the results to a profile: a JSON file, FHIR Shorthand, or an address; repeat it")
      (list @c{raco fml --decompile sm.json} "a StructureMap resource as text")
      (list @c{raco fml --parse-corpus dir/} @elem{read every @tt{.map} under a folder and write each back; for testing the tool on a collection})
      (list @c{raco fml --tck} "the test collections, with the scores they must not fall below"))]

@c{--format text|json|outcome} chooses how problems are reported: as text for a person, or as JSON
or a FHIR OperationOutcome for a program. The command ends with status 0 when all is well, 1 on an
error in a map or a fall in a test score, and 2 when it was called wrongly or a file could not be
read. A file is read the same with or without a @tt{#lang fml} first line.

Giving a map data of the wrong type is refused before any rule runs:

@verbatim|{
$ raco fml qr2pat.map --transform obs.json
qr2pat.map:7:1: fml transform error: the source is of type Observation, but group main takes src : QuestionnaireResponse
}|

@subsection[#:tag "module"]{As a module, from Racket}

Put @tt{#lang fml} on the first line of a map and the file is a Racket module, so a program can
require it and call it. The map is checked when the program is built: a misspelt element stops the
build, at its line.

@racketblock[
(require json "qr2pat.map")
(define (read-file f) (call-with-input-file f read-json))
(transform (read-file "qr.json"))
(code:comment "'#hash((birthDate . \"1980-02-03\") (gender . \"female\") (resourceType . \"Patient\"))")
(hash-ref structuremap 'url)        (code:comment "\"http://example.org/StructureMap/qr2patient\"")
]

@racket[transform] is the map as a function: FHIR JSON in, FHIR JSON out. The module also provides
@racket[structuremap], the StructureMap, and @racket[fml], the map as parsed. Running the file
itself (@c{racket qr2pat.map}) prints the StructureMap.

@racket[transform] takes three options:

@itemlist[
@item{@racket[#:validate] checks the result and raises an error that names the rule, as
@c{raco fml check --examples} does. Give it @racket[#t] for FHIR's own rules, or a profile: its
address, a folder of FHIR Shorthand, a StructureDefinition, or a list of these. With the profile
and the text-answer form of @secref["checking"], @racket[(transform (read-file "bo.json") #:validate "fsh/")]
raises:
@verbatim|{
the map's Patient is invalid against IntakePatient (http://example.org/StructureDefinition/intake-patient) (FHIR R5 (5.0.0)):
  qr2pat.map:9:49: fml validate error: Patient.gender: minimum required = 1, but found 0 (the rule here wrote nothing: its source matched, but `(item.answer.value.code)` was empty for {"answer":[{"valueString":"male"}],"linkId":"gender"})
}|
The error is an @tt{exn:fail:operation}, the same kind @tt{ensure-valid} raises, so one handler
catches both.}
@item{@racket[#:structures] gives the definitions of your own shapes (@secref["logical"]), as a
list of StructureDefinitions or a data source.}
@item{@racket[#:strict?] makes a source value that cannot be read an error. Without it, such a
value is a warning and the map runs without it.}]

A map module fits into a pipeline with the family's other tools. Here the result is validated
against a profile by its address, with @racket[~>] passing each step's result to the next:

@racketblock[
(require (only-in "qr2pat.map" [transform form->patient])
         fhir/operation (only-in fhir/conformance/operations ensure-valid))
(~> qr form->patient (ensure-valid #:profile "http://hl7.org/fhir/StructureDefinition/Patient"))
]

Requiring the module also registers the map under its @tt{url}, which the next section uses.

@subsection{By its address, or as a FHIR operation}

A program does not need the file to use a map: it can ask for it by its @tt{url}.
@racket[structure-map] finds the map among those registered in the program, then in the current
data source, then in the installed packages, and gives it back as a function that knows its source
and target types.

@racketblock[
(require fml/registry fml/operations fhir/operation
         (only-in fhir/conformance/operations ensure-valid))
(define to-patient (structure-map "http://example.org/StructureMap/qr2patient"))
(list (smap-source to-patient) (smap-target to-patient))   (code:comment "'(QuestionnaireResponse Patient)")
(to-patient qr)                                            (code:comment "FHIR JSON in, FHIR JSON out")
(ensure-valid (to-patient qr))                             (code:comment "convert, then validate")
((compose-maps a b) x)                                     (code:comment "b after a; refused unless a's target is b's source")
(apply-operation "StructureMap/$transform" qr #:srcMap fml-text)
(apply-operation "StructureMap/$transform" qr #:source "http://example.org/StructureMap/qr2patient")
]

@tt{StructureMap/$transform} is the operation FHIR defines for running a map, and what a FHIR
server offers over HTTP. It takes the map as text (@tt{srcMap}), by address (@tt{source}) or as a
StructureMap resource (@tt{sourceMap}).

@subsection{The pieces, from Racket}

These are the steps the command line and the module form do for you, one at a time: read the text,
compile it, check it, run it, write it back.

@racketblock[
(require fml/parser/parse fml/structuremap fml/print fml/transform fml/check fhir/result/result
         fhir/r5/schemas fhir/serialize/from-json)             (code:comment "the engine needs FHIR's types loaded")
(define m (ok-value (parse-fml text #:source "qr2pat.map")))
(fml->structuremap m)                                           (code:comment "the StructureMap")
(check-map m)                                                   (code:comment "the findings; '() when there are none")
(run-map m (ok-value (from-json qr #:lenient? #t)))            (code:comment "typed source in, target JSON out")
(structuremap->fml sm)                                          (code:comment "a StructureMap back to the parsed map")
(fml->text m)                                                   (code:comment "the parsed map as text")
(specification-notes m)                                         (code:comment "the older forms the text used, as warnings")
]

@section[#:tag "runs"]{How a map runs}

This section says what happens, in order, when a map is run. You need it when a result surprises
you.

A run starts at the first group, with the data you gave as its source and an empty target.

@itemlist[
@item{@bold{A rule runs once for every match of its source.} The matches are the values of the
element, narrowed by the list mode (@tt{first}, @tt{last}, @tt{not_first}, @tt{not_last},
@tt{only_one}) and by @tt{where}. A rule with several sources runs for every combination. A rule
whose source matches nothing does nothing.}
@item{@bold{Then the checks.} The stated number of values (@tt{1..1}) and the @tt{check} condition
must hold, or the run stops with an error naming the rule. @tt{log} sends its value to
@racket[current-fml-log].}
@item{@bold{Then the targets, in order}, then the group calls after @tt{then}, then the rules
inside.}
@item{@bold{A target with no value} (@c{tgt.component as c}) makes the element.}
@item{@bold{A rule with one source, one target and no value} (@c{src.a -> tgt.a;}) carries the
value over. A simple value is copied and converted to the kind the target element holds:
@racket["12345"] into an integer element is @racket[12345]. A structured value is handed to the
group marked @c{<<types>>} or @c{<<type+>>} whose inputs are of those types, or copied whole when
there is none.}
@item{@bold{A group that @tt{extends} another} runs that group's rules first.}
@item{@bold{@tt{translate}} looks a code up in a table of code translations: a @tt{conceptmap}
block in the map, or a ConceptMap resource the terminology service can find.}
@item{@bold{A write takes one value.} When the FHIRPath on the right finds several, the first is
written.}]

An error while running names the rule and shows its line. For an unknown transform it lists the
ones there are. A call to a group that does not exist is the exception: it names the groups there
are, but comes without a line. The check catches it beforehand.

@subsection[#:tag "transforms"]{The built-in helpers (transforms)}

This table is every transform a rule can use on the right of the arrow.

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @bold{transform} @bold{what it gives})
      (list @c{create('Type')} "a new, empty value of that type")
      (list @c{copy(v)} "the value, unchanged")
      (list @c{cast(v, 'type')} "the value converted to another simple type; an error when it cannot be")
      (list @c{evaluate(v, expression)} @elem{what a FHIRPath expression finds, starting from @tt{v}; @c{(expression)} alone is the usual way to write one})
      (list @c{truncate(v, n)} "the first n characters of a text")
      (list @c{append(a, b, …)} "the texts joined")
      (list @c{reference(v)} @elem{the text @tt{Type/id} that refers to a resource: one read from the source, or one the map made})
      (list @c{translate(v, map, 'code')} @elem{what a table of code translations gives for this code. The table is @c{'#name'} for a @tt{conceptmap} block in the map, else the address of a ConceptMap. The last input says what to give back: @tt{code}, @tt{system}, @tt{display}, @tt{Coding} or @tt{CodeableConcept}})
      (list @c{uuid()} "a new unique identifier")
      (list @c{c(system, code, display)} "a Coding: one code from one code system; the display is optional")
      (list @c{cc(system, code, display)} @elem{a CodeableConcept: a coded concept; the display is optional. @c{cc(text)} gives one with only a text})
      (list @c{qty(value, unit, system, code)} @elem{a Quantity: a number with its unit; the system and code are optional. @c{qty('5.4 mg')} reads one from a text})
      (list @c{id(system, value)} "an Identifier; a third input is its type, as text")
      (list @c{cp(system, value)} @elem{a ContactPoint: a phone number or an e-mail address. @c{cp(value)} leaves the system out}))]

@section{In an editor, and for an AI agent}

@itemlist[
@item{@bold{In an editor.} The family's language server,
@hyperlink["https://oloveluck.github.io/fhir/fhir-lsp/index.html"]{fhir-lsp}, underlines a map's
mistakes as you type (the same findings as the check), lists its groups, shows what a name stands
for, jumps from a name to where it is declared (from an @tt{imports} address to the map that has
it) and completes names and keywords. It treats the maps in the same folder as one project.}
@item{@bold{For an agent.} In an
@hyperlink["https://oloveluck.github.io/fhir/fhir-mcp/index.html"]{fhir-mcp} workspace a map is
checked as it is written. @tt{run} converts every piece of data of the map's source type and
validates each result; @tt{explain} converts one and traces each problem to the rule that wrote
the element. The repository's @tt{guide.md} is the language written for an agent to read (fhir-mcp
serves it as @tt{learn://fml}), and @tt{.claude/skills/fml} is the same workflow for an agent using
the command line.}]

For someone building on these: the editor support is the module @tt{fml/support} (a
@tt{fhir/lang/support} value, which the language server loads by name), and the workspace support
is @racketmodname[fml/workspace].

@section{Limits}

@itemlist[
@item{@bold{Three transforms are missing.} @tt{escape}, @tt{dateOp} and @tt{pointer} are not
implemented. The check does not flag them; the rule fails when it runs, with the list of the ones
there are.}
@item{@bold{One version of FHIR.} Maps run over FHIR R5. Data from another version is read as far
as it fits, and the run names each value it could not read. An element R5 dropped (R4's
@tt{Composition.code}) is not an element here.}
@item{@bold{JSON only.} Data is read and written as JSON. Maps that produce XML documents, such as
CDA, are not supported.}
@item{@bold{Imports that cannot be found are skipped.} An @tt{imports} address no registered map,
data source or package answers is ignored without a message. The check then cannot tell whether a
group the map calls exists, and says nothing; the call fails when the map runs.}
@item{@bold{Only @tt{share} among the target list modes has an effect.} Of the words that can
follow a target (@tt{first}, @tt{last}, @tt{share}, @tt{single}), only @tt{share} changes what a
run does. The others are kept in the StructureMap and otherwise ignored.}
@item{@bold{@tt{as} in FHIRPath} must be written as the function @c{.as(Type)} inside a map.}
@item{@bold{What cannot be decided is not reported.} The check says nothing about a type whose
definition it cannot find, so a clean check of a map over your own shapes means something only
when their definitions were given.}]

@section[#:tag "words"]{Words used here}

@itemlist[
@item{@bold{Map}: a text file of rules that convert data of one shape into another.}
@item{@bold{Source}, @bold{target}: the data a map reads, and the data it produces.}
@item{@bold{Rule}: one line of a map: what to take from the source, and where to write it in the
target.}
@item{@bold{Group}: a named list of rules with its sources and targets. A run starts at the first.}
@item{@bold{Transform}: a built-in helper that makes a value, such as @tt{cc} or @tt{qty}.}
@item{@bold{Resource}: one record in FHIR, of a named type: a Patient, an Observation.
@bold{Element}: one named part of it, such as a Patient's @tt{birthDate}.}
@item{@bold{QuestionnaireResponse}: the FHIR resource that holds the answers to a form.}
@item{@bold{StructureMap}: the FHIR resource a map compiles to; the form in which tools store and
exchange maps.}
@item{@bold{StructureDefinition}: the FHIR resource that describes a shape: its elements, their
kinds and how many of each there may be.}
@item{@bold{Logical model}: a StructureDefinition for a shape of your own that is not a FHIR
resource.}
@item{@bold{Profile}: a StructureDefinition that narrows a FHIR type for one setting: this element
is required here, that one must have this code.}
@item{@bold{Validate}: to check a piece of data against the definition of its type and any
profiles.}
@item{@bold{Canonical} (url): the address a definition or a map is known by. It identifies; it need
not be a working web address.}
@item{@bold{FHIRPath}: FHIR's language for picking values out of data: @c{item.answer.value}.}
@item{@bold{Choice element}: an element written under different names for different kinds of value,
such as @tt{valueString} or @tt{valueQuantity}. In a map, and in FHIR's own definitions, it is just
@tt{value}.}
@item{@bold{Coding}, @bold{CodeableConcept}, @bold{Quantity}: FHIR's values for a code from a code
system, a coded concept, and a number with a unit.}
@item{@bold{ConceptMap}: a table that translates the codes of one code system into another's.}
@item{@bold{Package}: a file of published FHIR definitions (@tt{.tgz}).}
@item{@bold{FHIR Shorthand} (FSH): the language profiles and logical models are usually written
in. It is the @hyperlink["https://oloveluck.github.io/fsh/"]{fsh} package of this family.}
@item{@bold{R4}, @bold{R5}: versions of FHIR. R5 is the newer.}]

@section{How it is tested}

Against maps other people published, not only its own examples:

@itemlist[
@item{HL7's mapping test cases: all 6 that can be run here give the expected result, and all 15
maps are read. Four more are skipped: they produce XML, or take their map as JSON.}
@item{The maps of matchbox, another engine for this language: all 43 are read, and the 17 that can
be run on their own are.}
@item{HL7's mapping tutorial, thirteen steps over shapes of its own: all 37 results are as
expected, both from the text and from the StructureMaps this package makes from it. Two result
files in that collection are set aside, because the other engine's own tests do not expect them:
one belongs to a map it never runs, and one to a run it expects to fail, as it does here.}
@item{Every one of those 58 maps survives the trip to a StructureMap and back, and comes out of
@c{--decompile} as text that means the same map. So do the 25 StructureMap resources matchbox
ships.}
@item{The check finds nothing wrong in 55 of the 58. The other three have real mistakes: two use
elements FHIR R5 dropped, one misspells a name.}
@item{The 332 maps HL7 wrote to convert between FHIR R3 and R4 are all read and written back.}]

The repository's @tt{TESTING.md} has the full table and names every difference. To run the tests
yourself, install the family from source as the
@hyperlink["https://oloveluck.github.io/fhir/"]{front page} describes; then, in this repository,
@c{raco test .} runs the unit tests and @c{raco fml --tck} runs the collections above.

@section{Reference}

@subsection[#:style 'unnumbered]{@hash-lang[] @tt{fml}}
@defmodulelang[fml]

Provides @racket[fml] (the parsed map), @racket[structuremap] (the StructureMap as a jsexpr) and
@racket[(transform src #:structures sds #:strict? strict? #:validate spec)], as in
@secref["module"]. A @racket[src] that is not a resource raises an argument error. The map is
checked against its structures when the module is compiled.

@subsection[#:style 'unnumbered]{@tt{fml/parser/parse}}
@defmodule[fml/parser/parse]

@defproc[(parse-fml [src string?] [#:source source (or/c string? path? #f) #f]) (or/c ok? err?)]{
@racket[(ok fml-map)], or @racket[(err fml-error)] carrying a diagnostic positioned in
@racket[source]. The AST (@tt{fml/parser/ast}) is prefab: @racket[fml-map] (url, name, metadata,
uses, imports, constants, groups, conceptmaps), @racket[fml-group], @racket[fml-rule] (sources, targets,
dependents, nested rules), @racket[fml-source], @racket[fml-target] (context @racket[#f] for a bare
invocation), @racket[fml-conceptmap] (name, prefixes, mappings); a transform is
@racket[fml-literal], @racket[fml-fhirpath] (a FHIRPath AST), @racket[fml-invoke] or
@racket[fml-var].}

@defproc[(specification-notes [m fml-map?]) list?]{
What the text @racket[m] was parsed from wrote that the specified language does not have, as
warnings (diagnostics) at the form: @tt{where} or @tt{check} without parentheses, @tt{where} before
the alias, @c{share <id>}, a @tt{conceptmap} block, a @c{///} line that states no element. Such
maps are read and run; @racket[check-map] reports these with its own findings. Empty for a map not
parsed from text.}

@subsection[#:style 'unnumbered]{@tt{fml/structuremap}}
@defmodule[fml/structuremap]

@defproc[(fml->structuremap [m fml-map?]) hash?]{
The StructureMap resource, the canonical compiled form of a mapping: the metadata lines as its
elements, status @racket["draft"] when none is stated (the specification's default), unnamed rules
named @tt{rule-N}, @tt{conceptmap} blocks as contained ConceptMaps.}

@defproc[(structuremap->fml [sm hash?]) fml-map?]{
The inverse: a StructureMap resource read into the AST (embedded FHIRPath parsed, typed literal
parameters, R4 @tt{equivalence} as well as R5 @tt{relationship} in contained ConceptMaps). The
round trip returns the parsed AST up to the conceptmap prefixes and the @tt{rule-N} names; every
corpus map is checked.}

@subsection[#:style 'unnumbered]{@tt{fml/print}}
@defmodule[fml/print]

@defproc[(fml->text [m fml-map?]) string?]{
The map as text, in the language the specification defines and nothing else: the header as
@c{/// url = '…'} and @c{/// name = '…'} metadata lines, urls and rule names in double quotes,
the expressions of @tt{where}, @tt{check}, @tt{log} and @tt{default} in parentheses, one rule to a
line. With @racket[structuremap->fml] it reads any StructureMap resource as the language
(@c{raco fml --decompile}). A contained ConceptMap and a target's @tt{listRuleId} have no syntax
in the specified language: a map holding one is written with the reference implementation's
@tt{conceptmap} block (under a comment that says so) and @c{share <id>}. Over every corpus map and
every StructureMap resource of the matchbox corpus, the text parses, compiles to the same
StructureMap, and prints the same again.}

@subsection[#:style 'unnumbered]{@tt{fml/transform}}
@defmodule[fml/transform]

@defproc[(run-map [m fml-map?] [source fhir?]) hash?]{
Runs the map's first group over a typed source node and returns the target resource as a jsexpr.
Targets are shaped by the registered type schema (a repeating element appends, a @tt{value[x]} takes
the key of the value's type, a primitive element coerces what is assigned to it). A failure while
running a rule (a @tt{check} that does not hold, @tt{only_one} over several elements, a @tt{cast}
that cannot convert, an object assigned to a primitive element, an unknown transform) raises
@tt{exn:fail:diagnostic} positioned at the rule, as a parse error is. A call to a group that does
not exist raises a plain @racket[exn:fail], without a position. A source that is not of the
first group's source type (nor of a type derived from it) is refused the same way, positioned at
the group, before any rule runs.}

@defproc[(uses-types [m fml-map?]) hash?]{
The map's @tt{uses} resolved to type symbols, keyed by alias and type name; a structure the towers
lack is resolved as a StructureDefinition (the current data source, then the packages) and
registered. Call it before reading a source instance of a logical model.}

@defparam[current-fml-log log (-> string? any)]{
Where a rule's @tt{log(…)} goes; nowhere by default.}

@defparam[current-empty-targets table (or/c #f (and/c hash? (not/c immutable?)))]{
When a mutable @racket[hasheq], a run records in it each target whose value expression came out
empty, with the source value the rule read. @racket[validate-result] uses it to say why an element
was not written. @racket[#f] by default.}

@defproc[(register-structure-map! [m fml-map?]) void?]{Resolvable by its url in this process.}
@defproc[(registered-structure-maps) (listof string?)]{The urls registered, sorted.}
@defproc[(resolve-structure-map [url string?]) (or/c fml-map? #f)]{
Registered maps, then StructureMaps in @racket[current-data-source], then the resources modules
registered, then the packages; a @c{|version} is ignored.}
@defproc[(map-imports [m fml-map?]) (listof fml-map?)]{The map and its imports, transitively; one
that resolves nowhere is skipped.}

@subsection[#:style 'unnumbered]{@tt{fml/registry}}
@defmodule[fml/registry]

@defproc[(structure-map [ref (or/c string? fml-map? hash?)] [#:structures structures (or/c #f data-source? (listof hash?)) #f]) smap?]{
A map as a typed procedure: by canonical (resolved as @racket[resolve-structure-map] does), a parsed
map, or a StructureMap resource. @racket[structures] are the StructureDefinitions of its logical
models. Applying it: @racket[(m src)], FHIR JSON or a typed node in, FHIR JSON out.}
@defproc[(smap? [v any/c]) boolean?]{A map as a procedure.}
@defproc[(smap-source [m smap?]) (or/c symbol? #f)]{The entry group's source structure type.}
@defproc[(smap-target [m smap?]) (or/c symbol? #f)]{Its target structure type.}
@defproc[(smap-ast [m smap?]) fml-map?]{The AST.}
@defproc[(compose-maps [a smap?] [b smap?]) smap?]{
@racket[b] after @racket[a]; an error unless @racket[a]'s target structure is @racket[b]'s source.}

@subsection[#:style 'unnumbered]{@tt{fml/check}}
@defmodule[fml/check]

@defproc[(check-map [m fml-map?] [#:source source (or/c string? path? #f) #f]) (listof diagnostic?)]{
The static findings, each a @tt{fhir/diagnostic} at its rule, source or target, followed by the
@racket[specification-notes].}
@defproc[(check-map/examples [m fml-map?] [examples (listof hash?)]
                             [#:profiles profiles (listof hash?) '()] [#:source source (or/c string? path? #f) #f]
                             [#:names names (or/c #f (listof string?)) #f])
         (listof diagnostic?)]{
Each example transformed and validated (fhir-validation, loaded on demand) against its base type and
@racket[profiles]; an issue is positioned at the rule that wrote the element. @racket[names] labels
each example in its findings (the command line gives the file names).}
@defproc[(validate-result [m fml-map?] [result any/c]
                          [#:profiles profiles (listof hash?) '()] [#:source source (or/c string? path? #f) #f]
                          [#:label label string? ""])
         (listof diagnostic?)]{
One result of the map validated and each issue traced: to the rule that wrote the element, with
why it wrote nothing when it did not, or the note that no rule of the group writes it. What
@racket[check-map/examples] and the module's @racket[#:validate] use.}
@defproc[(profiles-from [spec (or/c string? path? hash?)]) (listof hash?)]{
The StructureDefinitions a @c{--profile} argument names: a JSON file, an FSH project (a directory
or a @tt{.fsh} file: its profiles, with its other definitions registered), or a canonical the
packages define.}

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
subject is the content, @racket[#:srcMap] the FML text. The content is read leniently into the R5
tower; @tt{sourceMap} is a StructureMap resource and @tt{source} a canonical (registered maps, the
data source, the registry, the packages). A rule that fails at run time comes back as an
OperationOutcome whose issue carries the rule's file and line (the standard
@tt{operationoutcome-issue-line} extensions).}

@subsection[#:style 'unnumbered]{@tt{fml/workspace}}
@defmodule[fml/workspace]

@defthing[workspace-support any/c]{The @tt{fhir/lang/workspace} support fhir-mcp loads for FML.}
@defthing[guide string?]{The repository's @tt{guide.md}: the language for agents (fhir-mcp's
@tt{learn://fml}).}
