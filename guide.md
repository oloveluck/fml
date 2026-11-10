# FHIR Mapping Language (FML)

FML converts data from one structure to another: a QuestionnaireResponse into a Patient, a form of
your own design (a logical model) into FHIR resources, one FHIR version into another. A map names its
source and target structures and lists rules, each reading source elements and writing target
elements. It compiles to a StructureMap resource, the standard form, and runs over one source
resource at a time.

This guide is for writing FML here: the language in brief, the ways to run and check a map, and
what to do when something goes wrong. The specification is the reference for the language itself.

## Official references
- FHIR Mapping Language: https://hl7.org/fhir/R5/mapping-language.html
- The StructureMap resource it compiles to: https://hl7.org/fhir/R5/structuremap.html
- The mapping tutorial: https://hl7.org/fhir/R5/mapping-tutorial.html

## Cheat sheet
```fml
map "http://example.org/StructureMap/qr2patient" = "QR2Patient"
uses "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse" as source
uses "http://hl7.org/fhir/StructureDefinition/Patient" as target

group main(source src : QuestionnaireResponse, target tgt : Patient) {
  src.item as item where linkId = 'gender' -> tgt.gender = (item.answer.value.code);
  src.item as item where linkId = 'birth' then {
    item.answer as a -> tgt.birthDate = (a.value);
  };
}
```
A rule reads `source -> target`: `src.item as item` binds each matching element, `where` filters
(FHIRPath, evaluated with the alias bound), `-> tgt.x = v` writes the target. `then { … }` nests
rules; `then g(a, b)` calls another group. A value is a variable, a literal, FHIRPath in
parentheses, or a transform: `create('Type')`, `copy`, `cast`, `evaluate`, `translate`, `uuid`,
`reference`, and the constructors `c` (Coding), `cc` (CodeableConcept), `qty` (Quantity), `id`,
`cp`, `append`. A choice element reads whichever variant is present (`ans.value`) and writes the
variant of the value's type (`tgt.effective = d` writes `effectiveDateTime` for a dateTime). A string
at the end of a rule names it. Over the QuestionnaireResponse below, the map gives
`{"resourceType": "Patient", "gender": "female", "birthDate": "1980-02-03"}`:
```json
{"resourceType": "QuestionnaireResponse", "status": "completed",
 "item": [{"linkId": "gender", "answer": [{"valueCoding": {"code": "female"}}]},
          {"linkId": "birth",  "answer": [{"valueDate": "1980-02-03"}]}]}
```

## Worked example
A blood-pressure reading from a form, as an Observation with a coded component:
```fml
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
}
```
It checks clean. Over a response with `subject` `Patient/p1` and a `systolic` item answered
`valueInteger: 120`, the Observation is `final`, coded `85354-9`, for `Patient/p1`, with one component
coded `8480-6` whose `valueQuantity` is 120 mm[Hg].

## Recipe: one text into several elements
A form's free-text name into `family` and `given`: FHIRPath in parentheses computes each part
(`split`, `first`, `last`, `replaceMatches`, `substring`):
```fml
map "http://example.org/StructureMap/name-split" = "NameSplit"
uses "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse" as source
uses "http://hl7.org/fhir/StructureDefinition/Patient" as target

group main(source src : QuestionnaireResponse, target tgt : Patient) {
  src.item as item where linkId = 'name' then {
    item.answer as a -> tgt.name as n then {
      a.value as full -> n.family = (full.split(' ').last()),
                         n.given = (full.split(' ').first()) "parts";
    } "name";
  };
}
```
An answer `valueString: "Ann Marie Ng"` gives `"name": [{"family": "Ng", "given": ["Ann"]}]`. An
assignment writes one value: a FHIRPath giving several (`split(' ').take(2)`) writes its first, so a
middle name is a rule of its own (`n.given = (full.split(' ')[1])`).

## Checking a map
The check holds a map against its two structures and reports, on the rule's line:
- an element the type does not have (`` `subjct` is not an element of QuestionnaireResponse (did you mean `subject`?)``);
- a value whose type cannot go into the element;
- a group call with the wrong number of arguments, or to a group that does not exist;
- a required target element no rule assigns (a warning: `Observation.code is required (min 1) but
  no rule of main assigns it`).

Given example sources, each is transformed and the result validated against the target (and a
profile), and each problem is traced to the rule that wrote the element. A rule that fails at run
time (a `check` that does not hold, `only_one` over several elements, a `cast` that does not
convert) is reported at its line too.

## Running it
**Command line.** `fml` (or `raco fml`):
```
fml map.map                                        # the StructureMap, as JSON
fml map.map --transform source.json                # run it over one (R5) resource; the target JSON
fml map.map --transform source.json --structures sds/   # over logical models: their StructureDefinitions
fml check map.map --structures sds/ --examples ex/ --profile profile.json   # check; transform and validate
```
`--profile` is a StructureDefinition file, an FSH project (a directory or a `.fsh` file: its
profiles, and its other definitions in context), or a canonical the packages define; a profile
applies to the results of its type.
A StructureMap `.json` is read wherever a `.map` is. Errors and findings are at their line, exit 1
on an error; `--format json` gives them as JSON.

**As a module.** A file that starts with `#lang fml` is a Racket module providing `transform`
(FHIR JSON in, FHIR JSON out; `#:structures` for logical models), `structuremap` and the AST.
Requiring it registers its StructureMap, so the map is found by its canonical. The file is the map
as written anywhere else, after one line: `#lang fml` then `map "http://…" = "intake"`, `uses …`,
the groups; another module says `(require "intake.fml")` and calls `(transform qr)`.
Without the `#lang fml` line, requiring the file fails with `default-load-handler: expected a
`module' declaration, but found something else`: add the line. `raco fml` reads the file with or
without it.
`(transform qr #:validate "fsh/")` holds the result to its base type and those profiles (`#t`: the
base type and what it claims; a canonical, an FSH project, an SD, or a list of them) and raises
with each problem at the rule that wrote the element — `ensure-valid` after the map can only name
the element:
```
the map's Patient is invalid against IntakePatient (http://example.org/StructureDefinition/intake-patient) (FHIR R5 (5.0.0)):
  intake.rkt:7:47: fml validate error: Patient.gender: code not in required value set …
  intake.rkt: fml validate error: Patient.birthDate: minimum required = 1, but found 0 (no rule of main writes `birthDate`)
```
When a rule for the element exists but its source matched nothing (the response had no birthDate
item), the problem is at that rule and says `(the rule here wrote nothing: its source matched
nothing in the input)`.

**By canonical, or as an operation.** `(structure-map "http://…/StructureMap/x")` (`fml/registry`)
is the map as a typed procedure, found among the maps registered in the process, the data source,
then the packages; `StructureMap/$transform` is the same as a FHIR operation, with the map as text
(`srcMap`), a canonical (`source`) or a resource (`sourceMap`).

**In a workspace** (fhir-mcp). A `.map` or `.fml` file is a StructureMap, checked against its
structures as it is written. `run` transforms every resource of the map's source type in the data
(or `over` a type or one resource) and validates each result. `explain` transforms one source
resource and traces each problem in the result to the rule that wrote it. Staging a run's results is
how a transform's output is written to the data. The `uses` structures resolve from the towers, the
workspace's FSH (a `Logical:` model, a profile), the data, then the packages.

## Logical models
A `uses` url FHIR does not define (a form of your own) is read as a StructureDefinition from the
data, the workspace or the packages, and registered as a type, so `src.patientId` navigates it.
Give the definitions with `--structures` on the command line or `#:structures` from Racket. A source
instance of a logical model names its type in `resourceType` (`{"resourceType": "TLeft", "a": …}`).

## Common problems
- `` `x` is not an element of T``: the element name is misspelt, or belongs to another version of
  FHIR (the engine targets R5: R4's `Composition.code`, `contributor` are gone).
- `fml warning: the source's item[0].answer[0].valueDate ("01/05/1950") not read`: the source holds a
  value that is not valid FHIR (a date written `01/05/1950`), or an element the R5 type lacks; the
  map never saw it, so whatever it would have written is missing. Fix the source, or convert it in
  the map from a string. `transform #:strict? #t` makes it an error; a workspace run lists it as
  `unread`.
- A target element is missing from the result: its rule's source matched nothing. Transform one
  source and look at which rules wrote what (`explain` in a workspace), or loosen the `where`.
- A required element is reported unassigned: add a rule that writes it, or a dependent group that
  receives the whole target.
- A result fails validation: the problem names the rule that wrote the element (when one did); a
  required element no rule writes is reported without a line.
- `unknown transform`: the error lists the supported ones; `escape`, `dateOp` and `pointer` are not
  implemented.
- `as` inside embedded FHIRPath: it is read as the target alias. Use `.as(Type)` there.
- `the source is of type Observation, but group BpIntake takes src : QuestionnaireResponse`: the
  map's entry group declares its source's type, and the resource given is another (a type derived
  from the declared one is accepted: a group taking `Resource` takes any resource).
