# fml — FHIR Mapping Language

> Part of [fhir](https://github.com/oloveluck/fhir), a FHIR implementation designed to be used by
> AI agents: one line per problem, at its place, with what would fix it; a guide written for an
> agent to read; the same language inside the `fhir-mcp` workspace.

Health data often arrives in one shape and is needed in another: a filled-in questionnaire that
should become a Patient, a form of your own design that should become an Observation. The
[FHIR Mapping Language](https://hl7.org/fhir/R5/mapping-language.html) is the standard way to write
that conversion down as a *map*: a list of rules, each saying "take this from the source, write
that to the target". This package checks a map against the two shapes, runs it over your data, and
reports a mistake at its line.

```
$ cat qr2pat.map
/// url = 'http://example.org/StructureMap/qr2patient'
/// name = 'QR2Patient'

uses "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse" as source
uses "http://hl7.org/fhir/StructureDefinition/Patient" as target

group main(source src : QuestionnaireResponse, target tgt : Patient) {
  src.item as item where (linkId = 'gender') -> tgt.gender = (item.answer.value.code) "gender";
}

$ fml qr2pat.map --transform qr.json
{"resourceType":"Patient","gender":"female"}
```

## Install

Build the [fhir family's tools](https://github.com/oloveluck/fhir#install) once; `fml` is one of
them. With Racket, `raco fml` is the same command.

## Documentation

**[oloveluck.github.io/fml](https://oloveluck.github.io/fml/)**: a first example explained line by
line, writing and checking a map, shapes of your own, maps in a program, and the reference.

- For agents: [guide.md](guide.md), a cheat sheet and recipes, each one tested; and the
  [fml skill](.claude/skills/fml/SKILL.md).
- How well it works, against HL7's test cases and tutorial: [TESTING.md](TESTING.md).
- How it is built: [CLAUDE.md](CLAUDE.md).

License: MIT.
