# fml — FHIR Mapping Language

The [FHIR Mapping Language](https://hl7.org/fhir/R5/mapping-language.html): compile a map to a
StructureMap, run it over a resource, and check it against the structures it maps between, with each
finding on its line. [TESTING.md](TESTING.md) has the conformance results.

Part of the [fhir](https://github.com/oloveluck/fhir) family of FHIR languages and tools.

## Command line

[Build the family's tools](https://github.com/oloveluck/fhir#getting-the-tools) once and put
`dist/bin` on your `PATH`; they run without Racket. With Racket installed, `raco fml` is the same.

```sh
fml qr2pat.map                                   # the StructureMap, as JSON
fml qr2pat.map --transform qr.json               # run the map over an R5 resource
fml map.map --transform source.json --structures sds/   # over logical models, given their definitions
fml check map.map --examples ex/                 # check the map; transform and validate the examples
fml --decompile StructureMap.json                # a StructureMap resource as the mapping language
```

```
$ fml qr2pat.map --transform qr.json
{"gender":"female","resourceType":"Patient"}
```

Errors and findings point at their line, with exit status 1.

## In Racket

A map that starts with `#lang fml` is a module; requiring it registers its StructureMap by canonical:

```racket
#lang racket/base
(require json "qr2pat.map")
(transform (call-with-input-file "qr.json" read-json))   ; '#hash((gender . "female") (resourceType . "Patient"))
```

`(structure-map "http://…/StructureMap/x")` (`fml/registry`) resolves any map by canonical into a
procedure, and `StructureMap/$transform` is an operation.

## Develop

```sh
raco pkg install --link --auto ../fhir/fhir ../fhir/fhir-spec ../fhirpath ../vcl ../fhir/fhir-tx .   # repos side by side
raco test .
```

Reference: [oloveluck.github.io/fml](https://oloveluck.github.io/fml/). The language for agents,
with a checked cheat sheet and the common problems: [guide.md](guide.md); the workflow without MCP:
[.claude/skills/fml](.claude/skills/fml/SKILL.md). Architecture: `CLAUDE.md`.
License: MIT.
