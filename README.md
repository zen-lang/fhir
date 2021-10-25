# Convert FHIR to zen-lang

![example workflow](https://github.com/zen-lang/fhir/actions/workflows/main/badge.svg)


Documentation generator for zen


Get FHIR IGs as input produce zen bundle as output.

* SD => schemas
* Aidbox transformation?
* VS => validation? & load into Concept (ndjson)
* SP + algorythm => search params 
* Generate API Constructor


Aidbox + zen bundle => Conformant FHIR Server


## Structure Definitions


npm module (package name, index.json, resources )


loader(npm, custom?) => inter-1 => inter-2 (context dependent) => zen => export (zen npm, zen sa, load in memory)

Unique identificator of resource is uri

1. Load into memory (URL)
- strip unimportant attributes
- fix bugs like type of primitives
- min/max => collection & requir*
- type of attribute
- elements (id,path) =>  recursive structure
- union types as aidbox
- first class extensions
- * non-first class (mode?) => { extension: [] } problem with references
- * first class primitive extensions: primitive ext => _given: {fcext: }
- * non-first class => propertyPattern (keyPattern) :_ {:type zen/any}
- reference?

2. Walk all resources with parents & enrich with parents (collection?, type) 
- deps

3. Generate zen schemas 
- recursive schemas (Questionnaire, CodeSystem )


4. Generate zen module



JSON:

given:
_givne {extensions: []}






