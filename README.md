# Table of contents

- [Zen FHIR format](#zen-fhir-format) 
  - [References](#references) 
  - [First-class extensions](#first-class-extensions)
  - [Polymorphics](#polymorphics)
- [Convert FHIR IG to zen-lang](#convert-fhir-ig-to-zen-lang)
  - [Artifacts](#artifacts)
  - [How to](#how-to)
    - [Convert FHIR IG](#convert-fhir-ig)
      - [Commands](#commands)
      - [Example](#example)
  

# Zen FHIR format

Zen FHIR format is FHIR json format for storage. Resources structure has 3 types of isomorphic transformations applied:

- References
- First-Class Extensions
- Polymorphics (choice of types)

## References:

In FHIR, references are represented as URI string. In most cases, systems are interested in discrete parts of references like resource id and type. For performance and accuracy reasons zen FHIR format suggests to store reference parts in separate fields. In FHIR there are three types of references - **absolute**, **relative** and **local**. By Zen FHIR format they're stored in different attributes.

- **Relative** (interpreted as a reference to a resource on the same server; triggers referential consistency check):
  
  **reference** is parsed into a pair of **`{id,resourceType}`** attributes
  
  ```yaml
  # FHIR
  subject:
    reference: "Patient/pt-1" 
  
  # Zen FHIR
  subject:
    resourceType: "Patient"
    id: "pt-1"
  ```

- **Absolute** (interpreted as a reference to an external resource;  no ref validation)

  **reference** is parsed into the **uri** attribute
  
  ```yaml
  # FHIR
  subject:
    reference: "http://external/fhir/Patient/pt-1" 
  
  # zen FHIR
  subject:
    uri: "http://external/fhir/Patient/pt-1"
  ```

- **Local** (interpreted as a local ref to contained resources)
  
  **reference** is parsed into a **localRef** attribute
  
  ```yaml
  # FHIR
  subject:
    reference: "#pt" 
  
  # zen FHIR
  subject:
    localRef: "pt"
  ```

## First-class extensions

While FHIR uses two different ways to define **core elements** and **extensions**, zen FHIR format provides unified framework to describe both. Zen FHIR format offers user-defined elements or "first-class extensions". In zen FHIR format, you can define new attributes (elements) for existing (FHIR) resources. Let's illustrate this on complex attribute for Patient from US-Core FHIR Profile `us-core-race`.

This is how a patient with race looks in FHIR format:

```yaml
resourceType: Patient
id: sample-pt
extension:
- url: http://hl7.org/fhir/us/core/StructureDefinition/us-core-race
  extension:
  - url: text
    valueString: Asian Indian
  - url: ombCategory
    valueCoding:
       system: urn:oid:2.16.840.1.113883.6.238
       code: 2028-9
       display: Asian
  - url: detailed
    valueCoding:
       system:
       code: 2029-7	
       display: Asian Indian
```

If you [describe the extension](https://github.com/zen-fhir/zen.fhir#describe-an-extension) in zen-lang then resource can be transformed and validated in zen FHIR format:

```yaml
resourceType: Patient
  id: sample-pt
  race:
    text: Asian Indian
    category: {system: 'urn:oid:2.16.840.1.113883.6.238', code: 2028-9, display: Asian}
    detailed: {system: 'urn:oid:2.16.840.1.113883.6.238', code: 2029-7, display: Asian Indian}
```

## Polymorphics

In FHIR some elements can have multiple types. Such elements in FHIR spec postfixed with `[x]`, like `Observation.value[x]`, but in JSON such element  is represented in a different way: `Observation.valueString`. The simple logical check "why this is wrong" is "you could not have a collection of union elements in FHIR JSON". zen FHIR format fixes this by moving type as a key inside of a nested object - `valueString:... => value: {string: ...}`

```yaml
#FHIR
resourceType: Observation
valueQuantity:
  unit: ...

# zen FHIR
resourceType: Observation
value:
  Quantity:
    unit: ...
```

<!---
Input:
- FHIR npm package (simplifier.net repo)

Output:
- zen-lang npm package (npmjs.com)
- zen-lang package as Aidbox standalone project (zip, github release)
- Executable (jar, github release)

TODO: Convert zen-lang to FHIR
-->
# Convert FHIR IG to zen-lang
[FHIR package](https://registry.fhir.org/learn) to [zen-lang](https://github.com/zen-lang/zen) schemas converter.

## Artifacts
On each [release](https://github.com/zen-lang/fhir/releases) Github action publishes:
- [zen-lang npm packages on npmjs.com](https://www.npmjs.com/search?q=%40zen-lang%2F)
- [zen-lang standalone projects on github](https://github.com/zen-lang/fhir/releases/latest)
- [jar executable on github](https://github.com/zen-lang/fhir/releases/latest)

How to enable and use zen-lang packages is described in [this guide](https://docs.aidbox.app/profiling/draft-profiling-with-zen-lang)

## How to

### Convert FHIR IG

Download and execute a jar executable from [the latest release](https://github.com/zen-lang/fhir/releases/latest) like this:
```bash
java -jar [JAR_PATH] [COMMAND] [OPTIONS]
```
  
  #### Commands
  **`stndlp`**               Builds standalone Aidbox zen project. Result is a zip archive.
  ###### Options:
   `-i`, `--input` S   Path to node-modules-folder (including node-modules)\
   `-o`, `--output` S  Path to resulting zip archive\
   `--omit-deps`  Remove deps from resulting project
   
  **`zenbnd`**               Builds zen project from provided IG. Result is a NPM package directory
  ###### Options:
   `-i`, `--input` S    Path to node-modules-folder (including node-modules)\
   `-o`, `--output` S   Path to resulting directory\
   `-v`, `--version` S  Resulting package version\
   `-n`, `--name` S     Resulting package name (optional)
   
  **`cmndj`**                Converts ConceptMap to .ndjson.gz bundle
  ###### Options:
   `-i`, `--input` S   Path to node-modules-folder (including node-modules)\
   `-o`, `--output` S  Path to resulting zip archive

#### Example

```bash
# Create and open directory
$ mkdir zen-profiling && cd zen-profiling

# Download needed FHIR package
$ npm --registry https://packages.simplifier.net install hl7.fhir.us.davinci-cdex@latest                                
+ hl7.fhir.us.davinci-cdex@0.2.0
added 1 package from 1 contributor

# Download the package dependencies
$ npm --registry https://packages.simplifier.net install                                                                 
added 3 packages from 3 contributors

# Current directory structure:
├── node_modules
│   ├── hl7.fhir.r4.core
│   ├── hl7.fhir.us.core
│   ├── hl7.fhir.us.davinci-cdex
│   └── hl7.fhir.us.davinci-hrex
└── package-lock.json

# Convert downloaded FHIR packages to zen-lang
$ java -jar ~/Downloads/zen-fhir-0.0.24-2-standalone.jar zenbnd -i node_modules -o zen/node_modules -v 0.1.0   
:done

# Directory structure after convertion
├── node_modules
├── package-lock.json
└── zen
    └── node_modules
        ├── hl7-fhir-r4-core
        │   ├── hl7-fhir-r4-core
        │   ├── hl7-fhir-r4-core.edn
        │   ├── hl7-fhir-r4-core-terminology-bundle.ndjson.gz
        │   └── package.json
        ├── hl7-fhir-us-core
        │   ├── hl7-fhir-us-core
        │   ├── hl7-fhir-us-core.edn
        │   ├── hl7-fhir-us-core-terminology-bundle.ndjson.gz
        │   └── package.json
        ├── hl7-fhir-us-davinci-cdex
        │   ├── hl7-fhir-us-davinci-cdex
        │   ├── hl7-fhir-us-davinci-cdex.edn
        │   ├── hl7-fhir-us-davinci-cdex-terminology-bundle.ndjson.gz
        │   └── package.json
        └── hl7-fhir-us-davinci-hrex
            ├── hl7-fhir-us-davinci-hrex
            ├── hl7-fhir-us-davinci-hrex.edn
            ├── hl7-fhir-us-davinci-hrex-terminology-bundle.ndjson.gz
            └── package.json
```
[How to use zen-lang schemas guide](https://docs.aidbox.app/profiling/draft-profiling-with-zen-lang)

<!---
### Convert zen-lang to FHIR
TBD
-->
