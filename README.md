<!---
Input:
- FHIR npm package (simplifier.net repo)

Output:
- zen-lang npm package (npmjs.com)
- zen-lang package as Aidbox standalone project (zip, github release)
- Executable (jar, github release)

TODO: Convert zen-lang to FHIR
-->
# Convert FHIR to zen-lang
[FHIR package](https://registry.fhir.org/learn) to [zen-lang](https://github.com/zen-lang/zen) schemas converter.

## Artifacts
On each release Github action publishes:
- [zen-lang npm packages on npmjs.com](https://www.npmjs.com/search?q=%40zen-lang%2F)
- [zen-lang standalone projects on github](https://github.com/zen-lang/fhir/releases)
- [jar executable on github](https://github.com/zen-lang/fhir/releases)

How to enable and use zen-lang packages is described in [this guide](https://docs.aidbox.app/profiling/draft-profiling-with-zen-lang)

## How to

### Convert FHIR to zen-lang

Download and execute a jar executable from [the latest release](https://github.com/zen-lang/fhir/releases) like this:
```bash
java -jar [JAR_PATH] [FHIR_FROM_PATH] [ZEN_TO_PATH] [VER] [PACKAGE_NAME]
```
#### Arguments description
  - `JAR_PATH` - path to downloaded jar file _(**required**)_
  - `FHIR_FROM_PATH` - path to directory with FHIR packages _(**required**)_
  - `ZEN_TO_PATH` - path to directory where converted zen-lang schemas will be saved _(**required**)_
  - `VER` - converted zen-lang packages version _(**required**)_
  - `PACKAGE_NAME` - save only this specific package _(optional)_

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
$ java -jar ~/Downloads/zen-fhir-0.0.24-2-standalone.jar node_modules zen/node_modules 0.1.0   
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
