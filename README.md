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
On each [release](https://github.com/zen-lang/fhir/releases) Github action publishes:
- [zen-lang npm packages on npmjs.com](https://www.npmjs.com/search?q=%40zen-lang%2F)
- [zen-lang standalone projects on github](https://github.com/zen-lang/fhir/releases/latest)
- [jar executable on github](https://github.com/zen-lang/fhir/releases/latest)

How to enable and use zen-lang packages is described in [this guide](https://docs.aidbox.app/profiling/draft-profiling-with-zen-lang)

## How to

### Convert FHIR to zen-lang

Download and execute a jar executable from [the latest release](https://github.com/zen-lang/fhir/releases/latest) like this:
```bash
java -jar [JAR_PATH] [COMMAND] [OPTIONS]
```
  
  #### Commands
  **`zenbnd`**               Builds zen project from provided IG. Result is a directory.
  ###### Options:
   `-i`, `--input` S    Path to node-modules-folder\
   `-o`, `--output` S   Path to resulting zip archive\
   `-v`, `--version` S  Resulting package version\
   `-n`, `--name` S     Resulting package name (optional)
   
  **`stndlp`**               Builds standalone Aidbox zen project. Result is a zip archive.
  ###### Options:
   `-i`, `--input` S   Path to node-modules-folder\
   `-o`, `--output` S  Path to resulting zip archive
   
  **`cmndj`**                Converts ConceptMap to .ndjson.gz bundle
  ###### Options:
   `-i`, `--input` S   Path to node-modules-folder\
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
