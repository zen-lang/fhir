.EXPORT_ALL_VARIABLES:
.PHONY: test build

SHELL = bash

init:
	 npm --registry=https://packages.simplifier.net install
	 npm --registry=https://packages.simplifier.net install

repl:
	clj -M:test:nrepl -m nrepl.cmdline --middleware [cider.nrepl/cider-middleware]

test:
	clojure -M:test:kaocha

init-r3:
	cd r3 && npm --registry=https://packages.simplifier.net install
	cd r3 && npm --registry=https://packages.simplifier.net install

build:
	clojure -X:build all

zen-profiles:
	clojure -M -m zen.fhir.tooling $(node_modules) $(zrc) $(ver) $(package)

zen-profiles-nictiz:
	clojure -M -m zen.fhir.tooling.nictiz $(node_modules) $(zrc) $(ver) $(package)

build-stanadlone-projects:
	clojure -M -m zen.fhir.tooling.aidbox-standalone $(node_modules) $(zrc)

build-stanadlone-projects-nictiz:
	clojure -M -m zen.fhir.tooling.aidbox-standalone-nictiz $(node_modules) $(zrc)

build-terminology-bundles:
	clojure -M -m zen.fhir.tooling.terminology
