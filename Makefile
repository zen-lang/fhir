.EXPORT_ALL_VARIABLES:
.PHONY: test build

SHELL = bash

ZEN_FHIR_VERSION = $(shell git describe --tag --abbrev=0)

set-zen-fhir-version:
	echo -n ${ZEN_FHIR_VERSION} > resources/zen-fhir-version
	sed "s/\$${ZEN_FHIR_VERSION}/${ZEN_FHIR_VERSION}/" zrc/zen/fhir.edn.tpl > zrc/zen/fhir.edn

init: set-zen-fhir-version
	 npm --registry=https://packages.simplifier.net install
	 npm --registry=https://packages.simplifier.net install

repl: sen-zen-fhir-version
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

build-stanadlone-projects:
	clojure -M -m zen.fhir.tooling.aidbox-standalone $(node_modules) $(zrc)

build-terminology-bundles:
	clojure -M -m zen.fhir.tooling.terminology
