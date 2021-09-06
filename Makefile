.EXPORT_ALL_VARIABLES:
.PHONY: test build

SHELL = bash

init:
	 npm --registry=https://packages.simplifier.net install

repl:
	clj -M:test:nrepl -m nrepl.cmdline --middleware [cider.nrepl/cider-middleware]

test:
	clojure -M:test:kaocha

build:
	clojure -X:build all

zen-profiles:
	clojure -M -m zen.fhir.tooling $(node_modules) $(zrc) $(ver) $(package)

