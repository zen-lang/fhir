.EXPORT_ALL_VARIABLES:
.PHONY: test

SHELL = bash

init:
	 npm config set registry  https://packages.simplifier.net && npm install

repl:
	clj -M:test:nrepl -m nrepl.cmdline --middleware [cider.nrepl/cider-middleware]

test:
	clojure -M:test:kaocha

# init:
#		mkdir -p fhir && cd fhir && curl http://hl7.org/fhir/definitions.json.zip > definitions.json.zip && unzip definitions.json.zip

deploy:
	cd build && git init && git add . && git commit -m "first commit" & git branch -M master & git remote add origin git@github.com:zen-lang/zen-lang.github.io.git && git push -u --force origin master

pom:
	clj -X:deps mvn-pom

jar:
	clojure -X:depstar jar :jar target/zen-doc.jar

uberjar:
	clojure -X:depstar uberjar :jar target/zen-doc.uber.jar

pub:
	clj -A:deploy

zen-profiles:
	clj -M -m zen.fhir.tooling $(node_modules) $(zrc) $(ver)

