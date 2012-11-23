SHELL := /bin/bash

environmentDir="environment"
resDir="resources"
cabalBuildFile="dist/build/serenity/serenity"
cabalTestFile="dist/build/test-serenity/test-serenity"
runFile="serenity"
rtsArgs="-K1G"

clean:
	rm -rf dist
	rm -rf ${environmentDir}
	cabal clean

build:
	cabal configure --enable-tests
	cabal build

dist: build
	mkdir -p ${environmentDir}
	cp -r ${resDir}/* ${environmentDir}
	cp ${cabalBuildFile} "${environmentDir}/${runFile}"
	chmod a+x "${environmentDir}/${runFile}"

client: dist
	make clientCached

server: dist
	make serverCached


clientCached:
	cd ${environmentDir}; \
	./${runFile} client

serverCached:
	cd ${environmentDir}; \
	./${runFile} server

