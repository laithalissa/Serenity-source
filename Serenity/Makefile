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
	cabal configure 
	cabal build

prepareEnvironment:
	mkdir -p ${environmentDir}
	cp -r ${resDir}/* ${environmentDir}
	cp ${cabalBuildFile} "${environmentDir}/${runFile}"
	chmod a+x "${environmentDir}/${runFile}"

runServer: build prepareEnvironment
	cd ${environmentDir}; \
	./${runFile} server

