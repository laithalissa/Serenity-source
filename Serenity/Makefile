SHELL := /bin/bash

environmentDir="environment"
resDir="resources"
cabalBuildFile="dist/build/serenity/serenity"
cabalTestFile="dist/build/test-serenity/test-serenity"
runFile="serenity"
rtsArgs="-K1G"

clean:
	cabal clean
	rm -rf ${environmentDir}

build:
	cabal configure --enable-tests
	cabal build

prepareEnvironment:
	mkdir ${environmentDir}
	cp -r ${resDir}/* ${environmentDir}
	cp ${cabalBuildFile} "${environmentDir}/${runFile}"
	chmod a+x "${environmentDir}/${runFile}"

runServer: build prepareEnvironment
	cd ${environmentDir}; \
	./${runFile} server

