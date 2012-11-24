SHELL := /bin/bash

resDir="resources"

buildDir="build"

runDir="${buildDir}/environment"
cabalBuildFile="dist/build/serenity/serenity"
runFile="serenity"

testDir="${buildDir}/test"
cabalTestFile="dist/build/test-serenity/test-serenity"
testFile="serenity-test"

rtsArgs="-K1G"

clean:
	rm -rf dist
	rm -rf ${buildDir}
	cabal clean

build:
	cabal configure --enable-tests
	cabal build

dist: build
	mkdir -p ${buildDir}
	mkdir -p ${runDir}
	cp -r ${resDir}/* ${runDir}
	cp ${cabalBuildFile} ${runDir}/${runFile}
	chmod a+x ${runDir}/${runFile}

dist-test: build
	mkdir -p ${buildDir}
	mkdir -p ${testDir}
	cp -r ${resDir}/* ${testDir}
	cp ${cabalTestFile} ${testDir}/${testFile}
	chmod a+x ${testDir}/${testFile}

test: dist-test
	cd ${testDir}; \
	./${testFile}
	cat dist/test/Serenity-0.1-test-serenity.log

client: dist
	make clientCached

server: dist
	make serverCached


clientCached:
	cd ${runDir}; \
	./${runFile} client

serverCached:
	cd ${runDir}; \
	./${runFile} server

