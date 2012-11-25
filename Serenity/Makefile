SHELL := /bin/bash

resDir="resources"

buildDir="build"

runDir="${buildDir}/environment"
cabalBuildFile="dist/build/serenity/serenity"
runFile="serenity"
runClientArgs= +RTS -N -K500M -m20 -M3G -RTS client localhost 9900 ALC
runClientLogFile="${runDir}/client.log"
runServerArgs= +RTS -N -K500M -M1G -RTS server
runServerLogFile="${runDir}/server.log"
testDir="${buildDir}/test"
cabalTestFile="dist/build/test-serenity/test-serenity"
testFile="serenity-test"
testLogFile="dist/test/Serenity-0.1-test-serenity.log"
testRTSArgs= +RTS -K1G -RTS


clean:
	rm -rf dist
	rm -rf ${buildDir}
	cabal clean

build:
	cabal install --only-dependencies
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
	./${testFile} ${testRTSArgs}; \
	cat ${testLogFile}

client: dist
	make clientCached

server: dist
	make serverCached

run: dist
	make runCached

runCached:
	sleep1; make clientCached > ${runClientLogFile} &
	make serverCached > ${runServerLogFile}

clientCached:
	cd ${runDir}; \
	./${runFile} ${runClientArgs}

serverCached:
	cd ${runDir}; \
	./${runFile} ${runServerArgs}



