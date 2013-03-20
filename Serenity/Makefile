SHELL := /bin/bash

runClientArgs= +RTS -N -K500M -m20 -M3G -RTS -m client -h localhost -p 9900 -n 2
runClientLogFile=client.log

runServerArgs= +RTS -N -K500M -M1G -RTS -m server -p 9900
runServerLogFile=server.log

logDir=log

clean:
	cabal clean
	rm -rf ${logDir}

build:
	mkdir -p ${logDir}
	cabal configure --enable-tests
	cabal build

test: build
	cabal test --show-details=always

install: build
	cabal install --disable-documentation

client: install
	serenity ${runClientArgs}

server: install
	serenity ${runServerArgs}

run: install
	(sleep 2; serenity ${runClientArgs} > ${logDir}/${runClientLogFile}) &
	serenity ${runServerArgs} > ${logDir}/${runServerLogFile}

runD:
	(sleep 2; serenity ${runClientArgs} > ${logDir}/${runClientLogFile}) &
	serenity ${runServerArgs} > ${logDir}/${runServerLogFile}

