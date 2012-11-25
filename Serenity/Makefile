SHELL := /bin/bash

runClientArgs= +RTS -N -K500M -m20 -M3G -RTS -m client -h localhost -p 9900 -n Joseph
runClientLogFile=client.log

runServerArgs= +RTS -N -K500M -M1G -RTS -m server -p 9900
runServerLogFile=server.log

clean:
	cabal clean

build:
	cabal configure --enable-tests
	cabal build

test: build
	cabal test --show-details=always

install: build
	cabal install

client: install
	serenity ${runClientArgs}

server: install
	serenity ${runServerArgs}

run: install
	(sleep 2; serenity ${runClientArgs} > ${runClientLogFile}) &
	serenity ${runServerArgs} > ${runServerLogFile}
