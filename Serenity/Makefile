SHELL := /bin/bash

runClientArgs= +RTS -N -K500M -m20 -M3G -RTS client localhost 9900 Joseph
runClientLogFile=client.log

runServerArgs= +RTS -N -K500M -M1G -RTS server
runServerLogFile=server.log

clean:
	cabal clean

build:
	cabal configure --enable-tests
	cabal build

test: build
	cabal test

install: build
	cabal install

client: install
	serenity ${runClientArgs}

server: install
	serenity ${runServerArgs}

run: install
	(sleep 2; serenity ${runClientArgs} > ${runClientLogFile}) &
	serenity ${runServerArgs} > ${runServerLogFile}
