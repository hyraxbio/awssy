package = awssy
exe = awssy

all: build

build: cabal-build


cabal-run:
	cabal run $(exe) -- ./config/ ./config/mesh.ini

cabal-run-fast:
	cabal run $(exe) --disable-optimisation --ghc-options "-O0 -j6 +RTS -A128m -n2m -qg -RTS"

cabal-build:
	cabal build $(package) --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

cabal-build-fast:
	cabal build $(package) --disable-optimisation --ghc-options "-O0 -j6 +RTS -A128m -n2m -qg -RTS"

cabal-ghcid:
	ghcid --lint -c "cabal repl --repl-options='-ignore-dot-ghci' --repl-options='-fobject-code' --repl-options='-fno-warn-unused-do-bind' --repl-options='-j6' "


.PHONY : cabal-run cabal-build cabal-build-fast cabal-ghcid cabal-test cabal-run-fast cabal-run-fast-prod
