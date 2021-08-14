package = awssy
stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

all: build 
cabal-all: cabal-build 

setup:
	$(stack) setup
	$(stack) build --dependencies-only --test --no-run-tests
	$(stack) install hlint weeder

lint:
	hlint .
	weeder .

check-nightly:
	$(stack) setup --resolver nightly
	$(stack) build --resolver nightly --pedantic --test

build:
	$(stack) build $(package) --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

build-fast:
	$(stack) build $(package):exe:$(package) --fast --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

build-watch:
	$(stack) build $(package) --fast --file-watch --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

build-profile:
	$(stack) --work-dir .stack-work-profiling --profile build

cabal-build:
	cabal new-build $(package) 

build-dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

run:
	$(stack) build --fast && $(stack) exec -- $(package)

ghci:
	$(stack) ghci $(package):exe:$(package) --ghci-options='-j8 +RTS -A128m -n2m -qg'


bench:
	$(stack) bench $(package)

ghcid:
	$(stack) exec -- ghcid --lint -c "stack ghci $(package):exe:$(package) --ghci-options='-fobject-code -fno-warn-unused-do-bind -j6 +RTS -A128m -n2m -qg' --main-is $(package):exe:$(package)"


dev-deps:
	stack install ghcid


.PHONY : build build-dirty run install ghci test test-ghci ghcid dev-deps lint check-nightly setup
