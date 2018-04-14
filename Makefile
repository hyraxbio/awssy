package = awssy
stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

all: build test lint

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
	$(stack) build $(package) --no-run-tests

build-dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

run:
	$(stack) build --fast && $(stack) exec -- $(package)

ghci:
	$(stack) ghci $(package):exe

test:
	$(stack) test $(package)

test-ghci:
	$(stack) ghci $(package):test:$(package)-test

bench:
	$(stack) bench $(package)

ghcid:
	$(stack) exec -- ghcid -c "stack ghci $(package) --ghci-options='-fobject-code -fno-warn-unused-do-bind' --main-is $(package):exe:$(package)"

ghcid-run:
	$(stack) exec -- ghcid -c "stack ghci $(package):exe --ghci-options='-fobject-code -fno-warn-unused-do-bind' --main-is $(package):exe:$(package)" --test=":main debug" -W

dev-deps:
	stack install ghcid


.PHONY : build build-dirty run install ghci test test-ghci ghcid dev-deps lint check-nightly setup
