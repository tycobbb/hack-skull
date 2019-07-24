include ./Makefile.base.mk

# -- cosmetics --
help-column-width = 11

# -- context --
tools-stack = stack

# -- init --
## initializes the environment
init: init/pre init/base init/post
.PHONY: init

## installs hie (warning, slow)
init/hie:
ifeq ("$(shell command -v hie)", "")
	cd tmp
	git clone https://github.com/haskell/haskell-ide-engine --recursive
	cd haskell-ide-engine && ./install.hs build-all
endif
.PHONY: init/dev

# -- init/helpers
init/base:
	brew bundle -v
.PHONY: init/base

init/pre:
ifeq ("$(shell command -v brew)", "")
	$(info ✘ brew is not installed, please see:)
	$(info - https://brew.sh)
	$(error 1)
endif
.PHONY: init/pre

init/post:
ifeq ("$(shell command -v hie)", "")
	$(info ✘ hie not installed, to install the, run:)
	$(info - make init/hie)
	$(error 1)
endif
.PHONY: init/post

# -- start --
## alias for s/dev
start: s/dev
.PHONY: start

## starts the dev game
s/dev:
	@$(tools-stack) run
.PHONY: s/dev

## starts the dev game in debug
s/dbg:
	@$(tools-stack) run -- --debug
.PHONY: s/dev

## starts the dev game after a clean
s/dev/clean: b/clean s/dev
.PHONY: s/dev/clean

# -- build --
## alias for b/dev
build: b/dev
.PHONY: build

## cleans the build cache
b/clean: b/dev/pre
	$(tools-stack) clean
.PHONY: b/clean

# -- build/dev
## builds the dev utilities
b/dev: b/dev/pre
	$(tools-stack) build
.PHONY: b/dev

# -- build/dev/helpers
b/dev/pre:
ifeq ("$(shell command -v stack)", "")
	$(info ✘ stack is not installed, please run:)
	$(info - make init)
	$(error 1)
endif
.PHONY: b/dev/pre
