include ./Makefile.base.mk

# -- cosmetics --
help-column-width = 11

# -- context --
tools-stack = stack

# -- init --
## initializes the dev environment
init: init/pre init/base
.PHONY: init

# -- init/helpers
# need to use global apollo for now, see:
#	- https://github.com/apollographql/apollo-tooling/issues/881
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

# -- start --
## alias for s/dev
start: s/dev
.PHONY: start

## starts the dev server
s/dev:
	@$(tools-stack) run
.PHONY: s/dev

## starts the dev server after a clean
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