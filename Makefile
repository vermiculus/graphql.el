# EMACS_VERSION should be set in your ~/.profile on your development machine
EMAKE_SHA1            ?= $(shell yq r .travis.yml 'env.global[0]' | cut -d= -f2)
PACKAGE_BASENAME      := graphql

.DEFAULT_GOAL: help
.PHONY: clean

emake.mk: export EMAKE_SHA1 := $(EMAKE_SHA1)
emake.mk: ## download the emake Makefile
# we use yq to retrieve the before_install command.
# upon expansion, that command will be run with this target.
	rm -f emake.mk
	$(shell yq r .travis.yml 'before_install[0]')

include emake.mk

clean:
	rm -rf .emake/
	rm -f *.elc
