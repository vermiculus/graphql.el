# EMACS_VERSION should be set in your ~/.profile on your development machine
EMAKE_SHA1            ?= $(shell yq r .travis.yml 'env.global[0]' | cut -d= -f2)
PACKAGE_BASENAME      := graphql

.DEFAULT_GOAL: help

emake.mk: export EMAKE_SHA1 := $(EMAKE_SHA1)
emake.mk:                       ## download the emake Makefile
	$(shell yq r .travis.yml 'before_install[0]')

include emake.mk
