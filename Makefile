EENVS := PACKAGE_FILE="graphql.el" PACKAGE_DEV_DEPS="" PACKAGE_SOURCES="gnu"
EMAKE := $(EENVS) emacs -batch -l test/make.el --eval "(make (pop argv))"

.PHONY: clean install compile test CI-setup

clean:
	rm -f *.elc
	rm -rf .elpa/

install: .elpa/
.elpa/:
	$(EMAKE) update

compile: clean
	$(EMAKE) compile

test:
	$(EMAKE) test

CI-setup:
	export PATH="$(HOME)/bin:$(PATH)"
	wget 'https://raw.githubusercontent.com/flycheck/emacs-travis/master/emacs-travis.mk'
	make -f emacs-travis.mk install_emacs
	emacs --version
