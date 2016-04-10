TAEGET ?= web-beautify.el
EMACS ?= emacs
CASK ?= cask

.PHONY : test

EMACS ?= emacs
CASK ?= cask

ELPA_DIR = \
	.cask/$(shell $(EMACS) -Q --batch --eval '(princ emacs-version)')/elpa

test: elpa test-compile
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		$(patsubst %,-l %,$(wildcard test/test-*.el)) \
		-f ert-run-tests-batch-and-exit

test/test-%: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		-l $@ \
 		-f ert-run-tests-batch-and-exit

test-compile:
	$(CASK) exec $(EMACS) -batch -Q -L . -eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" $(TAEGET)

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
