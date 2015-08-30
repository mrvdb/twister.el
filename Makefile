emacs ?= emacs
all: test

test: clean
	cask exec emacs -Q -batch -l ert-tests/twister-rpc-test.el -l twister-rpc.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -Q -batch -f batch-byte-compile twister-rpc.el

clean:
	rm -f f.elc

.PHONY:	all test
