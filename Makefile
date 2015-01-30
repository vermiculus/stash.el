VERSIONS = 1 2 3 4

all :: $(VERSIONS)

$(VERSIONS) :: clean
	evm install emacs-24.$@-bin --skip || true
	evm use emacs-24.$@-bin
	emacs --version
	cask install
	emacs --batch -L . -l ert -l tests.el -f ert-run-tests-batch-and-exit

clean:
	cask clean-elc

install_cask:
	curl -fsSkL https://raw.github.com/cask/cask/master/go | python

install_evm:
	curl -fsSkL https://raw.github.com/rejeep/evm/master/go | bash
