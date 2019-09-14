shell:
		nix-shell
build:
		rm -f result
		nix-build release.nix
clean:
		rm -f .ghc.environment*
		nix-shell --run 'cabal new-clean'
emacs:
		nix-shell --run 'emacs . &'
c2n:
		cabal2nix . > project.nix

.PHONY: shell build clean emacs c2n
