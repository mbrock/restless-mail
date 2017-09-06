all: nix; cabal build
nix: default.nix shell.nix; nix-shell --command 'cabal configure'
default.nix: restless-mail.cabal; cabal2nix . > default.nix
