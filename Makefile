nix: default.nix restless-mail.nix; nix-shell -A restless-mail.env \
  restless-mail.nix --command 'cabal configure'
default.nix: restless-mail.cabal; cabal2nix . > default.nix
