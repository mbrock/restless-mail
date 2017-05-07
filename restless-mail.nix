let pkgs = import <nixpkgs> {};
in rec {
  restless-mail = pkgs.haskellPackages.callPackage ./default.nix {};
}
