{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;

  lessrest-git = (pkgs.fetchFromGitHub {
    owner  = "lessrest";
    repo   = "restless-git";
    rev    = "4ea8f9309f6ea19a5766d87e8fc5c5e99b37182a";
    sha256 = "0qghkh3aq1zxpr62sdn1lxrc9k093k5flrim0nly5zvk97yqzhny";
  });

  haskellPackages = pkgs.haskellPackages.override {
    overrides = (self: super: {
      ghc =
        super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages =
        self.ghc.withPackages;
      restless-git =
        pkgs.haskell.lib.addBuildTool (self.callPackage lessrest-git {}) pkgs.git;
    });
  };

  drv = haskellPackages.callPackage (import ./default.nix) {};

in if pkgs.lib.inNixShell then drv.env else drv
