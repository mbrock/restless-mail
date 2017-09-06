let
  pkgs = import <nixpkgs> {};
  haskell = pkgs.haskellPackages.override {
    overrides = self: super: {
      restless-git = pkgs.haskell.lib.addBuildTool (self.callPackage (pkgs.fetchFromGitHub {
        owner = "lessrest";
        repo = "restless-git";
        rev = "4ea8f9309f6ea19a5766d87e8fc5c5e99b37182a";
        sha256 = "0qghkh3aq1zxpr62sdn1lxrc9k093k5flrim0nly5zvk97yqzhny";
      }) {}) pkgs.git;
    };
  };
in rec {
  restless-mail = haskell.callPackage ./default.nix {};
}
