{
  description = "A generic project initialization tool written in Haskell";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-23.11;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = hinit-dev.envFunc { withHoogle = true; };
            defaultPackage = hinit;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages.override {
            overrides = hself: hsuper: {
              haskeline = hsuper.haskeline_0_8_2;
              megaparsec = hsuper.megaparsec_9_2_0;
              path = hsuper.path_0_9_0;
              optics-core = hsuper.optics-core_0_4;
            };
          };
          hinit-base = hpkgs.callCabal2nix "hinit" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit hinit-base;
            hinit-dev = addBuildTools hinit-base [
              haskell-language-server
              cabal-install
            ];
            hinit = generateOptparseApplicativeCompletion "hi" (justStaticExecutables hinit-base);
          };
    };
}
