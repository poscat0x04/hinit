{
  description = "A generic project initialization tool written in Haskell";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;
  inputs.spdx-license = {
    url = github:poscat0x04/spdx-license/0.1.0;
    inputs = {
      nixpkgs.follows = "/nixpkgs";
      flake-utils.follows = "/flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, spdx-license, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = hi-dev.envFunc { withHoogle = true; };
            defaultPackage = hi;
          }
    ) // {
      overlay = self: super:
        let
          pkgs = spdx-license.overlay self super;
          hpkgs = super.haskellPackages.override {
            overrides = hself: hsuper: {
              haskeline = hsuper.haskeline_0_8_1_0;
              spdx-license = pkgs.spdx-license;
            };
          };
          hi-base = hpkgs.callCabal2nix "HI" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit hi-base;
            hi-dev = addBuildTools hi-base [
              haskell-language-server
              cabal-install
            ];
            hi = generateOptparseApplicativeCompletion "hi" (justStaticExecutables hi-base);
          };
    };
}
