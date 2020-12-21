{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          hpkgs = pkgs.haskellPackages.override {
            overrides = self: super: {
              haskeline = super.haskeline_0_8_1_0;
            };
          };
          HI = hpkgs.callCabal2nix "HI" ./. {};
          shell = pkgs.haskell.lib.addBuildTools HI (with pkgs; [
            cabal-install
            haskell-language-server
          ]);
        in
          {
            devShell = shell.envFunc { withHoogle = true; };
            defaultPackage = HI;
          }
    );
}
