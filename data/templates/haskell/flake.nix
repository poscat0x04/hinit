{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = {{ project }}-dev.envFunc {};
            defaultPackage = {{ project }};
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          {{ project }} = hpkgs.callCabal2nix "{{ project }}" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit {{ project }};
            {{ project }}-dev = addBuildTools {{ project }} [
              haskell-language-server
              cabal-install
            ];
          };
    };
}
