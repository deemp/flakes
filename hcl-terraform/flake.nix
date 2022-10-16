{
  inputs = {
    nixpkgs_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs;
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;
    drv-tools.url = github:br4ch1st0chr0n3/flakes?dir=drv-tools;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs = { drv-tools, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hcl = import ./hcl.nix;
        tfTools = import ./tf-tools.nix { inherit pkgs system drv-tools; };
        tests = (import ./tests.nix { inherit pkgs system drv-tools; });
      in
      {
        functions = tfTools;
        inherit hcl;
        packages = {
        } // tests;
      }
    );
}
