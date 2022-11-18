{
  inputs = {
    nixpkgs_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;
    flakes-tools.url = github:br4ch1st0chr0n3/flakes?dir=flakes-tools;
    drv-tools.url = github:br4ch1st0chr0n3/flakes?dir=drv-tools;
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , flakes-tools
    , drv-tools
    , ...
    }: flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (flakes-tools.functions.${system}) mkFlakesTools;
        flakesTools = mkFlakesTools [ "." ];
        hcl = import ./.nix/hcl.nix;
        tfTools = import ./.nix/tf-tools.nix { inherit pkgs system drv-tools; };
        tests = (import ./.nix/tests.nix { inherit pkgs system drv-tools; });
        packages = tests // tfTools.packages // {
          pushToCachix = flakesTools.pushToCachix;
          updateLocks = flakesTools.updateLocks;
        };
      in
      {
        functions = tfTools.functions;
        inherit packages hcl;
        devShells.default = pkgs.mkShell {
          buildInputs = pkgs.lib.lists.flatten (builtins.attrValues packages);
        };
      });

  nixConfig = {
    extra-trusted-substituters = [
      https://haskell-language-server.cachix.org
      https://nix-community.cachix.org
      https://hydra.iohk.io
      https://br4ch1st0chr0n3.cachix.org
    ];
    extra-trusted-public-keys = [
      haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8=
      nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
      hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
      br4ch1st0chr0n3.cachix.org-1:o1FA93L5vL4LWi+jk2ECFk1L1rDlMoTH21R1FHtSKaU=
    ];
  };
}
