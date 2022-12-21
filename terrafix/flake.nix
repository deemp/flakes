{
  inputs = {
    nixpkgs_.url = github:deemp/flakes?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:deemp/flakes?dir=source-flake/flake-utils;
    drv-tools.url = github:deemp/flakes?dir=drv-tools;
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , drv-tools
    , ...
    }: flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hcl = import ./nix-files/hcl.nix;
        tfTools = import ./nix-files/tf-tools.nix { inherit pkgs system drv-tools; };
        tests = import ./nix-files/tests.nix { inherit pkgs system drv-tools; };
        packages = tests // tfTools.packages;
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
      https://deemp.cachix.org
    ];
    extra-trusted-public-keys = [
      haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8=
      nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
      hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
      deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18=
    ];
  };
}
