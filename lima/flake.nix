{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , drv-tools
    , haskell-tools
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (drv-tools.functions.${system}) mkBinName withAttrs withMan withDescription;
      inherit (drv-tools.configs.${system}) man;
      inherit (haskell-tools.functions.${system}) toolsGHC;
      inherit (toolsGHC { version = "92"; }) justStaticExecutable callCabal2nix;

      myPackage =
        let
          packageName = "lima";
          packageExe = justStaticExecutable packageName (callCabal2nix packageName ./. { });
        in
        withMan
          (withDescription packageExe "Convert `Haskell` (`.hs`) to `Markdown` (`.md`) or between `Literate Haskell` (`.lhs`) and `Markdown` (`.md`)")
          (
            self: ''
              ${man.DESCRIPTION}
              ${self.meta.description}

              ${man.SYNOPSYS}
              `${packageName} <command> (-f file) [-c config]`

              ${man.EXAMPLES}
              `${packageName} lhs2md -f testdata/input0.lhs -f testdata/input1.lhs`
              :   convert: `testdata/input0.lhs` ->  `testdata/input0.lhs.md` and `testdata/input1.lhs` -> `testdata/input1.lhs.md`
              `${packageName} hs2md -f testdata/input2.hs -c testdata/config/lima.yaml`
              :   convert: `testdata/input2.hs` ->  `testdata/input2.hs.md`
            ''
          );
    in
    {
      packages = {
        default = myPackage;
      };
    });

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
