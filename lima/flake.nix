{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    devshell.url = "github:deemp/flakes?dir=devshell";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , drv-tools
    , haskell-tools
    , devshell
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (drv-tools.functions.${system}) mkBinName withAttrs withMan withDescription;
      inherit (drv-tools.configs.${system}) man;
      inherit (devshell.functions.${system}) mkCommands mkShell;
      inherit (haskell-tools.functions.${system}) haskellTools;
      inherit (haskellTools "92" { } (_: [ ]) [ ]) justStaticExecutable cabal callCabal2nix;

      myPackage =
        let
          packageName = "lima";
          packageExe = justStaticExecutable packageName (callCabal2nix packageName ./. { });
        in
        withMan
          (withDescription packageExe "Convert between `Literate Haskell` (`.lhs`) and `Markdown` (`.md`)")
          (
            self: ''
              ${man.DESCRIPTION}
              ${self.meta.description}

              ${man.SYNOPSYS}
              `${packageName} (md2lhs|lhs2md|hs2md) file1 [file2] [...]`

              ${man.EXAMPLES}
              `${packageName} lhs2md testdata/input0.lhs testdata/input1.lhs`
              :   convert: `testdata/input0.lhs` ->  `testdata/input0.lhs.md` and `testdata/input1.lhs` -> `testdata/input1.lhs.md`
              `${packageName} hs2md testdata/input2.hs`
              :   convert: `testdata/input2.hs` ->  `testdata/input2.hs.md`
            ''
          );

      tools = [ cabal ];
    in
    {
      packages = {
        default = myPackage;
      };

      devShells.default = mkShell
        {
          packages = tools;
          bash.extra = '''';
          commands = mkCommands "tools" tools;
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
