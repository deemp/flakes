{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = "github:deemp/flakes?dir=codium";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    devshell.url = "github:deemp/flakes?dir=devshell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
  };
  outputs =
    { self
    , flake-utils
    , flakes-tools
    , nixpkgs
    , my-codium
    , drv-tools
    , haskell-tools
    , devshell
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (devshell.functions.${system}) mkCommands mkShell;
      inherit (haskell-tools.functions.${system}) toolsGHC;

      # Next, set the desired GHC version
      ghcVersion = "92";

      # and the name of the package
      myPackageName = "lima";

      override = {
        overrides = self: super: {
          myPackage = super.callCabal2nix myPackageName ../. { };
        };
      };

      inherit (toolsGHC ghcVersion override (ps: [ ps.myPackage ]) [ ]) cabal hls hpack ghcid;

      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens yaml
          git nix-ide workbench markdown-all-in-one markdown-language-features;
      };

      codiumTools = [
        writeSettings
        cabal
        hls
        hpack
        ghcid
      ];

      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = codiumTools;
      };

      tools = codiumTools ++ [ codium ];
    in
    {
      packages = {
        default = codium;
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
