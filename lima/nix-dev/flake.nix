{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    codium.url = "github:deemp/flakes?dir=codium";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    devshell.url = "github:deemp/flakes?dir=devshell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (inputs.codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (inputs.codium.configs.${system}) extensions settingsNix;
      inherit (inputs.devshell.functions.${system}) mkCommands mkShell;
      inherit (inputs.haskell-tools.functions.${system}) toolsGHC;

      # Next, set the desired GHC version
      ghcVersion = "92";

      # and the name of the package
      myPackageName = "lima";

      override = {
        overrides = self: super: {
          myPackage = super.callCabal2nix myPackageName ../. { };
        };
      };

      inherit (toolsGHC {
        version = ghcVersion;
        inherit override;
        packages = (ps: [ ps.myPackage ]);
      })
        cabal hls hpack ghcid;

      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens yaml
          git nix-ide workbench markdown-all-in-one markdown-language-features;
      };

      codiumTools = [
        cabal
        hls
        hpack
        ghcid
      ];

      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = codiumTools;
      };

      tools = codiumTools;
    in
    {
      packages = {
        inherit codium writeSettings;
      };

      devShells.default = mkShell
        {
          packages = tools;
          bash.extra = "";
          commands = (mkCommands "tools" tools) ++ [
            {
              name = "nix run nix-dev/#writeSettings";
              category = "ide";
              help = writeSettings.meta.description;
            }
            {
              name = "nix run nix-dev/#codium .";
              category = "ide";
              help = codium.meta.description;
            }
            {
              name = "cabal-test";
              category = "test";
              help = "Test via `cabal`";
              # just in case we need to override some Haskell packages
              # and don't want cabal to load them from Hackage
              # we need to use the `cabal v1-*` commands
              command = "cabal v1-test";
            }
          ];
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
