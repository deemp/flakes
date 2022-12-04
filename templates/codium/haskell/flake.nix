{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = "github:deemp/flakes?dir=codium";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    my-devshell.url = "github:deemp/flakes?dir=devshell";
    manager.url = "github:deemp/flakes?dir=manager";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , my-codium
    , drv-tools
    , haskell-tools
    , my-devshell
    , manager
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (drv-tools.functions.${system}) mkBinName withAttrs;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      devshell = my-devshell.devshell.${system};
      inherit (my-devshell.functions.${system}) mkCommands;
      inherit (haskell-tools.functions.${system}) toolsGHC;
      hsShellTools = haskell-tools.toolSets.${system}.shellTools;
      inherit (toolsGHC "92") stack hls ghc;

      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one;
      };

      tools = [
        hsShellTools.implicit-hie
        hsShellTools.ghcid
        manager.packages.${system}.default
        stack
        writeSettings
        ghc
      ];

      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = tools ++ [ hls ];
      };
    in
    {
      packages = {
        default = codium;
      };

      devShells.default = devshell.mkShell
        {
          packages = [ codium ] ++ tools;
          bash = {
            extra = ''
              # enable completions for `manager`
              source <(manager --bash-completion-script `which manager`)
            '';
          };
          commands = mkCommands "tools" (tools ++ [ codium ]);
        };

      # Nix-provided libraries for stack
      stack-dependencies = { ghcVersion }:

        pkgs.haskell.lib.buildStackProject {
          name = "stack-dependencies";

          ghc = pkgs.haskell.compiler.${ghcVersion};

          buildInputs = [
            pkgs.lzma
            pkgs.hello
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
