{
  inputs = {
    my-codium.url = "github:br4ch1st0chr0n3/flakes?dir=codium";
    nixpkgs.follows = "my-codium/nixpkgs";
    flake-utils.follows = "my-codium/flake-utils";
    drv-tools.follows = "my-codium/drv-tools";
    my-devshell.follows = "my-codium/my-devshell";
    flake-tools.url = "github:br4ch1st0chr0n3/flakes?dir=flake-tools";
    manager.url = "github:br4ch1st0chr0n3/flakes?dir=manager";
    haskell-tools.follows = "manager/haskell-tools";
    flake-compat.follows = "manager/flake-compat";
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , my-codium
    , drv-tools
    , haskell-tools
    , flake-tools
    , manager
    , my-devshell
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.functions.${system})
        writeSettingsJSON
        mkCodium
        ;
      inherit (my-codium.configs.${system})
        extensions
        settingsNix
        ;
      devshell = my-devshell.devshell.${system};
      inherit (flake-tools.functions.${system})
        mkFlakesTools
        ;
      inherit (haskell-tools.functions.${system})
        toolsGHC
        ;
      hsShellTools = haskell-tools.toolSets.${system}.shellTools;
      inherit (toolsGHC "92") staticExecutable stack hls;

      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one;
      };

      tools = (builtins.attrValues hsShellTools) ++ [
        manager.packages.${system}.default
        stack
        writeSettings
        hls
        pkgs.jq
      ];

      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = tools;
      };

      flakesTools = mkFlakesTools [ "." ];
    in
    {
      packages = {
        default = hls;
        pushToCachix = flakesTools.pushToCachix;
        updateLocks = flakesTools.update;
      };

      devShells.default = devshell.mkShell
        {
          packages = [ codium ] ++ tools;
          bash = {
            extra = ''
              source <(manager --bash-completion-script '$(which manager)')
            '';
          };
          commands = [
            {
              name = "codium, ghcid, stack";
              help = "available in codium";
            }
            {
              name = "${writeSettings.name}";
              help = "write .vscode/settings.json";
            }
            {
              name = "manager";
              category = "tools";
              help = "manage Haskell modules and template files";
            }
          ];
        };

      stack-shell = { ghcVersion }:

        pkgs.haskell.lib.buildStackProject {
          name = "nix-managed-stack-shell";

          ghc = pkgs.haskell.compiler.${ghcVersion};

          buildInputs = [
            pkgs.zlib
          ];
        };
    });

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://hydra.iohk.io"
      "https://br4ch1st0chr0n3.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "br4ch1st0chr0n3.cachix.org-1:o1FA93L5vL4LWi+jk2ECFk1L1rDlMoTH21R1FHtSKaU="
    ];
  };
}
