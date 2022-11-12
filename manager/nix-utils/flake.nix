{
  inputs = {
    nixpkgs_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = "github:br4ch1st0chr0n3/flakes?dir=codium";
    drv-tools.url = "github:br4ch1st0chr0n3/flakes?dir=drv-tools";
    flake-tools.url = "github:br4ch1st0chr0n3/flakes?dir=flake-tools";
    haskell-tools.url = "github:br4ch1st0chr0n3/flakes?dir=language-tools/haskell";
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
    , flake-tools
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.functions.${system})
        writeSettingsJSON
        mkCodium
        ;
      inherit (drv-tools.functions.${system})
        mkBinName
        ;
      devshells = my-codium.functions.${system};
      inherit (my-codium.configs.${system})
        extensions
        settingsNix
        ;
      devshell = my-codium.devshell.${system};
      inherit (haskell-tools.functions.${system})
        toolsGHC
        ;
      hsShellTools = haskell-tools.toolSets.${system}.shellTools;

      inherit (toolsGHC "90") hls;

      # Wrap Stack to work with manager's hpack
      stack-wrapped = pkgs.symlinkJoin {
        # will be available as the usual `stack` in terminal
        name = "stack";
        paths = [ pkgs.stack ];
        buildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/stack \
            --add-flags "\
              --system-ghc \
              --no-install-ghc \
              --with-hpack '${mkBinName hsShellTools.hpack "hpack"}' \
            "
        '';
      };

      writeSettings = writeSettingsJSON ({
        inherit (settingsNix) haskell todo-tree files editor gitlens git nix-ide workbench;
      });

      tools = (builtins.attrValues hsShellTools) ++ [
        stack-wrapped
        writeSettings
        hls
      ];

      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github; };
        runtimeDependencies = tools;
      };
    in
    {
      packages = {
        default = codium;
        inherit writeSettings;
      };

      devShells.default = devshell.mkShell
        {
          packages = [ codium ];
          bash = {
            extra = ''
            '';
          };
          commands = [
            {
              name = "codium, ${writeSettings.name}, ghcid";
            }
            {
              name = "ghc, stack";
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
