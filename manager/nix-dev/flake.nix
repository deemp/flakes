{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = "github:deemp/flakes?dir=codium";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    devshell.url = "github:deemp/flakes?dir=devshell";
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , my-codium
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
      inherit (toolsGHC "90") stack hls hpack implicit-hie ghcid;

      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one
          json-language-features markdown-language-features;
      };

      codiumTools = [
        hpack
        stack
        writeSettings
        hls
        pkgs.jq
        implicit-hie
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

      devShells.default =
        mkShell
          {
            packages = tools;
            bash.extra = "";
            commands =
              (mkCommands "tools" tools) ++
              [
                {
                  name = "test-gen-hie";
                  category = "test";
                  help = "run gen-hie from a Haskell script";
                  command = ''
                    cat <<EOT > Ex.hs
                    module Ex where
                    import System.Process
                    main = putStrLn =<< readProcess "gen-hie" ["--help"] ""
                    EOT
                    stack runghc -- Ex
                    rm Ex.*
                  '';
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
