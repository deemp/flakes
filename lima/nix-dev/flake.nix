{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    codium.url = "github:deemp/flakes?dir=codium";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    devshell.url = "github:deemp/flakes?dir=devshell";
    haskell-language-server_.url = "github:deemp/flakes?dir=source-flake/haskell-language-server";
    haskell-language-server.follows = "haskell-language-server_/haskell-language-server";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (inputs.codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (inputs.codium.configs.${system}) extensions settingsNix;
      inherit (inputs.devshell.functions.${system}) mkCommands mkRunCommandsDir mkShell;
      inherit (inputs.haskell-tools.functions.${system}) toolsGHC;
      hls = inputs.haskell-language-server.packages.${system}.default;
      # Next, set the desired GHC version
      ghcVersion = "926";

      # and the name of the package
      myPackageName = "lima";

      inherit (pkgs.haskell.lib)
        dontCheck
        dontStrip
        dontHaddock
        overrideCabal
        ;

      override =
        let donts = drv: pkgs.lib.trivial.pipe drv [ dontStrip dontCheck dontHaddock ]; in
        {
          overrides = self: super: {
            myPackage = overrideCabal (super.callCabal2nix myPackageName ../. { }) (
              x: {
                testHaskellDepends = [
                  super.doctest-parallel_0_3_0_1
                ] ++ (x.testHaskellDepends or [ ]);
              }
            );
          };
        };

      inherit (toolsGHC {
        version = ghcVersion;
        inherit override;
        packages = (ps: [ ps.myPackage ]);
      })
        cabal hpack ghcid;

      tools = [
        hls
        cabal
        hpack
        ghcid
      ];

      packages = {
        codium = mkCodium {
          extensions = { inherit (extensions) nix haskell misc github markdown; };
          runtimeDependencies = tools;
        };
        writeSettings = writeSettingsJSON {
          inherit (settingsNix) haskell todo-tree files editor gitlens yaml
            git nix-ide workbench markdown-all-in-one markdown-language-features;
        };
      };

      devShells.default = mkShell {
        packages = tools;
        bash.extra = "export LC_ALL=C.UTF-8";
        commands =
          mkCommands "tools" tools ++
          mkRunCommandsDir "nix-dev" "ide" {
            "codium ." = packages.codium;
            inherit (packages) writeSettings;
          } ++
          [
            {
              name = "cabal-test";
              category = "test";
              help = "Test via `cabal`";
              command = "cabal v1-test";
            }
          ];
      };
    in
    {
      inherit packages devShells;
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
