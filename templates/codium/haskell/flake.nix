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
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    # necessary for stack - nix integration
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs =
    { self
    , flake-utils
    , flakes-tools
    , nixpkgs
    , my-codium
    , drv-tools
    , haskell-tools
    , my-devshell
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (drv-tools.functions.${system}) mkBinName withAttrs withMan withDescription;
      inherit (drv-tools.configs.${system}) man;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      devshell = my-devshell.devshell.${system};
      inherit (my-devshell.functions.${system}) mkCommands;
      inherit (haskell-tools.functions.${system}) toolsGHC;
      inherit (toolsGHC "92")
        stack hls ghc staticExecutable
        implicit-hie ghcid;

      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one markdown-language-features;
      };

      codiumTools = [
        implicit-hie
        ghcid
        stack
        writeSettings
        ghc
        myPackage
      ];

      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = codiumTools ++ [ hls ];
      };

      myPackageDeps = [
        pkgs.hello
        pkgs.lzma
      ];
      myPackage =
        let
          packageName = "nix-managed";
          packageExe = staticExecutable packageName ./.;
        in
        withMan
          (
            withDescription
              (
                withAttrs
                  (pkgs.symlinkJoin {
                    name = packageName;
                    paths = [ packageExe ];
                    buildInputs = [ pkgs.makeBinaryWrapper ];
                    postBuild = ''
                      wrapProgram $out/bin/${packageName} \
                        --set PATH ${
                          pkgs.lib.makeBinPath myPackageDeps
                         }
                    '';
                  })
                  { pname = packageName; }
              ) "Demo Nix-packaged `Haskell` program"
          )
          (
            x: ''
              ${man.DESCRIPTION}
              ${x.meta.description}
            ''
          );

      tools = codiumTools ++ [ codium ];
      flakesTools = mkFlakesTools [ "." ];
    in
    {
      packages = {
        default = codium;
        inherit (flakesTools) updateLocks pushToCachix;
      };

      devShells.default = devshell.mkShell
        {
          packages = tools;
          bash.extra = ''printf "Hello, world!\n"'';
          commands = mkCommands "tools" tools;
        };

      # Nix-provided libraries for stack
      stack-shell = { ghcVersion }:

        pkgs.haskell.lib.buildStackProject {
          name = "stack-shell";

          ghc = pkgs.haskell.compiler.${ghcVersion};

          buildInputs = myPackageDeps;
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
