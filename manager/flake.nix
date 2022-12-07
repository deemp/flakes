{
  inputs = {
    nixpkgs.follows = "haskell-tools/nixpkgs";
    flake-utils.follows = "haskell-tools/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    hpack_.url = "github:deemp/flakes?dir=source-flake/hpack";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    hpack.follows = "hpack_/hpack";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , hpack
    , haskell-tools
    , drv-tools
    , ...
    }:
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsShellTools = haskell-tools.toolSets.${system}.shellTools;
        inherit (haskell-tools.functions.${system}) toolsGHC;
        inherit (toolsGHC "90") staticExecutable;
        inherit (drv-tools.functions.${system}) withDescription withMan;
        inherit (drv-tools.configs.${system}) man;
        managerTools = [
          hpack.packages.${system}.default
          pkgs.haskellPackages.implicit-hie
          pkgs.coreutils
          pkgs.nix
          pkgs.git
        ];
        manager =
          let
            manager_ = "manager";
            manager-exe = staticExecutable manager_ ./.;
          in
          withMan
            (
              withDescription
                (
                  pkgs.symlinkJoin {
                    name = manager_;
                    paths = [ manager-exe ];
                    buildInputs = [ pkgs.makeBinaryWrapper ];
                    postBuild = ''
                      # wrapProgram $out/bin/${manager_} \
                        # --set PATH ${
                          pkgs.lib.makeBinPath managerTools
                          }
                      COMPLETIONS=$out/share/bash-completion/completions
                      MANAGER=${manager-exe}/bin/${manager_}
                      mkdir -p $COMPLETIONS
                      cat <($MANAGER --bash-completion-script $MANAGER) > $COMPLETIONS/${manager_}
                    '';
                  }
                ) "Manage repetitive Haskell modules. Run `manager -h`"
            )
            (
              x: ''
                ${man.DESCRIPTION}
                ${x.meta.description}
              ''
            )
        ;
      in
      {
        packages = {
          default = manager;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [ manager ];
          shellHook = ''
            # source <(manager --bash-completion-script `which manager`)
            # manager
          '';
        };

        stack-shell = { ghcVersion }:

          pkgs.haskell.lib.buildStackProject {
            name = "nix-stack-shell";

            ghc = pkgs.haskell.compiler.${ghcVersion};

            buildInputs = [
              pkgs.zlib
            ] ++ managerTools;
          };
      }
      ) // {
      templates = {
        init = {
          path = ./template;
          description = ''Used by `manager` to initialize a project'';
        };
      };
    };

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
