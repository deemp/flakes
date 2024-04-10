{
  inputs = {
    nixpkgs.url = "github:deemp/flakes?dir=pins/nixpkgs";
  };

  outputs = inputs:
    import ./makeFlake.nix {
      inherit inputs;
      perSystem = system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};

          inherit (pkgs) lib;

          nix = rec {
            drv-tools = import ./drv-tools { inherit system pkgs; };
            flakes-tools = import ./flakes-tools { inherit system pkgs drv-tools; };
            language-tools.haskell = import ./language-tools/haskell { inherit system pkgs drv-tools; };
            language-tools.purescript = import ./language-tools/purescript { inherit system pkgs drv-tools; };
            json2md = import ./language-tools/purescript { inherit system pkgs drv-tools; };
            workflows = import ./workflows { inherit system pkgs drv-tools; };
          };

          subdirs =
            lib.pipe
              {
                src = [
                  [ "drv-tools" ]
                  [ "flakes-tools" ]
                  [ "language-tools" "haskell" ]
                  [ "language-tools" "purescript" ]
                  [ "workflows" ]
                ];
                attr = [ "packages" "devShells" "lib" "test" ];
              }
              [
                lib.cartesianProductOfSets

                (map (x: {
                  name = x.src;
                  value = lib.getAttrFromPath x.src nix;
                  inherit (x) attr;
                }))

                (map (x:
                  if (x.value?"${x.attr}")
                  then (lib.setAttrByPath ([ x.attr ] ++ x.name) x.value."${x.attr}")
                  else null
                ))

                (lib.filter (x: x != null))

                nix.drv-tools.lib.mergeAttrsRecursive
              ];

          devshell = (pkgs.appendOverlays [ (builtins.getFlake "github:deemp/devshell/${(builtins.fromJSON (builtins.readFile ./pins/devshell/flake.lock)).nodes.devshell.locked.rev}").overlays.default ]).devshell;

          packages = subdirs.lib.drv-tools.mkShellApps {
            inherit (subdirs.lib.flakes-tools.mkFlakesTools {
              root = ./.;
              dirs = [
                "codium"
                "templates/haskell-minimal"
                "."
              ];
              subDirs = [
                "templates/codium"
                "pins"
              ];
            }) saveFlakes format updateLocks;

            genDocs = {
              text = ''
                mkdir -p docs/src
                cp README/*.md docs/src
                ${lib.getExe' pkgs.mdbook "mdbook"} build docs
              '';
              description = "Generate docs";
            };
          };

          devShells.default = devshell.mkShell {
            commands = {
              scripts = [
                {
                  prefix = "nix run .#";
                  packages = { inherit (packages) genDocs; };
                }
              ];
              flakes-tools = [
                {
                  prefix = "nix run .#";
                  packages = { inherit (packages) saveFlakes format updateLocks; };
                }
              ];
            };
          };

          formatter = pkgs.nixpkgs-fmt;
        in
        lib.recursiveUpdate subdirs { inherit packages devShells formatter; };

      other = rec {
        makeFlake = import ./makeFlake.nix;
        makeDefault = import ./makeDefault.nix;

        # no overlay is provided because it's easy to override nixpkgs via flake inputs

        # let makeFlake = import ./makeFlake.nix; in
        # makeFlake {
        #   inputs = {
        #     inherit (import ./source-flake) nixpkgs formatter;
        #     codium = import ./codium;
        #     devshell = import ./devshell;
        #     drv-tools = import ./drv-tools { inherit (inputs) nixpkgs; };
        #     flakes-tools = import ./flakes-tools;
        #     workflows = import ./workflows;
        #   };
        #   perSystem = { inputs, system }:
        #     let
        #       pkgs = inputs.nixpkgs.legacyPackages.${system};
        #       inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON extensionsCommon settingsCommonNix;
        #       inherit (inputs.drv-tools.lib.${system}) withAttrs mkShellApps getExe getExe';
        #       inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
        #       inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
        #       inherit (inputs.workflows.lib.${system}) writeWorkflow nixCI expr steps names run stepsIf os;

        #       writeInputs = lockPath:
        #         with pkgs.lib;
        #         pkgs.writeText "inputs" (
        #           concatStringsSep "\n" (
        #             mapAttrsToList
        #               (_: value: (builtins.fetchTree value.locked).outPath)
        #               (
        #                 filterAttrs
        #                   (name: _: name != "root")
        #                   (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes
        #               )
        #           )
        #         );

        #       # TODO
        #       # remove devshell flake (mkRunCommands is deprecated)
        #       # convert flakes to lib.<name>
        #       # make the save function accept a list of (derivation or a path to a lock file)
        #       # inputs.flakes.lib.flakes-tools.writeInputs

        #       packages = mkShellApps {
        #         inp = writeInputs ./flake.lock;

        #         inherit (mkFlakesTools { root = ./.; dirs = [ "source-flake" "codium" ]; }) pushToCachix;
        #         inherit (flakesTools) saveFlakes format updateLocks;

        #         writeSettings = writeSettingsJSON settingsCommonNix;
        #         codium = mkCodium ({ extensions = extensionsCommon; });

        #         writeWorkflows = writeWorkflow "ci" (withAttrs
        #           (nixCI {
        #             jobArgs = {
        #               cacheNixArgs = {
        #                 linuxGCEnabled = true;
        #                 linuxMaxStoreSize = 5000000000;
        #                 macosGCEnabled = true;
        #                 macosMaxStoreSize = 5000000000;
        #               };
        #               doFormat = true;
        #               doPushToCachix = true;
        #               doCommit = false;
        #               steps = { dir, stepsAttrs }:
        #                 stepsIf ("${names.matrix.os} == '${os.ubuntu-22}'") [
        #                   {
        #                     name = "Write workflows";
        #                     run = pkgs.lib.strings.concatMapStringsSep "\n\n"
        #                       (dir_: "${run.nixScript { dir = dir_; inDir = true; name = "writeWorkflows"; }}")
        #                       [ "templates/codium/haskell" "templates/codium/haskell-simple" "workflows" ]
        #                     ;
        #                   }
        #                   {
        #                     name = "Update docs";
        #                     run = run.nixScript { name = getExe packages.genDocs; };
        #                   }
        #                   (steps.commit {
        #                     messages = [
        #                       (steps.updateLocks { }).name
        #                       (steps.format { }).name
        #                       stepsAttrs."Write workflows".name
        #                       stepsAttrs."Update docs".name
        #                     ];
        #                   })
        #                   {
        #                     name = "Copy docs";
        #                     run = "cp -r docs/book docs/dist";
        #                   }
        #                   {
        #                     name = "Publish docs on GitHub Pages";
        #                     uses = "peaceiris/actions-gh-pages@v3.9.3";
        #                     "with" = {
        #                       github_token = expr names.secrets.GITHUB_TOKEN;
        #                       publish_dir = "docs/dist";
        #                       force_orphan = true;
        #                     };
        #                   }
        #                 ];
        #             };
        #           })
        #           { on.schedule = [{ cron = "0 0 * * 0"; }]; });
        #       };

        #       tools = [ ];
        #       devShells.default = mkShell {
        #         packages = tools;
        #         commands =
        #           mkCommands "tools" tools
        #           ++ mkRunCommands "ide" { inherit (packages) writeSettings; "codium ." = packages.codium; }
        #           ++ mkRunCommands "infra" { inherit (packages) writeWorkflows saveFlakes format updateLocks; }
        #           ++ mkRunCommands "docs" { inherit (packages) genDocs; };
        #       };
        #     in
        #     {
        #       inherit packages devShells;
        #     };
        #   raw = inputs: {
        #     all = (import ./source-flake) // {
        #       codium = import ./codium;
        #       devshell = import ./devshell;
        #       drv-tools = import ./drv-tools;
        #       env2json = import ./env2json;
        #       flakes-tools = import ./flakes-tools;
        #       haskell-tools = import ./language-tools/haskell;
        #       json2md = import ./json2md;
        #       purescript-tools = import ./language-tools/purescript;
        #       python-tools = import ./language-tools/python;
        #       workflows = import ./workflows;
        #     };
        #     inherit makeFlake;
        #     makeDefault = import ./makeDefault.nix;
        #     makeShell = src: (import (import ./source-flake).outputs.flake-compat { inherit src; }).shellNix;
        # };

        templates = rec {
          codium-generic = {
            path = ./templates/codium/generic;
            description = "`VSCodium` with extensions and executables";
          };
          codium-haskell = {
            path = ./templates/codium/haskell;
            description = "${codium-generic.description} for `Haskell`. Shows 5 ways to run a `Haskell` app.";
          };
          codium-haskell-simple = {
            path = ./templates/codium/haskell-simple;
            description = "${codium-generic.description} for `Haskell`.";
          };
          haskell-minimal = {
            path = ./templates/haskell-minimal;
            description = "Minimal flake for a `Haskell` package development.";
          };
          codium-python = {
            path = ./templates/codium/python;
            description = "${codium-generic.description} for `Python`.";
          };
        };
      };
    };

  nixConfig = {
    extra-trusted-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
