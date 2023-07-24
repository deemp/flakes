{
  outputs = inputs:
    let makeFlake = import ./makeFlake.nix; in
    makeFlake {
      inputs = {
        inherit (import ./source-flake) nixpkgs formatter;
        codium = import ./codium;
        devshell = import ./devshell;
        drv-tools = import ./drv-tools;
        flakes-tools = import ./flakes-tools;
        workflows = import ./workflows;
      };
      perSystem = { inputs, system }:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON extensionsCommon settingsCommonNix;
          inherit (inputs.drv-tools.lib.${system}) subDirectories withAttrs mkShellApps getExe;
          inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
          inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
          inherit (inputs.workflows.lib.${system}) writeWorkflow nixCI expr steps names run stepsIf os;

          flakesTools = (mkFlakesTools {
            root = ./.;
            dirs = [
              "codium"
              "devshell"
              "drv-tools"
              "env2json"
              "flakes-tools"
              "json2md"
              "templates/haskell-minimal"
              "workflows"
              "source-flake"
              "."
            ];
            subDirs = [
              "language-tools"
              "templates/codium"
            ];
          });

          packages =
            let
              packages1 = mkShellApps {
                genDocs = {
                  text = ''
                    mkdir -p docs/src
                    cp README/*.md docs/src
                    ${getExe pkgs.mdbook} build docs
                  '';
                  description = "Generate docs";
                };
              };

              packages2 = {
                inherit (mkFlakesTools { root = ./.; dirs = [ "source-flake" "codium" ]; }) pushToCachix;
                inherit (flakesTools) saveFlakes format updateLocks;
                writeSettings = writeSettingsJSON settingsCommonNix;
                codium = mkCodium ({ extensions = extensionsCommon; });
                writeWorkflows = writeWorkflow "ci" (withAttrs
                  (nixCI {
                    jobArgs = {
                      cacheNixArgs = {
                        linuxGCEnabled = true;
                        linuxMaxStoreSize = 5000000000;
                        macosGCEnabled = true;
                        macosMaxStoreSize = 5000000000;
                      };
                      doFormat = true;
                      doPushToCachix = true;
                      doCommit = false;
                      steps = dirs:
                        stepsIf ("${names.matrix.os} == '${os.ubuntu-22}'") [
                          (
                            let
                              nameWriteWorkflows = "Write workflows";
                              nameUpdateDocs = "Update docs";
                            in
                            [
                              {
                                name = nameWriteWorkflows;
                                run = pkgs.lib.strings.concatMapStringsSep "\n\n"
                                  (dir: "${run.nixScript { inherit dir; inDir = true; name = "writeWorkflows"; }}")
                                  [ "templates/codium/haskell" "templates/codium/haskell-simple" "workflows" ]
                                ;
                              }
                              {
                                name = nameUpdateDocs;
                                run = run.nixScript { name = packages1.genDocs.pname; };
                              }
                              (steps.commit {
                                messages = [
                                  (steps.updateLocks { }).name
                                  (steps.format { }).name
                                  nameWriteWorkflows
                                  nameUpdateDocs
                                ];
                              })
                            ]
                          )
                          {
                            name = "Copy docs";
                            run = "cp -r docs/book docs/dist";
                          }
                          {
                            name = "Publish docs on GitHub Pages";
                            uses = "peiris/actions-gh-pages@v3.9.3";
                            "with" = {
                              github_token = expr names.secrets.GITHUB_TOKEN;
                              publish_dir = "docs/dist";
                              force_orphan = true;
                            };
                          }
                        ];
                    };
                  })
                  { on.schedule = [{ cron = "0 0 * * 0"; }]; });
              };
            in
            packages1 // packages2;

          tools = [ ];
          devShells.default = mkShell {
            packages = tools;
            commands =
              mkCommands "tools" tools
              ++ mkRunCommands "ide" { inherit (packages) writeSettings; "codium ." = packages.codium; }
              ++ mkRunCommands "infra" { inherit (packages) writeWorkflows; }
              ++ mkRunCommands "docs" { inherit (packages) genDocs; };
          };
        in
        {
          inherit packages devShells;
          formatter = inputs.formatter.${system};
        };
      raw = inputs: {
        all = (import ./source-flake) // {
          codium = import ./codium;
          devshell = import ./devshell;
          drv-tools = import ./drv-tools;
          env2json = import ./env2json;
          flakes-tools = import ./flakes-tools;
          haskell-tools = import ./language-tools/haskell;
          json2md = import ./json2md;
          purescript-tools = import ./language-tools/purescript;
          python-tools = import ./language-tools/python;
          workflows = import ./workflows;
        };
        inherit makeFlake;
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
