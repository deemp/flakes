{
  inputs = { };
  outputs = inputs@{ self, ... }:
    let
      inputs_ = {
        inherit (import ./source-flake) nixpkgs flake-utils formatter;
        drv-tools = import ./drv-tools;
        flakes-tools = import ./flakes-tools;
        codium = import ./codium;
        devshell = import ./devshell;
        workflows = import ./workflows;
      };

      outputs = outputs_ { } // {
        inputs = inputs_;
        outputs = outputs_;
        flakes = {
          codium = import ./codium;
          devshell = import ./devshell;
          drv-tools = import ./drv-tools;
          env2json = import ./env2json;
          flakes-tools = import ./flakes-tools;
          json2md = import ./json2md;
          language-tools = {
            haskell = import ./language-tools/haskell;
            purescript = import ./language-tools/purescript;
            python = import ./language-tools/python;
          };
          source-flake = import ./source-flake;
          workflows = import ./workflows;
        };
      };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem
          (system:
          let
            pkgs = inputs.nixpkgs.legacyPackages.${system};
            inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON extensionsCommon settingsCommonNix;
            inherit (inputs.drv-tools.lib.${system}) subDirectories withAttrs mkShellApps getExe;
            inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
            inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
            workflows = (inputs.workflows.lib.${system});

            flakesTools = (mkFlakesTools {
              root = self.outPath;
              dirs =
                [
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

                inherit (workflows) writeWorkflow nixCI expr steps names run stepsIf os;

                packages2 = {
                  inherit (flakesTools) pushToCachix format updateLocks;
                  writeSettings = writeSettingsJSON settingsCommonNix;
                  codium = mkCodium ({ extensions = extensionsCommon; });
                  writeWorkflows = writeWorkflow "ci" (withAttrs
                    (nixCI {
                      cacheNixArgs = {
                        linuxMaxStoreSize = 5000000000;
                        macosMaxStoreSize = 5000000000;
                      };
                      updateLocksArgs = { doCommit = false; doGitPull = false; };
                      doFormat = true;
                      steps = dirs:
                        stepsIf ("${names.matrix.os} == '${os.ubuntu-22}'") [
                          (
                            let name = "Write workflows"; in
                            [
                              {
                                inherit name;
                                run =
                                  pkgs.lib.strings.concatMapStringsSep "\n\n"
                                    (dir: "${run.nixScript { inherit dir; inDir = true; name = "writeWorkflows"; }}")
                                    [ "templates/codium/haskell" "templates/codium/haskell-simple" "workflows" ]
                                ;
                              }
                              {
                                name = "Commit and push";
                                run = ''
                                  ${run.nix_ {
                                    doGitPull = true; doCommit = true;
                                    commitArgs.commitMessages = [ (steps.updateLocks { }).name (steps.format { }).name name ];
                                  }}
                                '';
                              }
                            ]
                          )
                          {
                            name = "Build docs";
                            run = ''
                              ${run.nixScript {name = packages1.genDocs.pname;}}
                              cp -r docs/book docs/dist
                            '';
                          }
                          {
                            name = "Update docs";
                            run = ''
                              git add "docs/src"
                              git commit -a -m "Update docs" && git push || echo "push failed!"
                            '';
                          }
                          {
                            name = "GitHub Pages action";
                            uses = "peaceiris/actions-gh-pages@v3.9.3";
                            "with" = {
                              github_token = expr names.secrets.GITHUB_TOKEN;
                              publish_dir = "docs/dist";
                              force_orphan = true;
                            };
                          }
                          {
                            name = "Remove dist dir";
                            run = ''
                              rm -rf "docs/dist"
                            '';
                          }
                        ];
                    })
                    { on.schedule = [{ cron = "0 0 * * 0"; }]; });
                };
              in
              packages1 // packages2;

            tools = [ pkgs.nixd ];
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
          })
        // {
          inherit (inputs) formatter;
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
    in
    outputs;

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
