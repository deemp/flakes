{
  inputs.flakes.url = "github:deemp/flakes";

  outputs =
    inputs:
    let
      inputs_ =
        let flakes = inputs.flakes.flakes; in
        {
          inherit (flakes.source-flake) flake-utils nixpkgs;
          inherit (flakes) drv-tools flakes-tools;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.drv-tools.lib.${system}) writeYAML genAttrsId mkAccessors mkAccessors_ singletonIf;
          inherit (builtins) concatMap filter map;
          testFlakesTools = (inputs.flakes-tools.testFlakesTools.${system});
          inherit (inputs.flakes-tools.lib.${system}) CACHE_DIRECTORY;
          inherit (pkgs.lib.strings) concatMapStringsSep concatStringsSep;
          inherit (pkgs.lib.attrsets) mapAttrsToList recursiveUpdate;
          inherit (pkgs.lib.lists) flatten;

          writeWorkflow = name: writeYAML name ".github/workflows/${name}.yaml";

          expr = expr_: "\${{ ${builtins.toString expr_} }}";

          os = {
            ubuntu-20 = "ubuntu-20.04";
            ubuntu-22 = "ubuntu-22.04";
            macos-11 = "macos-11";
            macos-12 = "macos-12";
          };

          # A list of runner OSs
          oss = __attrValues os;

          # insert if: expression into steps
          # can omit ${{ }} - https://docs.github.com/en/actions/learn-github-actions/expressions#example-expression-in-an-if-conditional

          # include steps conditionally
          # don't use interpolation
          # https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions#jobsjob_idif
          stepsIf = expr: steps: map (x: x // { "if" = expr; }) (flatten steps);

          # make stuff available like matrix.os instead of "matrix.os"
          names =
            mkAccessors
              (
                {
                  secrets = genAttrsId [
                    "CACHIX_CACHE"
                    "CACHIX_AUTH_TOKEN"
                    "SNYK_TOKEN"
                    "GITHUB_TOKEN"
                    "DOCKER_HUB_PAT"
                    "DOCKER_HUB_USERNAME"
                    "HACKAGE_TOKEN"
                  ];
                  github = genAttrsId [
                    "sha"
                  ];
                  matrix = genAttrsId [
                    "os"
                    "store"
                  ];
                  runner = genAttrsId [
                    "os"
                  ];
                }
                // (
                  # from flakes-tools - https://github.com/deemp/flakes/blob/9183df7c07abe9c1f5b4198ef6fb0b979c7af3a3/flakes-tools/flake.nix#L256
                  genAttrsId (mapAttrsToList (name: val: name) testFlakesTools)
                )
              );

          on = {
            # https://crontab.guru/#30_5,17_*_*_*
            schedule = [
              { cron = "0 0 * * *"; }
            ];
            pull_request = { };
            push = { };
            workflow_dispatch = { };
          };

          # TODO don't build scripts when `nix run` gets an option to make a gc root

          run =
            let
              gitPull = ''git pull --rebase --autostash'';
              commit_ =
                { doAdd ? false
                , add ? [ ]
                , doCommit ? false
                , commitMessage ? "commit message"
                , commitMessages ? [ commitMessage ]
                , doPush ? false
                , doIgnoreCommitFailed ? false
                , doIgnorePushFailed ? false
                }: concatStringsSep "\n"
                  (flatten [
                    (singletonIf doAdd "git add ${concatStringsSep " " add}")
                    (singletonIf doCommit
                      (concatStringsSep "" [
                        "git commit \\\n  ${concatMapStringsSep " \\\n  " (message: ''-m "action: ${message}"'') commitMessages}"
                        "${if doIgnoreCommitFailed then ''${" \\\n  "}|| echo "commit failed!"'' else ""}"
                      ])
                    )
                    (singletonIf doPush "git push${if doIgnorePushFailed then '' || echo "push failed!"'' else ""}")
                  ]);

              commit = args: commit_ ({
                doAdd = true;
                add = [ "." ];
                doCommit = true;
                doPush = true;
              } // args
              );

              nix_ =
                { doGitPull ? false
                , dir ? "."
                , # run in directory
                  inDir ? false
                , # script may be from a remote flake
                  remote ? false
                , doBuild ? false
                , doInstall ? false
                , installPriority ? 0
                , doRun ? false
                , scripts ? [ ]
                , doCommit ? false
                , commitArgs ? { }
                }:
                concatStringsSep "\n\n" (flatten [
                  (singletonIf doGitPull gitPull)
                  (singletonIf inDir "cd ${dir}")
                  (map
                    (name:
                    let
                      installable = if remote then name else "${if inDir then "." else dir}#${name}";
                      lines = flatten [
                        (singletonIf doBuild "nix build ${installable}")
                        (singletonIf doInstall "nix profile install ${installable}${if installPriority > 0 then " --priority ${builtins.toString installPriority}" else ""}")
                        (singletonIf doRun "nix run ${installable}")
                      ];
                    in
                    concatStringsSep "\n" lines
                    )
                    scripts
                  )
                  (singletonIf doCommit (commit commitArgs))
                ])
              ;
              nix = args: nix_ ({ doInstall = true; doRun = true; } // args);
              nixScript = args@{ name, ... }: nix ((builtins.removeAttrs args [ "name" ]) // { scripts = [ args.name ]; });
            in
            {
              inherit
                gitPull
                commit
                commit_
                nix
                nix_
                nixScript;
            };

          # cache nix store
          cacheNix_ =
            { files ? [ ]
            , keyJob ? "job"
            , keyOS ? expr names.runner.os
            , path ? ""
            , linuxGCEnabled ? false
            , linuxMaxStoreSize ? 0
            , macosGCEnabled ? false
            , macosMaxStoreSize ? 0
            }:
            let
              hashfilesArgs = concatMapStringsSep ", " (x: "'${x}'") files;
              hashfiles = expr "hashfiles(${hashfilesArgs})";
            in
            {
              name = "Restore and cache Nix store";
              uses = "nix-community/cache-nix-action@v1";
              "with" = {
                key = "nix-${keyOS}-${keyJob}-${hashfiles}";
                restore-keys = ''
                  nix-${keyOS}-${keyJob}-${hashfiles}
                  nix-${keyOS}-${keyJob}-
                '';
              }
              // (if path != "" then { inherit path; } else { })
              // (if linuxGCEnabled then { linux-gc-enabled = true; linux-max-store-size = linuxMaxStoreSize; } else { })
              // (if macosGCEnabled then { macos-gc-enabled = true; macos-max-store-size = macosMaxStoreSize; } else { });
            };

          cacheNix = args: cacheNix_ ({ files = [ "**/flake.nix" "**/flake.lock" ]; } // args);

          # Keep build outputs to garbage collect at the end only the trash
          # https://nixos.org/manual/nix/unstable/command-ref/conf-file.html#description
          installNix =
            { modifyNixConfig ? (x: x)
            , installNixActionVersion ? "v25"
            }: {
              name = "Install Nix";
              uses = "nixbuild/nix-quick-install-action@${builtins.toString installNixActionVersion}";
              "with" = {
                nix_conf = modifyNixConfig ''
                  access-tokens = github.com=${expr names.secrets.GITHUB_TOKEN}
                  substituters = https://cache.nixos.org/ https://nix-community.cachix.org https://cache.iog.io https://deemp.cachix.org
                  trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18=
                  keep-outputs = true
                '';
              };
            };

          steps = rec {
            checkout = {
              name = "Checkout this repo";
              uses = "actions/checkout@v3";
            };

            gitPull = {
              name = "Pull and rebase";
              run = run.gitPull;
            };

            inherit
              cacheNix
              cacheNix_
              installNix;

            logInToCachix = {
              name = "Log in to Cachix";
              run = ''
                nix run nixpkgs#cachix -- authtoken ${ expr names.secrets.CACHIX_AUTH_TOKEN }
              '';
            };

            pushToCachix_ = { dir ? ".", doInstall ? false }: {
              name = "Push flakes to Cachix";
              env = {
                CACHIX_CACHE = expr names.secrets.CACHIX_CACHE;
                CACHIX_AUTH_TOKEN = expr names.secrets.CACHIX_AUTH_TOKEN;
              };
              run = run.nixScript { inherit dir doInstall; name = names.pushToCachix; };
            };

            pushToCachix = args: pushToCachix_ ({ doInstall = true; } // args);

            format_ = { dir ? ".", doInstall ? false }: {
              name = "Format Nix files";
              run = run.nixScript { inherit dir doInstall; name = names.format; };
            };

            format = args: format_ ({ doInstall = true; } // args);

            configGitAsGHActions = {
              name = "Config git for github-actions";
              run = ''
                git config --global user.name github-actions
                git config --global user.email github-actions@github.com
              '';
            };

            updateLocks_ = { dir ? ".", doInstall ? false, doGitPull ? false, doCommit ? false, commitArgs ? { } }:
              let name = "Update flake locks"; in
              {
                inherit name;
                run = run.nixScript ({
                  inherit doCommit doGitPull doInstall dir;
                  commitArgs = {
                    commitMessage = name;
                  } // commitArgs;
                  name = names.updateLocks;
                });
              };

            updateLocks = args: updateLocks_ ({ doInstall = true; doGitPull = true; doCommit = true; } // args);

            nixStoreGC = {
              name = "Collect garbage in /nix/store";
              run = "nix store gc";
            };

            removeCacheProfiles = { dir ? CACHE_DIRECTORY }: {
              name = "Remove old cache profiles";
              run = "rm -rf ${dir}";
            };
          };

          nixCI_ =
            let
              steps_ = steps;
              on_ = on;
            in
            { steps ? (_: [ ])
            , dir ? "."
            , on ? on_
            , defaultOS ? os.ubuntu-22
            , strategy ? { matrix.os = oss; }
            , doCheckOS ? strategy != { }
            , runsOn ? (if doCheckOS then names.matrix.os else defaultOS)
            , installNixArgs ? { }
            , doCacheNix ? false
            , doRemoveCacheProfiles ? false
            , cacheNixArgs ? { }
            , cacheDirectory ? CACHE_DIRECTORY
            , doInstall ? false
            , doFormat ? false
            , formatArgs ? { }
            , doUpdateLocks ? false
            , updateLocksArgs ? { }
            , doPushToCachix ? false
            , pushToCachixArgs ? { }
            }:
            let
              runsOn_ = (if doCheckOS then expr else (x: x)) runsOn;
            in
            {
              name = "Nix CI";
              inherit on;
              jobs = {
                nixCI = {
                  name = "Nix CI";
                  runs-on = runsOn_;
                  steps = flatten
                    [
                      steps_.checkout
                      (installNix installNixArgs)
                      (singletonIf doCacheNix (steps_.cacheNix ({ keyJob = "cachix"; keyOS = runsOn_; } // cacheNixArgs)))
                      (singletonIf doRemoveCacheProfiles (steps_.removeCacheProfiles { dir = cacheDirectory; }))
                      (
                        (if doCheckOS then stepsIf ("${runsOn} == '${defaultOS}'") else (x: x)) [
                          steps_.configGitAsGHActions
                          (singletonIf doFormat (steps_.format ({ inherit dir doInstall; } // formatArgs)))
                          (singletonIf doUpdateLocks (steps_.updateLocks ({ inherit dir doInstall; } // updateLocksArgs)))
                        ]
                      )
                      (steps dir)
                      (singletonIf doPushToCachix (steps_.pushToCachix ({ inherit dir doInstall; } // pushToCachixArgs)))
                    ]
                  ;
                } // (if doCheckOS then { inherit strategy; } else { });
              };
            };

          nixCI = args: nixCI_ (
            recursiveUpdate
              {
                doCacheNix = true;
                cacheNixArgs = {
                  linuxGCEnabled = true;
                  macosGCEnabled = true;
                };
                doRemoveCacheProfiles = true;
                doInstall = true;
                doUpdateLocks = true;
                updateLocksArgs = { doGitPull = true; commitArgs.doIgnoreCommitFailed = true; };
                doPushToCachix = true;
              }
              args);

          packages = {
            writeWorkflowsDir = writeYAML "workflow" "./tmp/nixCI.yaml" (nixCI { });
            writeWorkflows = writeWorkflow "nixCI" (nixCI {
              dir = "nix-dev/";
              cacheNixArgs = {
                linuxMaxStoreSize = 6442450944;
                macosMaxStoreSize = 6442450944;
              };
            });
          };

          devShells.default = pkgs.mkShell {
            buildInputs = __attrValues { inherit (packages) writeWorkflows writeWorkflowsDir; };
          };
        in
        {
          inherit packages devShells;

          lib = {
            inherit
              expr
              genAttrsId
              mkAccessors
              mkAccessors_
              names
              nixCI
              nixCI_
              on
              os
              oss
              run
              steps
              stepsIf
              writeWorkflow
              writeYAML
              ;
          };
        });
    in
    outputs;
}
