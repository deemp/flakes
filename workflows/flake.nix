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
          inherit (pkgs.lib.attrsets) mapAttrsToList recursiveUpdate hasAttrByPath;
          inherit (pkgs.lib.lists) flatten imap0;

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

          run =
            let
              gitPull = ''git pull --rebase --autostash'';
              commit_ =
                { doGitPull ? false
                , doAdd ? false
                , add ? [ ]
                , doCommit ? false
                , doMessageFirst ? false
                , messageFirst ? "action"
                , messagePrefix ? "action: "
                , message ? "commit message"
                , messages ? [ message ]
                , doIgnoreCommitFailed ? false
                , doPush ? false
                , doIgnorePushFailed ? false
                }: concatStringsSep "\n\n"
                  (flatten [
                    (singletonIf doGitPull gitPull)
                    (singletonIf doAdd "git add ${concatStringsSep " " add}")
                    (singletonIf doCommit
                      (concatStringsSep "" [
                        "git commit \\\n  ${
                          concatMapStringsSep " \\\n  "
                          (message: ''-m "${messagePrefix}${message}"'') 
                          (flatten [(singletonIf doMessageFirst messageFirst) messages])
                        }"
                        "${if doIgnoreCommitFailed then ''${" \\\n  "}|| echo "commit failed!"'' else ""}"
                      ])
                    )
                    (singletonIf doPush "git push${if doIgnorePushFailed then " || echo \"push failed!\"" else ""}")
                  ]);

              commit = args: commit_ (
                {
                  doGitPull = true;
                  doAdd = true;
                  add = [ "." ];
                  doCommit = true;
                  doMessageFirst = true;
                  messagePrefix = "";
                  doIgnoreCommitFailed = true;
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
                , scripts ? [ ]
                , doBuild ? false
                , doInstall ? false
                , doInstallPriority ? (builtins.length scripts) > 1
                , # starting install priority
                  installPriority ? 0
                , doRun ? false
                , doCommit ? false
                , commitArgs ? { }
                }:
                concatStringsSep "\n\n" (flatten [
                  (singletonIf doGitPull gitPull)
                  (
                    imap0
                      (
                        i: name:
                        let
                          installable = if remote then name else "${if inDir then "." else dir}#${name}";
                          lines = flatten [
                            (singletonIf inDir "ROOT_DIR=$PWD\ncd ${dir}")
                            (singletonIf doBuild "nix build ${installable}")
                            (singletonIf doInstall "nix profile install ${installable}${if doInstallPriority then " --priority ${builtins.toString (i + installPriority)}" else ""}")
                            (singletonIf doRun "nix run ${installable}")
                            (singletonIf inDir "cd $ROOT_DIR")
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
              nixScript = args@{ name ? "script", ... }: nix ((builtins.removeAttrs args [ "name" ]) // { scripts = [ args.name ]; });
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
                  nix-${keyOS}-${keyJob}-
                '';
              }
              // (if path != "" then { inherit path; } else { })
              // (if linuxGCEnabled then { linux-gc-enabled = true; linux-max-store-size = linuxMaxStoreSize; } else { })
              // (if macosGCEnabled then { macos-gc-enabled = true; macos-max-store-size = macosMaxStoreSize; } else { });
            };

          # don't enable GC to not disable for macos
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
              name = "Pull latest repo changes";
              run = run.gitPull;
            };

            commit = args: {
              name = "Commit & Push";
              run = run.commit args;
            };

            inherit
              cacheNix
              cacheNix_
              installNix;

            purgeCache_ = { debug ? false, accessed ? false, created ? false, maxAge ? 0 }: {
              name = "Purge cache";
              uses = "deemp/purge-cache@v1";
              "with" = {
                inherit debug accessed created;
                max-age = maxAge;
              };
            };

            purgeCache = attrs: purgeCache_ ({ debug = true; created = true; maxAge = 172800; } // attrs);

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

            saveFlakes_ = { dir ? ".", doInstall ? false }: {
              name = "Save flakes";
              run = run.nixScript { inherit dir doInstall; name = names.saveFlakes; };
            };

            saveFlakes = args: saveFlakes_ ({ doInstall = true; } // args);

            configGitAsGHActions = {
              name = "Configure git for github-actions";
              run = ''
                git config --global user.name github-actions
                git config --global user.email github-actions@github.com
              '';
            };

            updateLocks_ = { dir ? ".", doInstall ? false }: {
              name = "Update flake locks";
              run = run.nixScript { name = names.updateLocks; inherit doInstall dir; };
            };

            updateLocks = args: updateLocks_ ({ doInstall = true; } // args);

            nixStoreGC = {
              name = "Collect garbage in /nix/store";
              run = "nix store gc";
            };

            removeCacheProfiles = { dir ? CACHE_DIRECTORY }: {
              name = "Remove old cache profiles";
              run = "rm -rf ${dir}";
            };
          };

          # if there are multiple jobs that need caches
          # it may be better to purge caches only in the last one
          job_ =
            let steps_ = steps; in
            { dir ? "."
            , steps ? (_: [ ])
            , defaultOS ? os.ubuntu-22
            , name ? "Nix CI"
            , strategy ? { matrix.os = oss; }
            , doMatrixOS ? hasAttrByPath [ "matrix" "os" ] strategy
            , runsOn ? (if doMatrixOS then names.matrix.os else defaultOS)
            , permissions ? { }
            , installNixArgs ? { }
            , doCacheNix ? false
            , doPurgeCache ? false
            , purgeCacheArgs ? { }
            , doRemoveCacheProfiles ? false
            , cacheNixArgs ? { }
            , cacheDirectory ? CACHE_DIRECTORY
            , doInstall ? false
            , doFormat ? false
            , formatArgs ? { }
            , doUpdateLocks ? false
            , updateLocksArgs ? { }
            , doCommit ? false
            , commitArgs ? { }
            , doSaveFlakes ? false
            , saveFlakesArgs ? { }
            , doPushToCachix ? false
            , pushToCachixArgs ? { }
            }:
            let
              runsOn_ = (if doMatrixOS then expr else (x: x)) runsOn;
            in
            {
              inherit name;
              runs-on = runsOn_;
              permissions = { contents = "write"; actions = "write"; } // permissions;
              steps = flatten [
                steps_.checkout
                (installNix installNixArgs)
                (singletonIf doCacheNix (steps_.cacheNix ({ keyOS = runsOn_; } // cacheNixArgs)))
                (singletonIf doRemoveCacheProfiles (steps_.removeCacheProfiles { dir = cacheDirectory; }))
                (
                  (if doMatrixOS then stepsIf ("${runsOn} == '${defaultOS}'") else (x: x)) [
                    steps_.configGitAsGHActions
                    (singletonIf doUpdateLocks (steps_.updateLocks ({ inherit dir doInstall; } // updateLocksArgs)))
                    (singletonIf doFormat (steps_.format ({ inherit dir doInstall; } // formatArgs)))
                    (singletonIf doCommit (steps_.commit ({
                      messages = [
                        (singletonIf doFormat (steps_.format { }).name)
                        (singletonIf doUpdateLocks (steps_.updateLocks { }).name)
                      ];
                    } // commitArgs)))
                  ]
                )
                (steps dir)
                (singletonIf doSaveFlakes (steps_.saveFlakes ({ inherit dir doInstall; } // saveFlakesArgs)))
                (singletonIf doPushToCachix (steps_.pushToCachix ({ inherit dir doInstall; } // pushToCachixArgs)))
                (singletonIf doPurgeCache (steps_.purgeCache purgeCacheArgs))
              ];
            } // (if strategy != { } then { inherit strategy; } else { });

          job = args: job_ (
            {
              doCacheNix = true;
              doRemoveCacheProfiles = true;
              doInstall = true;
              doPurgeCache = true;
            }
            //
            args
          );

          nixCI_ =
            let
              steps_ = steps;
              on_ = on;
            in
            { actionName ? "Nix CI"
            , on ? on_
            , jobId ? "default"
            , jobArgs ? { }
            }:
            {
              name = actionName;
              inherit on;
              jobs = {
                "${jobId}" = job (
                  # recursiveUpdate, because there are default nested attributes
                  {
                    doUpdateLocks = true;
                    doFormat = true;
                    doCommit = true;
                    doSaveFlakes = true;
                  }
                  //
                  jobArgs
                );
              };
            };

          # This action config should be used for a default job
          nixCI = args: nixCI_ ({ jobId = "nixCI"; } // args);

          packages = {
            writeWorkflowsDir = writeYAML "workflow" "./tmp/nixCI.yaml" (nixCI { });
            writeWorkflows = writeWorkflow "nixCI" (nixCI {
              jobArgs = {
                dir = "nix-dev/";
                doPushToCachix = true;
                cacheNixArgs = {
                  linuxGCEnabled = true;
                  linuxMaxStoreSize = 6442450944;
                  macosGCEnabled = true;
                  macosMaxStoreSize = 6442450944;
                };
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
              job
              job_
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
