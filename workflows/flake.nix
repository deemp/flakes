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
          inherit (pkgs.lib.attrsets) mapAttrsToList;
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
              commit =
                { commitMessage ? ""
                , commitMessages ? [ commitMessage ]
                , doIgnorePushFailed ? true
                }: "git commit -a ${concatMapStringsSep " \\\n  " (message: ''-m "action: ${message}"'') commitMessages} \\
                       && git push ${if doIgnorePushFailed then ''|| echo "push failed!"'' else ""}";
              nix =
                { doGitPull ? false
                , dir ? "."
                , # run in directory
                  inDir ? false
                , # script may be from a remote flake
                  remote ? false
                , doBuild ? false
                , doInstall ? true
                , installPriority ? 0
                , doRun ? true
                , scripts ? [ ]
                , doCommit ? false
                , commitMessage ? "run scripts"
                , commitMessages ? [ commitMessage ]
                , doIgnorePushFailed ? true
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
                  (singletonIf doCommit (commit { inherit commitMessages; }))
                ])
              ;
              nixScript = args@{ name, ... }: nix ((builtins.removeAttrs args [ "name" ]) // { scripts = [ args.name ]; });
            in
            {
              inherit gitPull commit nix nixScript;
            };

          # cache nix store
          cacheNix =
            { files ? [ "**/flake.nix" "**/flake.lock" ]
            , keyJob ? "job"
            , keyOs ? expr names.runner.os
            , path ? ""
            , linuxGCEnabled ? true
            , linuxMaxStoreSize ? 0
            , macosGCEnabled ? true
            , macosMaxStoreSize ? 0
            }:
            let
              hashfilesArgs = concatMapStringsSep ", " (x: "'${x}'") files;
              hashfiles = expr "hashfiles(${hashfilesArgs})";
            in
            {
              name = "Restore and cache Nix store";
              uses = "deemp/cache-nix-too@v1";
              "with" = {
                key = "nix-${keyOs}-${keyJob}-${hashfiles}";
                restore-keys = ''
                  nix-${keyOs}-${keyJob}-${hashfiles}
                  nix-${keyOs}-${keyJob}-
                '';
              }
              // (if path != "" then { inherit path; } else { })
              // (if linuxGCEnabled then { linux-gc-enabled = true; linux-max-store-size = linuxMaxStoreSize; } else { })
              // (if macosGCEnabled then { macos-gc-enabled = true; macos-max-store-size = macosMaxStoreSize; } else { });
            };

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
            inherit cacheNix installNix;
            logInToCachix = {
              name = "Log in to Cachix";
              run = ''
                nix run nixpkgs#cachix -- authtoken ${ expr names.secrets.CACHIX_AUTH_TOKEN }
              '';
            };
            pushToCachix = { dir ? ".", doInstall ? true }: {
              name = "Push flakes to Cachix";
              env = {
                CACHIX_CACHE = expr names.secrets.CACHIX_CACHE;
                CACHIX_AUTH_TOKEN = expr names.secrets.CACHIX_AUTH_TOKEN;
              };
              run = run.nixScript { inherit dir doInstall; name = names.pushToCachix; };
            };
            format = { dir ? ".", doInstall ? true }: {
              name = "Format Nix files";
              run = run.nixScript { inherit dir doInstall; name = names.format; };
            };
            configGitAsGHActions = {
              name = "Config git for github-actions";
              run = ''
                git config --global user.name github-actions
                git config --global user.email github-actions@github.com
              '';
            };
            updateLocks = { doCommit ? true, doGitPull ? true, doInstall ? true, dir ? "." }:
              let name = "Update flake locks"; in
              {
                inherit name;
                run = run.nixScript ({
                  inherit doCommit doGitPull doInstall dir;
                  name = names.updateLocks;
                  commitMessage = name;
                });
              };
            nixStoreGC = {
              name = "Collect garbage in /nix/store";
              run = "nix store gc";
            };
            removeCacheProfiles = { dir ? CACHE_DIRECTORY }: {
              name = "Remove old cache profiles";
              run = "rm -rf ${dir}";
            };
          };

          nixCI =
            let
              steps_ = steps;
              on_ = on;
              os_ = os;
            in
            { steps ? (_: [ ])
            , dir ? "."
            , on ? on_
            , os ? os_.ubuntu-22
            , strategy ? { matrix.os = oss; }
            , installNixArgs ? { }
            , doCacheNix ? true
            , doRemoveCacheProfiles ? true
            , cacheNixArgs ? { }
            , cacheDirectory ? CACHE_DIRECTORY
            , doInstall ? true
            , doFormat ? false
            , formatArgs ? { }
            , doUpdateLocks ? true
            , updateLocksArgs ? { }
            , doIgnorePushFailed ? true
            , doPushToCachix ? true
            , pushToCachixArgs ? { }
            }: {
              name = "Nix CI";
              inherit on;
              jobs = {
                nixCI = {
                  name = "Nix CI";
                  inherit strategy;
                  runs-on = expr names.matrix.os;
                  steps = flatten
                    [
                      steps_.checkout
                      (installNix installNixArgs)
                      (singletonIf doCacheNix (steps_.cacheNix ({ keyJob = "cachix"; keyOs = expr names.matrix.os; } // cacheNixArgs)))
                      (singletonIf doRemoveCacheProfiles (steps_.removeCacheProfiles { dir = cacheDirectory; }))
                      (
                        stepsIf ("${names.matrix.os} == '${os}'") [
                          steps_.configGitAsGHActions
                          (singletonIf doFormat (steps_.format ({ inherit dir doInstall; } // formatArgs)))
                          (singletonIf doUpdateLocks (steps_.updateLocks ({ inherit dir doInstall; } // updateLocksArgs)))
                        ]
                      )
                      (steps dir)
                      (singletonIf doPushToCachix (steps_.pushToCachix ({ inherit dir doInstall; } // pushToCachixArgs)))
                    ]
                  ;
                };
              };
            };

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
