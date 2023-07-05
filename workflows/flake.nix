{
  inputs.flakes.url = "github:deemp/flakes";

  outputs =
    inputs:
    let
      inputs_ =
        let flakes = inputs.flakes.flakes; in
        {
          inherit (flakes.source-flake) flake-utils nixpkgs;
          inherit (flakes) drv-tools;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.drv-tools.lib.${system}) writeYAML genAttrsId mkAccessors mkAccessors_;
          inherit (builtins) concatMap;
          inherit (pkgs.lib.strings) concatMapStringsSep;

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
          stepsIf = expr: steps: map (x: x // { "if" = expr; }) steps;

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
                  genAttrsId [ "pushToCachix" "updateLocks" ]
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
                }: ''git commit -a ${concatMapStringsSep " \\\n  " (message: ''-m "action: ${message}"'') commitMessages} && git push'';
              nix =
                { doGitPull ? false
                , dir ? "."
                , # run in directory
                  inDir ? false
                , # script may be from a remote flake
                  remote ? false
                , doBuild ? false
                , doRun ? true
                , scripts ? [ ]
                , doCommit ? false
                , commitMessage ? "run scripts"
                , commitMessages ? [ commitMessage ]
                }:
                (if doGitPull then "${gitPull}\n\n" else "") +
                (if inDir then "cd ${dir}\n\n" else "") +
                "${concatMapStringsSep
                  "\n"
                  (name:
                    let installable = if remote then name else "${if inDir then "." else dir}#${name}"; in
                    (if doBuild then "nix build ${installable}\n" else "") + (if doRun then "nix run ${installable}\n" else "")
                  )
                  scripts
             }${if builtins.length scripts > 0 then "\n" else ""}" +
                (if doCommit then "${commit {inherit commitMessages;}}\n" else "")
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
            , debug-enabled ? false
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
              // (if debug-enabled then { inherit debug-enabled; } else { });
            }
          ;

          # Keep build outputs to garbage collect at the end only the trash
          # https://nixos.org/manual/nix/unstable/command-ref/conf-file.html#description
          installNix =
            { modifyExtraNixConfig ? (x: x)
            , installNixActionVersion ? "v22"
            , nixVersion ? "2.16.1"
            }: {
              name = "Install Nix";
              uses = "cachix/install-nix-action@${builtins.toString installNixActionVersion}";
              "with" = {
                extra_nix_config = modifyExtraNixConfig ''
                  access-tokens = github.com=${expr names.secrets.GITHUB_TOKEN}
                  substituters = https://cache.nixos.org/ https://nix-community.cachix.org https://cache.iog.io https://deemp.cachix.org
                  trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18=
                  keep-outputs = true
                  keep-derivations = true
                '';
                # https://releases.nixos.org/?prefix=nix
                install_url = "https://releases.nixos.org/nix/nix-${nixVersion}/install";
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
            pushToCachix = dir: {
              name = "Push flakes to Cachix";
              env = {
                CACHIX_CACHE = expr names.secrets.CACHIX_CACHE;
                CACHIX_AUTH_TOKEN = expr names.secrets.CACHIX_AUTH_TOKEN;
              };
              run = run.nixScript { inherit dir; name = names.pushToCachix; };
            };
            configGitAsGHActions = {
              name = "Config git for github-actions";
              run = ''
                git config user.name github-actions
                git config user.email github-actions@github.com
              '';
            };
            updateLocks = { doCommit ? true, doGitPull ? true, dir ? "." }:
              let name = "Update flake locks"; in
              {
                inherit name;
                run = run.nixScript ({
                  inherit doCommit doGitPull dir;
                  name = names.updateLocks;
                  commitMessage = name;
                });
              };
            nixStoreCollectGarbage = {
              name = "Collect garbage in /nix/store";
              run = "nix store gc";
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
            , cacheNixArgs ? { }
            , doUpdateLocks ? true
            , updateLocksArgs ? { }
            , doPushToCachix ? true
            }: {
              name = "Nix CI";
              inherit on;
              jobs = {
                nixCI = {
                  name = "Nix CI";
                  inherit strategy;
                  runs-on = expr names.matrix.os;
                  steps =
                    [
                      steps_.checkout
                      (installNix installNixArgs)
                    ]
                    ++ (
                      if doCacheNix
                      then [ (steps_.cacheNix ({ keyJob = "cachix"; keyOs = expr names.matrix.os; } // cacheNixArgs)) ]
                      else [ ]
                    )
                    ++ (
                      if doUpdateLocks
                      then
                        stepsIf ("${names.matrix.os} == '${os}'") [
                          steps_.configGitAsGHActions
                          (steps_.updateLocks ({ inherit dir; } // updateLocksArgs))
                        ]
                      else [ ]
                    )
                    ++ (steps dir)
                    ++ (if doPushToCachix then [ (steps_.pushToCachix dir) ] else [ ])
                  ;
                };
              };
            };

          packages = {
            writeWorkflowsDir = writeYAML "workflow" "./tmp/nixCI.yaml" (nixCI { });
            writeWorkflows = writeWorkflow "nixCI" (nixCI { dir = "nix-dev/"; });
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
