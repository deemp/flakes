{ system ? builtins.currentSystem
, pkgs ? (import ../.).inputs.nixpkgs.legacyPackages.${system}
, drv-tools ? (import ../drv-tools { inherit system pkgs; })
, flakes-tools ? (import ../flakes-tools { inherit system pkgs drv-tools; })
}:
let
  lib = drv-tools.lib.mergeAttrsRecursive [ builtins pkgs.lib drv-tools.lib flakes-tools.lib ];

  writeWorkflow = name: lib.writeYAML name ".github/workflows/${name}.yaml";

  expr = expr_: "\${{ ${lib.toString expr_} }}";

  os = {
    ubuntu-20 = "ubuntu-20.04";
    ubuntu-22 = "ubuntu-22.04";
    macos-11 = "macos-11";
    macos-12 = "macos-12";
  };

  # A list of runner OSs
  oss = lib.attrValues os;

  # insert if: expression into steps
  # can omit ${{ }} - https://docs.github.com/en/actions/learn-github-actions/expressions#example-expression-in-an-if-conditional

  # include steps conditionally
  # don't use interpolation
  # https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions#jobsjob_idif
  stepsIf = expr: steps: map (x: x // { "if" = expr; }) (lib.flatten steps);

  # make stuff available like matrix.os instead of "matrix.os"
  names =
    lib.mkAccessors
      (
        {
          secrets = lib.genAttrsId [
            "CACHIX_CACHE"
            "CACHIX_AUTH_TOKEN"
            "SNYK_TOKEN"
            "GITHUB_TOKEN"
            "DOCKER_HUB_PAT"
            "DOCKER_HUB_USERNAME"
            "HACKAGE_TOKEN"
          ];
          github = lib.genAttrsId [
            "sha"
          ];
          matrix = lib.genAttrsId [
            "os"
            "store"
          ];
          runner = lib.genAttrsId [
            "os"
          ];
        }
        // (lib.genAttrsId (lib.mapAttrsToList (name: val: name) flakes-tools.test))
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
        }: lib.concatStringsSep "\n\n"
          (lib.flatten [
            (lib.singletonIf doGitPull gitPull)
            (lib.singletonIf doAdd "git add ${lib.concatStringsSep " " add}")
            (lib.singletonIf doCommit
              (lib.concatStringsSep "" [
                "git commit \\\n  ${
                          lib.concatMapStringsSep " \\\n  "
                          (message: ''-m "${messagePrefix}${message}"'') 
                          (lib.flatten [(lib.singletonIf doMessageFirst messageFirst) messages])
                        }"
                "${if doIgnoreCommitFailed then ''${" \\\n  "}|| echo "commit failed!"'' else ""}"
              ])
            )
            (lib.singletonIf doPush "git push${if doIgnorePushFailed then " || echo \"push failed!\"" else ""}")
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
        , doInstallPriority ? (lib.length scripts) > 1
        , # starting install priority
          installPriority ? 0
        , doRun ? false
        , doCommit ? false
        , commitArgs ? { }
        }:
        lib.concatStringsSep "\n\n" (lib.flatten [
          (lib.singletonIf doGitPull gitPull)
          (
            lib.imap0
              (
                i: name:
                let
                  installable = if remote then name else "${if inDir then "." else dir}#${name}";
                  lines = lib.flatten [
                    (lib.singletonIf inDir "ROOT_DIR=$PWD\ncd ${dir}")
                    (lib.singletonIf doBuild "nix build ${installable}")
                    (lib.singletonIf doInstall "nix profile install ${installable}${if doInstallPriority then " --priority ${lib.toString (i + installPriority)}" else ""}")
                    (lib.singletonIf doRun "nix run ${installable}")
                    (lib.singletonIf inDir "cd $ROOT_DIR")
                  ];
                in
                lib.concatStringsSep "\n" lines
              )
              scripts
          )
          (lib.singletonIf doCommit (commit commitArgs))
        ])
      ;
      nix = args: nix_ ({ doInstall = true; doRun = true; } // args);
      nixScript = args@{ name ? "script", ... }: nix ((lib.removeAttrs args [ "name" ]) // { scripts = [ args.name ]; });
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
    , debug ? false
    , gcEnabledLinux ? false
    , gcMaxStoreSizeLinux ? 0
    , gcEnabledMacos ? false
    , gcMaxStoreSizeMacos ? 0
    , purgeEnabled ? false
    , purgeByCreatedTime ? false
    , purgeByAccessedTime ? false
    , purgeMaxAge ? 0
    }:
    let
      hashfilesArgs = lib.concatMapStringsSep ", " (x: "'${x}'") files;
      hashfiles = expr "hashfiles(${hashfilesArgs})";
    in
    {
      name = "Restore and cache Nix store";
      uses = "nix-community/cache-nix-action@v2";
      "with" = {
        key = "nix-${keyOS}-${keyJob}-${hashfiles}";
        restore-keys = ''
          nix-${keyOS}-${keyJob}-
        '';
      }
      // (lib.optionalAttrs (path != "") { inherit path; })
      // (lib.optionalAttrs gcEnabledLinux {
        gc-enabled-linux = true;
        gc-max-store-size-linux = gcMaxStoreSizeLinux;
      })
      // (lib.optionalAttrs gcEnabledMacos {
        gc-enabled-macos = true;
        gc-max-store-size-macos = gcMaxStoreSizeMacos;
      })
      // (lib.optionalAttrs purgeEnabled {
        purge-enabled = purgeEnabled;
        purge-by-created-time = purgeByCreatedTime;
        purge-by-accessed-time = purgeByAccessedTime;
        purge-max-age = purgeMaxAge;
      })
      // (lib.optionalAttrs debug {
        inherit debug;
      });
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
      uses = "nixbuild/nix-quick-install-action@${lib.toString installNixActionVersion}";
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

    removeCacheProfiles = { dir ? lib.env.CACHE_DIRECTORY }: {
      name = "Remove old cache profiles";
      run = "rm -rf ${dir}";
    };
  };

  # if there are multiple jobs that need caches
  # it may be better to purge caches only in the last one
  job_ =
    let steps_ = steps; in
    { dir ? "."
    , steps ? ({ dir, stepsAttrs }: [ ])
    , defaultOS ? os.ubuntu-22
    , name ? "Nix CI"
    , permissions ? { }
    , strategy ? { matrix.os = oss; }
    , doMatrixOS ? lib.hasAttrByPath [ "matrix" "os" ] strategy
    , runsOn ? (if doMatrixOS then names.matrix.os else defaultOS)
    , doConfigGitAsGHActions ? doUpdateLocks || doFormat || doCommit
    , installNixArgs ? { }
    , doCacheNix ? false
    , cacheNixArgs ? { }
    , doRemoveCacheProfiles ? false
    , cacheDirectory ? lib.env.CACHE_DIRECTORY
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
      stepsList = lib.flatten [
        steps_.checkout
        (installNix installNixArgs)
        (lib.singletonIf doCacheNix (steps_.cacheNix ({ keyOS = runsOn_; } // cacheNixArgs)))
        (lib.singletonIf doRemoveCacheProfiles (steps_.removeCacheProfiles { dir = cacheDirectory; }))
        (
          (if doMatrixOS then stepsIf ("${runsOn} == '${defaultOS}'") else (x: x)) [
            (lib.singletonIf doConfigGitAsGHActions steps_.configGitAsGHActions)
            (lib.singletonIf doUpdateLocks (steps_.updateLocks ({ inherit dir doInstall; } // updateLocksArgs)))
            (lib.singletonIf doFormat (steps_.format ({ inherit dir doInstall; } // formatArgs)))
            (lib.singletonIf doCommit (steps_.commit ({
              messages = [
                (lib.singletonIf doUpdateLocks (steps_.updateLocks { }).name)
                (lib.singletonIf doFormat (steps_.format { }).name)
              ];
            } // commitArgs)))
          ]
        )
        (steps { inherit dir stepsAttrs; })
        (lib.singletonIf doSaveFlakes (steps_.saveFlakes ({ inherit dir doInstall; } // saveFlakesArgs)))
        (lib.singletonIf doPushToCachix (steps_.pushToCachix ({ inherit dir doInstall; } // pushToCachixArgs)))
      ];
      stepsAttrs = lib.mapGenAttrs (x: { ${x.name} = x; }) stepsList;
    in
    {
      inherit name;
      runs-on = runsOn_;
      permissions = { contents = "write"; actions = "write"; } // permissions;
      steps = stepsList;
    } // (if strategy != { } then { inherit strategy; } else { });

  job = args: job_ (
    {
      doCacheNix = true;
      doRemoveCacheProfiles = true;
      doInstall = true;
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
    writeWorkflowsDir = lib.writeYAML "workflow" "./tmp/nixCI.yaml" (nixCI { });
    writeWorkflows = writeWorkflow "nixCI" (nixCI {
      jobArgs = {
        dir = "nix-dev/";
        doPushToCachix = true;
        cacheNixArgs = {
          gcEnabledLinux = true;
          gcMaxStoreSizeLinux = 7000000000;
          gcEnabledMacos = true;
          gcMaxStoreSizeMacos = 7000000000;

          purgeEnabled = true;
          purgeByCreatedTime = true;
          purgeByAccessedTime = true;
          purgeMaxAge = 172800;

          debug = true;
        };
        steps = { dir, stepsAttrs }: [
          (steps.commit { messages = [ stepsAttrs."Update flake locks".name stepsAttrs."Format Nix files".name ]; })
        ];
      };
    });
  };

  devShells.default = pkgs.mkShell {
    buildInputs = lib.attrValues { inherit (packages) writeWorkflows writeWorkflowsDir; };
  };
in
{
  inherit packages devShells;

  lib = {
    inherit
      expr
      job
      job_
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
      ;
    inherit (lib)
      mkAccessors
      mkAccessors_
      writeYAML
      genAttrsId
      ;
  };
}
