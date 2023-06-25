{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs =
    { self
    , nixpkgs
    , drv-tools
    , flake-utils
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (drv-tools.lib.${system})
        writeYAML genAttrsId mkAccessors mkAccessors_;

      writeWorkflow = name: writeYAML name ".github/workflows/${name}.yaml";

      expr = expr_: "\${{ ${__toString expr_} }}";

      os = {
        ubuntu-20 = "ubuntu-20.04";
        ubuntu-22 = "ubuntu-22.04";
        macos-11 = "macos-11";
        macos-12 = "macos-12";
        ubuntu-latest = "ubuntu-latest";
      };
      oss = __attrValues os;

      # insert if: expression into steps
      # can omit ${{ }} - https://docs.github.com/en/actions/learn-github-actions/expressions#example-expression-in-an-if-conditional

      # include steps conditionally
      stepsIf = expr: steps: __map (x: x // { "if" = expr; }) steps;

      # make stuff available as matrix.os instead of "matrix.os"
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
          gitPull = ''
            git pull --rebase --autostash
          '';
          nixRunDir = dir: installable: ''
            nix build ${dir}#${installable}
            nix run ${dir}#${installable}
          '';
          nixRun = nixRunDir ".";
          nixRunInDir = dir: installable: ''
            cd '${dir}'
            ${nixRun installable}
          '';
          pullActionCommit = action: commitMessage: ''
            ${gitPull}
            ${action}
            git commit -a -m "action: ${commitMessage}" && git push || echo ""
          '';
          nixRunAndCommitInDir = dir: installable: pullActionCommit (nixRunInDir dir installable);
          nixRunAndCommitDir = dir: installable: pullActionCommit (nixRunDir dir installable);
          nixRunAndCommitInPWD = nixRunAndCommitInDir ".";
          nixRunAndCommit = nixRunAndCommitDir ".";
          commit = commitMessage: ''
            ${gitPull}
            git commit -a -m "action: ${commitMessage}" && git push || echo ""
          '';
        in
        {
          inherit
            gitPull pullActionCommit
            nixRunDir nixRun nixRunInDir
            nixRunAndCommitInDir nixRunAndCommitDir
            nixRunAndCommitInPWD nixRunAndCommit;
        };

      # Flake file paths -> step to cache based on flake files
      cacheNixFiles = { flakeFiles ? [ "flake.nix" "flake.lock" ], store ? "auto", keySuffix ? "", checkIsRunnerLinux ? false, restoreOnly ? true }:
        let
          hashfilesArgs = pkgs.lib.strings.concat__mapStringsSep ", " (x: "'${x}'") flakeFiles;
          hashfiles = expr "hashfiles(${hashfilesArgs})";
        in
        (
          # setting the store doesn't work for macOS
          # https://discourse.nixos.org/t/how-to-use-a-local-directory-as-a-nix-binary-cache/655
          if checkIsRunnerLinux then { "if" = "${names.runner.os} == 'Linux'"; } else { }
        ) // (
          if restoreOnly then {
            name = "Restore Nix store";
            uses = "actions/cache/restore@v3";
            "with" = {
              path = store;
              key = ''
                ${expr names.runner.os}-nix-${hashfiles}-${keySuffix}
                ${expr names.runner.os}-nix-
              '';
            };
          } else {
            name = "Restore and cache Nix store";
            uses = "actions/cache@v3.3.0";
            "with" = {
              path = store;
              key = "${expr names.runner.os}-nix-${hashfiles}-${keySuffix}";
              restore-keys = ''
                ${expr names.runner.os}-nix-${hashfiles}-${keySuffix}
                ${expr names.runner.os}-nix-
              '';
            };
          }
        );

      # nix store directory used for caching
      # https://nixos.org/manual/nix/unstable/command-ref/conf-file.html?highlight=conf#conf-store
      nixStore = {
        linux = "/home/runner/nix";
        auto = "auto";
      };

      # Flake directories -> step to cache based on flake files
      cacheNixDirs =
        { flakeDirs ? [ "." ], store ? nixStore.linux, keySuffix ? "", checkIsRunnerLinux ? false, restoreOnly ? true }:
        cacheNixFiles ({
          flakeFiles = (__concat__map (x: [ "${x}/flake.nix" "${x}/flake.lock" ]) flakeDirs);
          inherit keySuffix store checkIsRunnerLinux restoreOnly;
        });

      # Keep build outputs to garbage collect at the end only the trash
      # https://nixos.org/manual/nix/unstable/command-ref/conf-file.html#description
      installNix = { store ? nixStore.auto }: {
        name = "Install Nix";
        uses = "cachix/install-nix-action@v20";
        "with" = {
          extra_nix_config = ''
            access-tokens = github.com=${expr names.secrets.GITHUB_TOKEN}
            substituters = https://cache.nixos.org/ https://cache.iog.io https://nix-community.cachix.org https://deemp.cachix.org 
            trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18=
            store = ${store}
            keep-outputs = true
            keep-derivations = true
          '';
          # https://releases.nixos.org/?prefix=nix
          install_url = "https://releases.nixos.org/nix/nix-2.16.1/install";
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
        cacheNix = cacheNixDirs { };
        logInToCachix = {
          name = "Log in to Cachix";
          run = ''
            nix build nixpkgs#cachix
            nix run nixpkgs#cachix -- authtoken ${ expr names.secrets.CACHIX_AUTH_TOKEN }
          '';
        };
        pushFlakesToCachixDir = dir: {
          name = "Push flakes to Cachix";
          env.CACHIX_CACHE = expr names.secrets.CACHIX_CACHE;
          run = run.nixRunDir dir names.pushToCachix;
        };
        pushFlakesToCachix = pushFlakesToCachixDir ".";
        configGitAsGHActions = {
          name = "Config git for github-actions";
          run = ''
            git config user.name github-actions
            git config user.email github-actions@github.com
          '';
        };
        updateLocksAndCommitDir = dir:
          let name = "Update flake locks"; in
          {
            inherit name;
            run = run.nixRunAndCommitDir dir names.updateLocks name;
          };
        updateLocksAndCommit = updateLocksAndCommitDir ".";
        nixStoreCollectGarbage = {
          name = "Collect garbage in /nix/store";
          run = "nix store gc";
        };
      };

      strategies = {
        # setting the store doesn't work for macOS
        # https://discourse.nixos.org/t/how-to-use-a-local-directory-as-a-nix-binary-cache/655
        nixCache.matrix.include = [
          { os = os.macos-11; store = nixStore.auto; }
          { os = os.macos-12; store = nixStore.auto; }
          { os = os.ubuntu-20; store = nixStore.linux; }
          { os = os.ubuntu-22; store = nixStore.linux; }
        ];
      };

      nixCI_ = { steps_ ? (_: [ ]), dir ? "." }: {
        jobs = {
          nixCI = {
            name = "Nix CI";
            strategy = strategies.nixCache;
            runs-on = expr names.matrix.os;
            steps =
              [
                steps.checkout
                (installNix { store = expr names.matrix.store; })
                # TODO cache suffix should depend on os?
                (cacheNixDirs { keySuffix = "cachix"; store = expr names.matrix.store; restoreOnly = false; })
              ]
              ++ (steps_ dir)
              ++ [
                steps.logInToCachix
                (steps.pushFlakesToCachixDir dir)
                steps.nixStoreCollectGarbage
              ]
            ;
          };
        };
        name = "Nix CI";
        inherit on;
      };

      nixCIDir = dir: nixCI_ {
        steps_ = _: (stepsIf ("${names.matrix.os} == '${os.ubuntu-20}'") ([
          steps.configGitAsGHActions
          (steps.updateLocksAndCommitDir dir)
        ]));
        inherit dir;
      };

      nixCI = nixCIDir ".";
    in
    {
      packages = {
        writeWorkflowsDir = writeYAML "workflow" "./tmp/nixCI.yaml" nixCI;
        writeWorkflows = writeWorkflow "nixCI" (nixCIDir "nix-dev/");
      };
      lib = {
        inherit
          cacheNixDirs
          cacheNixFiles
          expr
          genAttrsId
          installNix
          mkAccessors
          mkAccessors_
          names
          nixCI
          nixCI_
          nixCIDir
          nixStore
          on
          os
          oss
          run
          steps
          stepsIf
          strategies
          writeWorkflow
          writeYAML
          ;
      };
    });
}
