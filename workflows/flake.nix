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
      inherit (drv-tools.functions.${system})
        writeYAML genAttrsId mkAccessors mkAccessors_;
      inherit (builtins)
        mapAttrs toString attrValues attrNames map
        isAttrs;

      writeWorkflow = name: writeYAML name ".github/workflows/${name}.yaml";

      expr = expr_: "\${{ ${toString expr_} }}";

      os = {
        ubuntu-20 = "ubuntu-20.04";
        ubuntu-22 = "ubuntu-22.04";
        macos-11 = "macos-11";
        macos-12 = "macos-12";
        ubuntu-latest = "ubuntu-latest";
      };
      oss = attrValues os;

      # insert if: expression into steps
      # can omit ${{ }} - https://docs.github.com/en/actions/learn-github-actions/expressions#example-expression-in-an-if-conditional

      # include steps conditionally
      stepsIf = expr: steps: map (x: x // { "if" = expr; }) steps;

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

      run = rec {
        gitPull = ''
          git pull --rebase --autostash
        '';
        nixRunDir = dir: installable: ''
          cd '${dir}'
          nix run .#${installable}
        '';
        nixRunAndCommitDir = dir: installable: commitMessage: ''
          ${gitPull}
          ${nixRunDir dir installable}
          git commit -a -m "action: ${commitMessage}" && git push || echo ""
        '';
        nixRunAndCommit = nixRunAndCommitDir ".";
        nixRun = nixRunDir ".";
        commit = commitMessage: ''
          ${gitPull}
          git commit -a -m "action: ${commitMessage}" && git push || echo ""
        '';
      };

      # Flake file paths -> step to cache based on flake files
      cacheNixFiles = { flakeFiles ? [ "flake.nix" "flake.lock" ], store ? "auto", keySuffix ? "", checkIsRunnerLinux ? false, restoreOnly ? true }:
        let
          hashfilesArgs = pkgs.lib.strings.concatMapStringsSep ", " (x: "'${x}'") flakeFiles;
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

      # Flake directories -> step to cache based on flake files
      cacheNixDirs =
        { flakeDirs ? [ "." ], store ? nixStoreLinux, keySuffix ? "", checkIsRunnerLinux ? false, restoreOnly ? true }:
        cacheNixFiles ({
          flakeFiles = (__concatMap (x: [ "${x}/flake.nix" "${x}/flake.lock" ]) flakeDirs);
          inherit keySuffix store checkIsRunnerLinux restoreOnly;
        });

      nixStoreLinux = "/home/runner/nix";
      nixStoreAuto = "auto";

      # Set the custom store to enable caching
      # https://nixos.org/manual/nix/unstable/command-ref/conf-file.html?highlight=conf#conf-store
      # Keep build outputs to garbage collect at the end only the trash
      # https://nixos.org/manual/nix/unstable/command-ref/conf-file.html#description
      installNix = { store ? nixStoreAuto }: {
        name = "Install Nix";
        uses = "cachix/install-nix-action@v20";
        "with" = {
          extra_nix_config = ''
            access-tokens = github.com=${expr names.secrets.GITHUB_TOKEN}
            substituters = https://cache.nixos.org/ https://cache.iog.io https://nix-community.cachix.org https://deemp.cachix.org 
            trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18=
            store = ${store}
            keep-outputs = true
          '';
          # https://releases.nixos.org/?prefix=nix
          install_url = "https://releases.nixos.org/nix/nix-2.16.1/install";
        };
      };

      steps = {
        checkout = {
          name = "Checkout this repo";
          uses = "actions/checkout@v3";
        };
        cacheNix = cacheNixDirs { };
        logInToCachix = {
          name = "Log in to Cachix";
          run = "nix run nixpkgs#cachix -- authtoken ${ expr names.secrets.CACHIX_AUTH_TOKEN }";
        };
        pushFlakesToCachix = {
          name = "Push flakes to Cachix";
          env.CACHIX_CACHE = expr names.secrets.CACHIX_CACHE;
          run = "nix run .#${names.pushToCachix}";
        };
        configGitAsGHActions = {
          name = "Config git for github-actions";
          run = ''
            git config user.name github-actions
            git config user.email github-actions@github.com
          '';
        };
        updateLocksAndCommit =
          let name = "Update flake locks"; in
          {
            inherit name;
            run = run.nixRunAndCommit names.updateLocks name;
          };
        nixStoreCollectGarbage = {
          name = "Collect garbage in /nix/store";
          run = "nix store gc";
        };
      };

      strategies = {
        # setting the store doesn't work for macOS
        # https://discourse.nixos.org/t/how-to-use-a-local-directory-as-a-nix-binary-cache/655
        nixCache.matrix.include = [
          { os = os.macos-11; store = nixStoreAuto; }
          { os = os.macos-12; store = nixStoreAuto; }
          { os = os.ubuntu-20; store = nixStoreLinux; }
          { os = os.ubuntu-22; store = nixStoreLinux; }
        ];
      };

      nixCI_ = steps_: {
        jobs = {
          nixCI = {
            name = "Nix CI";
            strategy = strategies.nixCache;
            runs-on = expr names.matrix.os;
            steps =
              [
                steps.checkout
                (installNix { store = expr names.matrix.store; })
                (cacheNixDirs { keySuffix = "cachix"; store = expr names.matrix.store; restoreOnly = false; })
              ]
              ++ steps_
              ++ [
                steps.logInToCachix
                steps.pushFlakesToCachix
              ]
            ;
          };
        };
        name = "Nix CI";
        inherit on;
      };

      nixCI = nixCI_ (stepsIf ("${names.matrix.os} == '${os.ubuntu-20}'") [
        steps.configGitAsGHActions
        steps.updateLocksAndCommit
      ]);
    in
    {
      packages = {
        writeWorkflowsDir = writeYAML "workflow" "./tmp/nixCI.yaml" nixCI;
        writeWorkflows = writeWorkflow "nixCI" nixCI;
      };
      functions = {
        inherit
          writeYAML writeWorkflow expr genAttrsId cacheNixFiles cacheNixDirs
          stepsIf mkAccessors mkAccessors_ run nixCI_ installNix;
      };
      configs = {
        inherit oss os names on steps nixCI strategies nixStoreLinux nixStoreAuto;
      };
    });
}
