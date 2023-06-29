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
      inherit (drv-tools.lib.${system}) writeYAML genAttrsId mkAccessors mkAccessors_;
      inherit (builtins) concatMap;
      inherit (pkgs.lib.strings) concatMapStringsSep;

      writeWorkflow = name: writeYAML name ".github/workflows/${name}.yaml";

      expr = expr_: "\${{ ${builtins.toString expr_} }}";

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

      # TODO don't build scripts when `nix run` gets an option to make a gc root

      run =
        let
          gitPull = ''git pull --rebase --autostash'';
          commit =
            { commitMessage ? ""
            , commitMessages ? [ commitMessage ]
            }: ''git commit -a ${concatMapStringsSep " \\\n  " (message: ''-m "action: ${message}"'') commitMessages} && git push || echo ""'';
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

      nixCache = {
        cache = "/tmp/nix-cache";
        working-set = "/tmp/working-set";
        access-time = "197004020402";
        time = "1970-04-02 04:02";
        strategy.matrix.os = __attrValues { inherit (os) macos-11 macos-12 ubuntu-20 ubuntu-22; };
      };

      # cache nix store
      cacheNix =
        { files ? [ "**/flake.nix" "**/flake.lock" ]
        , keyJob ? "job"
        , keyOs ? expr names.runner.os
        , path ? nixCache.cache
        }:
        let
          hashfilesArgs = concatMapStringsSep ", " (x: "'${x}'") files;
          hashfiles = expr "hashfiles(${hashfilesArgs})";
        in
        {
          name = "Restore and cache Nix store";
          uses = "actions/cache@v3";
          "with" = {
            inherit path;
            key = "nix-${keyOs}-${keyJob}-${hashfiles}";
            restore-keys = ''
              nix-${keyOs}-${keyJob}-${hashfiles}
              nix-${keyOs}-${keyJob}-
            '';
          };
        }
      ;

      # nix store directory used for caching
      # https://nixos.org/manual/nix/unstable/command-ref/conf-file.html?highlight=conf#conf-store
      nixStore = {
        linux = "/home/runner/nix";
        auto = "auto";
      };

      # Keep build outputs to garbage collect at the end only the trash
      # https://nixos.org/manual/nix/unstable/command-ref/conf-file.html#description
      installNix = { store ? nixStore.auto }: {
        name = "Install Nix";
        uses = "cachix/install-nix-action@v22";
        "with" = {
          extra_nix_config = ''
            access-tokens = github.com=${expr names.secrets.GITHUB_TOKEN}
            substituters = https://cache.nixos.org/ https://nix-community.cachix.org https://cache.iog.io https://deemp.cachix.org 
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
        inherit cacheNix installNix;
        importNixCache = {
          "name" = "Import /nix/store cache";
          "run" = ''
            nix-store --import < ${nixCache.cache} || echo "no cache found :("
            find /nix/store -maxdepth 1 -name '*-*' | xargs -I {} sudo touch -at ${nixCache.access-time} {}
            # for compatibility with macOS
            nix profile install nixpkgs#coreutils-prefixed
          '';
        };
        exportNixCache = {
          name = "Export /nix/store cache";
          run = ''
            TIME=$(gdate --date="${nixCache.time}" +%s)
            gls /nix/store -l --time-style +%s --time=atime | \
              awk -v t="$TIME" '{ if ($6 > t) printf "/nix/store/%s\n", $7 }' > ${nixCache.working-set}
            nix-store --export $(cat ${ nixCache.working-set }) > ${ nixCache.cache}
          '';
        };
        logInToCachix = {
          name = "Log in to Cachix";
          run = ''
            nix run nixpkgs#cachix -- authtoken ${ expr names.secrets.CACHIX_AUTH_TOKEN }
          '';
        };
        pushFlakesToCachixDir = dir: {
          name = "Push flakes to Cachix";
          env = {
            CACHIX_CACHE = expr names.secrets.CACHIX_CACHE;
            CACHIX_AUTH_TOKEN = expr names.secrets.CACHIX_AUTH_TOKEN;
          };
          run = run.nixScript { inherit dir; name = names.pushToCachix; };
        };
        pushFlakesToCachix = pushFlakesToCachixDir ".";
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

      nixCI_ = { steps_ ? (_: [ ]), dir ? "." }: {
        name = "Nix CI";
        inherit on;
        jobs = {
          nixCI = {
            name = "Nix CI";
            strategy = nixCache.strategy;
            runs-on = expr names.matrix.os;
            steps =
              [
                steps.checkout
                (installNix { })
                (cacheNix { keyJob = "cachix"; keyOs = expr names.matrix.os; })
                steps.importNixCache
              ]
              ++ (steps_ dir)
              ++ [
                (steps.pushFlakesToCachixDir dir)
                steps.exportNixCache
              ]
            ;
          };
        };
      };

      nixCIDir = dir: nixCI_ {
        steps_ = _: stepsIf ("${names.matrix.os} == '${os.ubuntu-20}'") [
          steps.configGitAsGHActions
          (steps.updateLocks { inherit dir; })
        ];
        inherit dir;
      };

      nixCI = nixCIDir ".";

      packages = {
        writeWorkflowsDir = writeYAML "workflow" "./tmp/nixCI.yaml" nixCI;
        writeWorkflows = writeWorkflow "nixCI" (nixCIDir "nix-dev/");
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
          nixCIDir
          nixStore
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
}
