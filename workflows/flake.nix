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
      };
      oss = attrValues os;

      # insert if: expression into steps
      # can omit ${{ }} - https://docs.github.com/en/actions/learn-github-actions/expressions#example-expression-in-an-if-conditional
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

      run = {
        runExecutableAndCommit = executable: commitMessage:
          ''
            git pull --rebase --autostash
            nix run .#${executable}
            git commit -a -m "action: ${commitMessage}" && git push || echo ""
          '';
      };

      steps = {
        checkout = {
          name = "Checkout this repo";
          uses = "actions/checkout@v3";
        };
        installNix = {
          name = "Install Nix";
          uses = "cachix/install-nix-action@v18";
          "with" = {
            extra_nix_config = ''
              access-tokens = github.com=${expr names.secrets.GITHUB_TOKEN}
              substituters = https://cache.nixos.org/ https://cache.iog.io https://nix-community.cachix.org https://deemp.cachix.org 
              trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8= deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18=
            '';
            install_url = "https://releases.nixos.org/nix/nix-2.11.1/install";
          };
        };
        logInToCachix = {
          name = "Log in to Cachix";
          uses = "cachix/cachix-action@v12";
          "with" = {
            name = expr names.secrets.CACHIX_CACHE;
            authToken = expr names.secrets.CACHIX_AUTH_TOKEN;
          };
        };
        pushFlakesToCachix = {
          name = "Push flakes to Cachix";
          env = {
            CACHIX_CACHE = expr names.secrets.CACHIX_CACHE;
          };
          run = "nix run .#${names.pushToCachix}";
        };
        configGitAsGHActions = {
          name = "Config git for github-actions";
          run = ''
            git config user.name github-actions
            git config user.email github-actions@github.com
          '';
        };
        updateLocksAndCommit = let name = "Update flake locks"; in
          {
            inherit name;
            run = run.runExecutableAndCommit names.updateLocks name;
          };
      };

      nixCI_ = steps_: {
        jobs = {
          nixCI = {
            name = "Nix CI";
            strategy.matrix.os = oss;
            runs-on = expr names.matrix.os;
            steps =
              [
                steps.checkout
                steps.installNix
              ]
              ++
              (stepsIf ("${names.matrix.os} == '${os.ubuntu-20}'") [
                steps.configGitAsGHActions
                steps.updateLocksAndCommit
              ])
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

      nixCI = nixCI_ [ ];
    in
    {
      packages = {
        writeWorkflowsDir = writeYAML "workflow" "./tmp/nixCI.yaml" nixCI;
        writeWorkflows = writeWorkflow "nixCI" nixCI;
      };
      functions = {
        inherit
          writeYAML writeWorkflow expr genAttrsId
          stepsIf mkAccessors mkAccessors_ run nixCI_;
      };
      configs = {
        inherit oss os names on steps nixCI;
      };
    });
}
