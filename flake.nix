{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = "github:deemp/flakes?dir=codium";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    my-devshell.url = "github:deemp/flakes?dir=devshell";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs =
    { self
    , nixpkgs
    , my-codium
    , flake-utils
    , my-devshell
    , flakes-tools
    , drv-tools
    , haskell-tools
    , ...
    }: flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (my-codium.functions.${system}) mkCodium writeSettingsJSON;
        inherit (my-codium.configs.${system}) extensions settingsNix;
        inherit (flakes-tools.functions.${system}) mkFlakesTools;
        inherit (drv-tools.functions.${system})
          mkShellApps framedNewlines mkBinName withAttrs;
        devshell = my-devshell.devshell.${system};
        inherit (my-devshell.functions.${system}) mkCommands;
        inherit (haskell-tools.functions.${system}) toolsGHC;
        hsShellTools = haskell-tools.toolSets.${system}.shellTools;
        inherit (toolsGHC "92") stack hls ghc;
        writeSettings = writeSettingsJSON {
          inherit (settingsNix) todo-tree files editor gitlens
            git nix-ide workbench markdown-all-in-one
            markdown-language-features yaml haskell
            ;
        };
        codiumTools = builtins.attrValues
          (
            {
              inherit (pkgs)
                rabbitmq-server hadolint kubernetes docker
                poetry minikube kubernetes-helm postgresql_15;
              inherit writeSettings stack ghc;
              inherit (hsShellTools) implicit-hie ghcid;
            } // scripts
          );
        codium = mkCodium {
          extensions = {
            inherit (extensions)
              nix misc markdown github docker
              python toml yaml kubernetes haskell;
          };
          runtimeDependencies = codiumTools ++ [ hls ];
        };
        flakesTools = mkFlakesTools [ "." ];
        tools = codiumTools ++ [ codium ];
        helmPluginsPath = "helm/plugins";
        scripts = mkShellApps
          {
            installHelmPlugins = {
              text = ''
                printf "${framedNewlines "installing helm plugins into $HELM_PLUGINS"}"
                ${setHelmEnv}
                helm plugin install https://github.com/databus23/helm-diff || echo "installed 'helm-diff'"
                helm plugin install https://github.com/jkroepke/helm-secrets || echo "installed 'helm-secrets'"

                helm repo add bitnami https://charts.bitnami.com/bitnami
              '';
              description = "Install Helm plugins";
              runtimeInputs = [ pkgs.kubernetes-helm ];
            };
          };
        setHelmEnv =
          let in
          ''
            export HELM_PLUGINS=$PWD/${helmPluginsPath}
            mkdir -p $HELM_PLUGINS
                
            export HELM_CONFIG_HOME=$PWD/helm/config
            mkdir -p $HELM_CONFIG_HOME

            export HELM_CONFIG_HOME=$PWD/helm/config
            mkdir -p $HELM_CONFIG_HOME
          '';
      in
      {
        packages = {
          inherit (flakesTools) updateLocks pushToCachix;
          default = codium;
        };
        devShells.default = devshell.mkShell
          {
            packages = tools;
            bash.extra = setHelmEnv;
            commands = (mkCommands "ide" tools);
          };
        # Nix-provided libraries for stack
        stack-shell = { ghcVersion }:

          pkgs.haskell.lib.buildStackProject {
            name = "stack-shell";

            ghc = pkgs.haskell.compiler.${ghcVersion};

            buildInputs = [
              pkgs.lzma
              pkgs.hello
            ];
          };
      });

  nixConfig = {
    extra-trusted-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
