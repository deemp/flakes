{
  inputs = { };
  outputs = inputs:
    let
      inputs_ = {
        inherit (import ./source-flake) nixpkgs flake-utils formatter;
        drv-tools = import ./drv-tools;
        flakes-tools = import ./flakes-tools;
        codium = import ./codium;
        devshell = import ./devshell;
        workflows = import ./workflows;
      };

      outputs = outputs_ { } // {
        outputs = outputs_;
        inputs = inputs_;
        flakes = {
          codium = import ./codium;
          devshell = import ./devshell;
          drv-tools = import ./drv-tools;
          env2json = import ./env2json;
          flakes-tools = import ./flakes-tools;
          json2md = import ./json2md;
          language-tools = {
            haskell = import ./language-tools/haskell;
            purescript = import ./language-tools/purescript;
            python = import ./language-tools/python;
          };
          source-flake = import ./source-flake;
          workflows = import ./workflows;
        };
      };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem
          (system:
          let
            pkgs = inputs.nixpkgs.legacyPackages.${system};
            inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON extensionsCommon settingsCommonNix;
            inherit (inputs.drv-tools.lib.${system}) subDirectories withAttrs;
            inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
            inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
            inherit (inputs.workflows.lib.${system}) writeWorkflow nixCI;

            # cache most frequently used flakes

            flakesTools = (mkFlakesTools (
              [
                "codium"
                "devshell"
                "drv-tools"
                "env2json"
                "flakes-tools"
                "json2md"
                (subDirectories ./. "language-tools")
                (subDirectories ./. "source-flake")
                (subDirectories ./. "templates/codium")
                "templates/haskell-minimal"
                "workflows"
                "."
              ]
            ));

            packages = {
              inherit (flakesTools) pushToCachix format updateLocks;
              writeSettings = writeSettingsJSON settingsCommonNix;
              codium = mkCodium ({ extensions = extensionsCommon; });
              writeWorkflows = writeWorkflow "ci" (withAttrs (nixCI { doCacheNix = false; }) { on.schedule = [{ cron = "0 0 * * 0"; }]; });
            };

            tools = [ pkgs.nixd ];
          in
          {
            devShells.default = mkShell {
              packages = tools;
              commands =
                mkCommands "tools" tools ++
                mkRunCommands "ide" {
                  inherit (packages) writeSettings;
                  "codium ." = packages.codium;
                } ++
                mkRunCommands "infra" {
                  inherit (packages) writeWorkflows;
                }
              ;
            };

            inherit packages;
          })
        // {
          inherit (inputs) formatter;
          templates = rec {
            codium-generic = {
              path = ./templates/codium/generic;
              description = "`VSCodium` with extensions and executables";
            };
            codium-haskell = {
              path = ./templates/codium/haskell;
              description = "${codium-generic.description} for `Haskell`. Shows 5 ways to run a `Haskell` app.";
            };
            codium-haskell-simple = {
              path = ./templates/codium/haskell-simple;
              description = "${codium-generic.description} for `Haskell`.";
            };
            haskell-minimal = {
              path = ./templates/haskell-minimal;
              description = "Minimal flake for a `Haskell` package development.";
            };
            codium-python = {
              path = ./templates/codium/python;
              description = "${codium-generic.description} for `Python`.";
            };
          };
        };
    in
    outputs;

  nixConfig = {
    extra-trusted-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
