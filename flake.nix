{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    formatter.url = "github:deemp/flakes?dir=source-flake/formatter";
    codium.url = "github:deemp/flakes?dir=codium";
    devshell.url = "github:deemp/flakes?dir=devshell";
    workflows.url = "github:deemp/flakes?dir=workflows";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem
    (system:
      let
        inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON extensionsCommon settingsCommonNix;
        inherit (inputs.drv-tools.lib.${system}) readDirectories withAttrs;
        inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
        inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
        inherit (inputs.workflows.lib.${system}) writeWorkflow nixCI;

        flakesTools = (mkFlakesTools (
          let f = dir: (builtins.map (x: "${dir}/${x}") (readDirectories ./${dir})); in
          [
            (f "source-flake")
            (f "language-tools")
            (f "templates/codium")
            [
              "drv-tools"
              "flakes-tools"
              "env2json"
              "codium"
              "json2md"
              "devshell"
              "workflows"
              "templates/haskell-minimal"
              "."
            ]
          ]
        ));

        packages = {
          inherit (flakesTools) pushToCachix updateLocks format;
          writeSettings = writeSettingsJSON settingsCommonNix;
          codium = mkCodium ({ extensions = extensionsCommon; });
          writeWorkflows = writeWorkflow "CI" (withAttrs nixCI { on.schedule = [{ cron = "0 0 1 * *"; }]; });
        };
      in
      {
        devShells.default = mkShell {
          commands = mkRunCommands "ide" {
            inherit (packages) writeSettings;
            "codium ." = packages.codium;
          };
        };

        inherit packages;
      })
  // {
    inherit (inputs.formatter) formatter;
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
