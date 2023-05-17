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
        inherit (inputs.codium.configs.${system}) extensions;
        inherit (inputs.codium.functions.${system}) mkCodium writeSettingsJSON;
        inherit (inputs.codium.configs.${system}) settingsNix;
        inherit (inputs.drv-tools.functions.${system}) readDirectories withAttrs;
        inherit (inputs.flakes-tools.functions.${system}) mkFlakesTools;
        inherit (inputs.devshell.functions.${system}) mkCommands mkRunCommands mkShell;
        inherit (inputs.workflows.functions.${system}) writeWorkflow;
        inherit (inputs.workflows.configs.${system}) nixCI;

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
              "terrafix"
              "workflows"
              "templates/haskell-minimal"
              "."
            ]
          ]
        ));

        packages = {
          inherit (flakesTools) pushToCachix updateLocks format;
          writeSettings = writeSettingsJSON settingsNix;
          codium = mkCodium { extensions = { inherit (extensions) nix misc github markdown yaml; }; };
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
