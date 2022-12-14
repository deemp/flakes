{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    formatter.url = "github:deemp/flakes?dir=source-flake/formatter";
    my-codium.url = "github:deemp/flakes?dir=codium";
    devshell.url = "github:deemp/flakes?dir=devshell";
    workflows.url = "github:deemp/flakes?dir=workflows";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , flakes-tools
    , drv-tools
    , my-codium
    , formatter
    , devshell
    , workflows
    , ...
    }: flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (my-codium.configs.${system}) extensions;
        inherit (my-codium.functions.${system}) mkCodium writeSettingsJSON;
        inherit (my-codium.configs.${system}) settingsNix;
        inherit (drv-tools.functions.${system}) readDirectories;
        inherit (flakes-tools.functions.${system}) mkFlakesTools;
        inherit (devshell.functions.${system}) mkCommands mkShell;
        inherit (workflows.functions.${system}) writeWorkflow;
        inherit (workflows.configs.${system}) nixCI;

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
              "manager"
              "manager/nix-dev"
              "lima"
              "lima/nix-dev"
              "terrafix"
              "workflows"
              "."
            ]
          ]
        ));
        writeSettings = writeSettingsJSON settingsNix;
        codiumTools = [ writeSettings ];
        codium = mkCodium {
          extensions = { inherit (extensions) nix misc github markdown yaml; };
          runtimeDependencies = codiumTools;
        };
        tools = [ codium writeSettings ];
      in
      {
        devShells.default = mkShell
          {
            packages = tools;
            commands = mkCommands "tools" tools;
          };

        packages = {
          pushToCachix = flakesTools.pushToCachix;
          updateLocks = flakesTools.updateLocks;
          format = flakesTools.format;
          writeWorkflows = writeWorkflow "ci" nixCI;
        };
      })
    // {
      inherit (formatter) formatter;
      templates = rec {
        codium-generic = {
          path = ./templates/codium/generic;
          description = "VSCodium with extensions and executables";
        };
        codium-haskell = {
          path = ./templates/codium/haskell;
          description = "${codium-generic.description} for Haskell";
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
