{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    codium.url = "github:deemp/flakes?dir=codium";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    vscode-extensions_.url = "github:deemp/flakes?dir=source-flake/nix-vscode-extensions";
    vscode-extensions.follows = "vscode-extensions_/vscode-extensions";
    devshell.url = "github:deemp/flakes?dir=devshell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    workflows.url = "github:deemp/flakes?dir=workflows";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem
    (system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        inherit (inputs.codium.functions.${system}) mkCodium writeSettingsJSON;
        inherit (inputs.codium.configs.${system}) extensions settingsNix;
        inherit (inputs.vscode-extensions.extensions.${system}) vscode-marketplace open-vsx;
        inherit (inputs.devshell.functions.${system}) mkCommands mkRunCommands mkShell;
        inherit (inputs.workflows.functions.${system}) writeWorkflow;
        inherit (inputs.workflows.configs.${system}) nixCI;
        inherit (inputs.flakes-tools.functions.${system}) mkFlakesTools;

        tools = [ pkgs.hello ];

        packages = {
          # --- IDE ---

          # This part can be removed if you don't use `VSCodium`
          # We compose `VSCodium` with dev tools
          # This is to let `VSCodium` run on its own, outside of a devshell
          codium = mkCodium {
            extensions = {
              inherit (extensions) nix haskell misc github markdown;
              # We can include more extensions by providing them in an attrset here
              extra = {
                inherit (vscode-marketplace.golang) go;
              };
            };
            runtimeDependencies = tools;
          };

          # a script to write `.vscode/settings.json`
          writeSettings = writeSettingsJSON {
            inherit (settingsNix) todo-tree files editor gitlens
              git nix-ide workbench markdown-all-in-one markdown-language-features;
          };

          # --- Flakes ---

          # Scripts that can be used in CI
          inherit (mkFlakesTools [ "." ]) updateLocks pushToCachix;

          # --- GH Actions

          # A script to write GitHub Actions workflow file into `.github/ci.yaml`
          writeWorkflows = writeWorkflow "ci" nixCI;
        };

        devShells.default = mkShell {
          packages = tools;
          bash.extra = "hello";
          commands =
            mkCommands "tools" tools
            ++ mkRunCommands "ide" {
              "codium ." = packages.codium;
              inherit (packages) writeSettings;
            };
        };
      in
      {
        inherit packages devShells;
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
