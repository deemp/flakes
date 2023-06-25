{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    codium.url = "github:deemp/flakes?dir=codium";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    devshell.url = "github:deemp/flakes?dir=devshell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    workflows.url = "github:deemp/flakes?dir=workflows";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem
    (system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON extensions extensionsCommon settingsNix settingsCommon;
        inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkRunCommandsDir mkShell;
        inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
        inherit (inputs.codium.inputs.vscode-extensions.extensions.${system}) vscode-marketplace open-vsx;
        inherit (inputs.workflows.lib.${system}) writeWorkflow nixCI;

        tools = [ pkgs.hello ];

        packages = {
          # --- IDE ---

          # This part can be removed if you don't use `VSCodium`
          # We compose `VSCodium` with dev tools
          # This is to let `VSCodium` run on its own, outside of a devshell
          codium = mkCodium {
            # We use the common extensions
            extensions = extensionsCommon // {
              # Next, we include the extensions from the pre-defined attrset
              inherit (extensions) haskell;
              # Furthermore, we can include extensions from https://github.com/nix-community/nix-vscode-extensions.
              # It's pinned in the flake inputs
              extra = { inherit (vscode-marketplace.golang) go; };
            };
            runtimeDependencies = tools;
          };

          # a script to write `.vscode/settings.json`
          writeSettings = writeSettingsJSON (settingsCommon // {
            inherit (settingsNix) haskell;
          });

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
            ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; }
            ++ mkRunCommandsDir "." "infra" { inherit (packages) updateLocks pushToCachix writeWorkflows; };
        };
      in
      {
        inherit packages devShells;
      });

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
