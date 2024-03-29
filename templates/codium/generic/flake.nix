{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs: inputs.flakes.makeFlake {
    inputs = { inherit (inputs.flakes.all) nixpkgs codium devshell flakes-tools workflows; };
    perSystem = { inputs, system }:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON extensions extensionsCommon settingsNix settingsCommonNix;
        inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkRunCommandsDir mkShell;
        inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
        inherit (inputs.codium.outputs.inputs.nix-vscode-extensions.extensions.${system}) vscode-marketplace open-vsx;
        inherit (inputs.workflows.lib.${system}) writeWorkflow nixCI;

        packages = {
          # --- IDE ---

          # We compose `VSCodium` with extensions
          codium = mkCodium {
            # We use common extensions
            extensions = extensionsCommon // {
              # Next, we include the extensions from the pre-defined attrset
              inherit (extensions) haskell;
              # Furthermore, we can include extensions from https://github.com/nix-community/nix-vscode-extensions.
              # It's pinned in the flake inputs
              extra = { inherit (vscode-marketplace.golang) go; };
            };
          };

          # a script to write `.vscode/settings.json`
          writeSettings = writeSettingsJSON (settingsCommonNix // {
            inherit (settingsNix) haskell;
          });

          # --- Flakes ---

          # Scripts that can be used in CI
          inherit (mkFlakesTools { dirs = [ "." ]; root = ./.; }) updateLocks pushToCachix saveFlakes format;

          # --- GH Actions

          # A script to write GitHub Actions workflow file into `.github/ci.yaml`
          writeWorkflows = writeWorkflow "ci" (nixCI { jobArgs.doPushToCachix = true; });
        };

        tools = [ pkgs.hello ];

        devShells.default = mkShell {
          packages = tools;
          bash.extra = "hello";
          commands =
            mkCommands "tools" tools
            ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; }
            ++ mkRunCommandsDir "." "infra" { inherit (packages) updateLocks pushToCachix saveFlakes writeWorkflows format; };
        };
      in
      {
        inherit packages devShells;
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
