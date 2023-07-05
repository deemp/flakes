{
  inputs = {
    flakes = {
      url = "github:deemp/flakes";
      flake = false;
    };
  };
  outputs =
    inputsTop:
    let
      inputs_ = {
        inherit (import inputsTop.flakes.outPath) flake-utils nixpkgs codium devshell flakes-tools workflows;
      };

      outputs = flake { } // {
        inherit flake;
        inputs = inputs_;
      };

      flake =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs_.flake-utils.outputs.lib.eachDefaultSystem
          (system:
          let
            pkgs = inputs_.nixpkgs.legacyPackages.${system};
            inherit (inputs_.codium.lib.${system}) mkCodium writeSettingsJSON extensions extensionsCommon settingsNix settingsCommonNix;
            inherit (inputs_.devshell.lib.${system}) mkCommands mkRunCommands mkRunCommandsDir mkShell;
            inherit (inputs_.flakes-tools.lib.${system}) mkFlakesTools;
            inherit (inputs_.codium.outputs.inputs.vscode-extensions.extensions.${system}) vscode-marketplace open-vsx;
            inherit (inputs_.workflows.lib.${system}) writeWorkflow nixCI;

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
              writeSettings = writeSettingsJSON (settingsCommonNix // {
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
