{
  inputs.flakes.url = "github:deemp/flakes";

  outputs =
    inputs:
    let
      inputs_ =
        let flakes = inputs.flakes.flakes; in
        {
          inherit (flakes.source-flake) flake-utils nixpkgs;
          inherit (flakes) codium drv-tools flakes-tools devshell;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.codium.lib.${system}) extensions settingsNix writeSettingsJSON mkCodium;
          inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;

          python =
            pkgs.python310.withPackages (p: with p; [
              python
              mypy
              ipykernel
              jupyter
              black
            ]);

          tools = [
            python
          ];

          packages = {
            codium = mkCodium {
              extensions = {
                inherit (extensions) nix misc github markdown python jupyter;
              };
              runtimeDependencies = tools;
            };
            writeSettings = writeSettingsJSON {
              inherit (settingsNix) todo-tree files editor gitlens
                git nix-ide workbench markdown-all-in-one markdown-language-features;
              extra = settingsNix.python // {
                "python.defaultInterpreterPath" = "${python}/bin/python";
              };
            };
          };

          devShells = {
            default = mkShell {
              packages = tools;
              bash.extra = '''';
              commands =
                mkCommands "tools" tools ++
                mkRunCommands "ide" {
                  "codium ." = packages.codium;
                  inherit (packages) writeSettings;
                };
            };
          };
        in
        {
          inherit packages devShells;
        });
    in
    outputs;
  nixConfig = {
    extra-substituters = [
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
  