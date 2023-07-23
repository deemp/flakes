{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs@{ self, ... }: inputs.flakes.makeFlake {
    inputs = { inherit (inputs.flakes.all) nixpkgs codium drv-tools flakes-tools devshell; };
    perSystem = { inputs, system }:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        inherit (inputs.codium.lib.${system}) extensions extensionsCommon settingsNix settingsCommonNix writeSettingsJSON mkCodium;
        inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;

        python =
          pkgs.python310.withPackages (p: with p; [
            python
            mypy
            ipykernel
            jupyter
            black
          ]);

        tools = [ python ];

        packages = {
          codium = mkCodium { extensions = extensionsCommon // { inherit (extensions) python jupyter; }; };
          writeSettings = writeSettingsJSON (settingsCommonNix // {
            python = settingsNix.python // {
              "python.defaultInterpreterPath" = "${python}/bin/python";
            };
          });
        };

        devShells = {
          default = mkShell {
            packages = tools;
            bash.extra = "";
            commands =
              mkCommands "tools" tools
              ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; };
          };
        };
      in
      {
        inherit packages devShells;
      };
  };

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
  