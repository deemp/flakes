{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils.follows = "flake-utils_/flake-utils";
    formatter.url = "github:deemp/flakes?dir=source-flake/formatter";
    my-codium.url = "github:deemp/flakes?dir=codium";
    vscode-extensions_.url = "github:deemp/flakes?dir=source-flake/vscode-extensions";
    vscode-extensions.follows = "vscode-extensions_/vscode-extensions";
    python-tools.url = "github:deemp/flakes?dir=language-tools/python";
    my-devshell.url = "github:deemp/flakes?dir=devshell";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , drv-tools
    , my-codium
    , formatter
    , python-tools
    , vscode-extensions
    , my-devshell
    , ...
    }: flake-utils.lib.eachDefaultSystem
      (system:
      let
        inherit (my-codium.configs.${system}) extensions;
        inherit (my-codium.functions.${system}) mkCodium writeSettingsJSON;
        inherit (my-codium.configs.${system}) settingsNix;
        inherit (drv-tools.functions.${system}) mkShellApp;
        inherit (python-tools.snippets.${system}) activateVenv;
        inherit (vscode-extensions.packages.${system}) vscode open-vsx;
        createVenvs = python-tools.functions.${system}.createVenvs [ "." ];
        pkgs = nixpkgs.legacyPackages.${system};

        codiumTools = [
          pkgs.docker
          pkgs.poetry
          pkgs.rustup
          pkgs.nodePackages.near-cli
          createVenvs
          writeSettings
        ];
        codium = mkCodium {
          extensions = {
            inherit (extensions)
              python markdown github nix misc typescript yaml;
            other = {
              inherit (vscode.mtxr) sqltools;
              inherit (vscode.nomicfoundation) hardhat-solidity;
            };
          };
          runtimeDependencies = codiumTools;
        };
        writeSettings = writeSettingsJSON settingsNix;
        devshell = my-devshell.devshell.${system};
        inherit (my-devshell.functions.${system}) mkCommands;
        tools = codiumTools ++ [ codium ];
      in
      {
        devShells.default = devshell.mkShell {
          bash.extra = activateVenv;
          packages = tools;
          commands = mkCommands "ide" tools;
        };
      });

  nixConfig = {
    extra-trusted-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://hydra.iohk.io"
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
