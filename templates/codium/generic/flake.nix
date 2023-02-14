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
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem
    (system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        inherit (inputs.codium.functions.${system}) mkCodium writeSettingsJSON;
        inherit (inputs.codium.configs.${system}) extensions settingsNix;
        inherit (inputs.vscode-extensions.extensions.${system}) vscode-marketplace open-vsx;
        inherit (inputs.devshell.functions.${system}) mkCommands mkShell;

        codiumTools = [ pkgs.hello ];

        # We construct `VSCodium` with ready attrsets of extensions like `nix`
        codium = mkCodium {
          extensions = {
            inherit (extensions) nix misc;
            # Also, we take some extensions from `extensions` and put them into the `haskell` attrset
            haskell = {
              inherit (open-vsx.haskell) haskell;
              inherit (vscode-marketplace.visortelle) haskell-spotlight;
            };
          };
          runtimeDependencies = codiumTools;
        };

        tools = codiumTools ++ [ codium ];
        # This is a script to write a `settings.json`
        # It uses attrsets of settings like `nix-ide`
        writeSettings = writeSettingsJSON {
          inherit (settingsNix)
            nix-ide git gitlens editor workbench
            files markdown-language-features todo-tree;
        };
      in
      {
        packages = {
          inherit writeSettings codium;
        };
        devShells.default = mkShell
          {
            packages = tools;
            bash = {
              extra = ''hello'';
            };
            commands = mkCommands "tools" tools;
          };
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
