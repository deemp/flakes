{
  inputs = {
    nixpkgs_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs";
    flake-utils_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils";
    flakes-tools.url = "github:br4ch1st0chr0n3/flakes?dir=flakes-tools";
    drv-tools.url = "github:br4ch1st0chr0n3/flakes?dir=drv-tools";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils.follows = "flake-utils_/flake-utils";
    formatter.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/formatter";
    my-codium.url = "github:br4ch1st0chr0n3/flakes?dir=codium";
    vscode-extensions_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/vscode-extensions";
    vscode-extensions.follows = "vscode-extensions_/vscode-extensions";
    python-tools.url = "github:br4ch1st0chr0n3/flakes?dir=language-tools/python";
    my-devshell.url = "github:br4ch1st0chr0n3/flakes?dir=devshell";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , flakes-tools
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
        inherit (flakes-tools.functions.${system}) mkFlakesTools;
        createVenvs = python-tools.functions.${system}.createVenvs [ "." ];
        pkgs = nixpkgs.legacyPackages.${system};

        codium = mkCodium {
          extensions = {
            inherit (extensions)
              python markdown github nix misc typescript yaml;
            other = { inherit (vscode.mtxr) sqltools; };
          };
          runtimeDependencies = [
            (
              builtins.attrValues
                {
                  inherit (pkgs)
                    docker poetry direnv rnix-lsp
                    nixpkgs-fmt fish mysql;
                }
            )
          ];
        };
        flakesTools = mkFlakesTools [ "." ];
        writeSettings = writeSettingsJSON settingsNix;
        devshell = my-devshell.devshell.${system};
      in
      {
        devShells.default = devshell.mkShell {
          bash.extra = activateVenv;
          packages = [
            pkgs.nodePackages.near-cli
            codium
            pkgs.poetry
            createVenvs
            writeSettings
          ];
          commands = [
            {
              name = codium.name;
              help = codium.meta.description;
              category = "ide";
            }
            {
              name = createVenvs.name;
              help = createVenvs.meta.description;
              category = "ide";
            }
            {
              name = writeSettings.name;
              help = writeSettings.meta.description;
              category = "ide";
            }
          ];
        };
        packages = {
          pushToCachix = flakesTools.pushToCachix;
          updateLocks = flakesTools.updateLocks;
        };
      });

  nixConfig = {
    extra-trusted-substituters = [
      https://haskell-language-server.cachix.org
      https://nix-community.cachix.org
      https://hydra.iohk.io
      https://br4ch1st0chr0n3.cachix.org
    ];
    extra-trusted-public-keys = [
      haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8=
      nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
      hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
      br4ch1st0chr0n3.cachix.org-1:o1FA93L5vL4LWi+jk2ECFk1L1rDlMoTH21R1FHtSKaU=
    ];
  };
}
