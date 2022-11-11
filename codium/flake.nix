{
  inputs = {
    nixpkgs_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils";
    vscode-extensions_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/vscode-extensions";
    vscode-extensions.follows = "vscode-extensions_/vscode-extensions";
    vscode-extensions-selected_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/vscode-extensions-selected";
    vscode-extensions-selected.follows = "vscode-extensions-selected_/vscode-extensions-selected";
    drv-tools.url = "github:br4ch1st0chr0n3/flakes?dir=drv-tools";
    flake-utils.follows = "flake-utils_/flake-utils";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , drv-tools
    , vscode-extensions
    , vscode-extensions-selected
    , ...
    }:
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        inherit (drv-tools.functions.${system})
          withLongDescription
          writeJSON
          toList
          mergeValues
          ;

        # A set of VSCodium extensions
        extensions = import ./extensions.nix {
          inherit
            system
            vscode-extensions
            vscode-extensions-selected;
        };

        # nixified and restructured settings.json
        settingsNix = import ./settings.nix;

        # create a codium with a given set of extensions
        # bashInteractive is necessary for correct work
        mkCodium = { extensions ? { }, runtimeDependencies ? [ ] }:
          let
            codium =
              let inherit (pkgs) vscode-with-extensions vscodium;
              in
              (vscode-with-extensions.override {
                vscode = vscodium;
                vscodeExtensions = toList extensions;
              });
            deps = pkgs.lib.lists.flatten [
              pkgs.bashInteractive
              pkgs.rnix-lsp
              pkgs.nixpkgs-fmt
              runtimeDependencies
            ];
          in
          withLongDescription
            (pkgs.symlinkJoin {
              name = "codium";
              paths = [ codium ];
              buildInputs = [ pkgs.makeBinaryWrapper ];
              postBuild = ''
                wrapProgram $out/bin/codium \
                  --prefix PATH : ${pkgs.lib.makeBinPath deps}
              '';
            })
            ''
              `VSCodium` with built-in `bashInteractive` and the given runtime dependencies. 
              They will be available on `PATH` inside the IDE
            ''
        ;

        # write .vscode/settings.json
        writeSettingsJSON = settings:
          withLongDescription (writeJSON "settings" "./.vscode/settings.json" (mergeValues settings)) "write `.vscode/settings.json`";

        # write .vscode/tasks.json
        writeTasksJSON = tasks:
          withLongDescription (writeJSON "tasks" "./.vscode/tasks.json" tasks) "write `.vscode/tasks.json`";

        # stuff for testing

        # codium with all extensions enabled
        codium = mkCodium { inherit extensions; };

        writeSettings = writeSettingsJSON settingsNix;

      in
      {
        inherit extensions;
        functions = {
          inherit
            mkCodium
            writeSettingsJSON
            writeTasksJSON
            ;
        };
        configs = {
          inherit extensions settingsNix;
        };
        # tests
        packages = {
          test = {
            inherit codium writeSettings;
          };
        };
        devShells.default = pkgs.mkShell {
          buildInputs = [ codium writeSettings ];
        };
      });

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://hydra.iohk.io"
      "https://br4ch1st0chr0n3.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "br4ch1st0chr0n3.cachix.org-1:o1FA93L5vL4LWi+jk2ECFk1L1rDlMoTH21R1FHtSKaU="
    ];
  };
}

