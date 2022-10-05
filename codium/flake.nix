{
  inputs = {
    nixpkgs_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs;
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;
    nix-vscode-marketplace_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nix-vscode-marketplace;
    vscodium-extensions_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/vscodium-extensions;
    drv-tools.url = github:br4ch1st0chr0n3/flakes?dir=drv-tools;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils.follows = "flake-utils_/flake-utils";
    nix-vscode-marketplace.follows = "nix-vscode-marketplace_/nix-vscode-marketplace";
    vscodium-extensions.follows = "vscodium-extensions_/vscodium-extensions";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , drv-tools
    , nix-vscode-marketplace
    , vscodium-extensions
    , ...
    }:
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        inherit (drv-tools.functions.${system})
          withLongDescription
          writeJson
          toList
          mergeValues
          mkDevShellsWithDefault
          ;

        # A set of VSCodium extensions
        extensions = import ./extensions.nix {
          inherit
            system
            nix-vscode-marketplace
            vscodium-extensions;
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
        writeSettingsJson = settings:
          withLongDescription (writeJson "settings" "./.vscode/settings.json" (mergeValues settings)) "write `.vscode/settings.json`";

        # write .vscode/tasks.json
        writeTasksJson = tasks:
          withLongDescription (writeJson "tasks" "./.vscode/tasks.json" tasks) "write `.vscode/tasks.json`";

        # stuff for testing

        # codium with all extensions enabled
        codium = [ (mkCodium { inherit extensions; }) ];

        writeSettings = writeSettingsJson settingsNix;

      in
      {
        inherit extensions;
        functions = {
          inherit
            mkCodium
            writeSettingsJson
            writeTasksJson
            ;
        };
        configs = {
          inherit extensions settingsNix;
        };
        devShells = mkDevShellsWithDefault
          {
            buildInputs = [ codium ];
          }
          {
            fish = { buildInputs = [ writeSettings ]; };
          };
      });

  nixConfig = {
    extra-substituters = [
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

