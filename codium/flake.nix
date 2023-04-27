{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    vscode-extensions.url = "github:nix-community/nix-vscode-extensions/6bf8fe514bbd28ac1053d912380cb2fb1cee5489";
    vscode-extensions-extra.url = "github:nix-community/nix-vscode-extensions/81c7057b41cccf936b574731c15303fa8a1ea424";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , drv-tools
    , vscode-extensions
    , vscode-extensions-extra
    , ...
    }:
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        inherit (drv-tools.functions.${system})
          withMan writeJSON toList mergeValues
          mkBin indentStrings4 withDescription
          withAttrs;
        man = drv-tools.configs.${system}.man;

        # A set of VSCodium extensions
        extensions = import ./nix-files/extensions.nix { inherit system vscode-extensions vscode-extensions-extra pkgs; };

        # nixified and restructured settings.json
        settingsNix = import ./nix-files/settings.nix;

        # create a codium with a given set of extensions
        # bashInteractive is necessary for correct work
        mkCodium = { extensions ? { }, runtimeDependencies ? [ ] }:
          let
            inherit (pkgs) vscode-with-extensions vscodium;
            codium =
              (vscode-with-extensions.override {
                vscode = vscodium;
                vscodeExtensions = toList extensions;
              });
            deps = pkgs.lib.lists.flatten [
              pkgs.bashInteractive
              pkgs.rnix-lsp
              pkgs.nixpkgs-fmt
              pkgs.direnv
              runtimeDependencies
            ];
            exeName = "codium";
          in
          withMan
            (withAttrs
              (pkgs.runCommand exeName
                { buildInputs = [ pkgs.makeBinaryWrapper ]; }
                ''
                  mkdir $out
                  cp -rs --no-preserve=mode,ownership ${codium}/* $out/
                  rm -rf $out/bin
                  mkdir $out/bin
                  makeWrapper ${codium}/bin/${exeName} $out/bin/${exeName} --prefix PATH : ${pkgs.lib.makeBinPath deps}
                ''
              )
              {
                inherit (vscodium) version;
                pname = exeName;
                name = "${exeName}-${vscodium.version}";
                meta = codium.meta // { description = "`VSCodium` with extensions and binaries on `PATH`"; };
              })
            (x: ''
              ${man.DESCRIPTION}
              ${x.meta.description}
              Its default runtime dependencies include `bashInteractive`, `rnix-lsp`, `nixpkgs-fmt`, `direnv`.

              Verify executables are on `PATH`: 
                  
                  Open VSCodium
                  Open a terminal there and run `printf '$PATH'`

              `PATH` should contain the Nix store paths of binaries that you set as runtime dependencies

              If no, try each of the following actions in order until the `PATH` is correct. After each action, check `PATH` in VSCodium:
                    
                  1. Repair VSCodium derivation (https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-store-repair.html)
                  2. Run VSCodium in a new terminal
                  3. Restart OS and collect Nix garbage (https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-store-gc.html), then run VSCodium
                      nix store gc
            '')
        ;

        writeSettingsJSON = settings:
          withMan
            (withDescription (writeJSON "settings" "./.vscode/settings.json" (mergeValues settings))
              (_: "Write `.vscode/settings.json`"))
            (x: ''
              ${man.DESCRIPTION}
              ${x.meta.description}
            '');

        writeTasksJSON = tasks:
          withMan
            (withDescription
              (writeJSON "tasks" "./.vscode/tasks.json" tasks)
              (_: "Write `.vscode/tasks.json`")
            )
            (x: ''
              ${man.DESCRIPTION}
              ${x.meta.description}
            '');

        # stuff for testing

        # codium with all extensions enabled
        testCodium = mkCodium {
          inherit extensions;
          runtimeDependencies = [ pkgs.hello ];
        };

        # test write settings
        testWriteSettings = writeSettingsJSON (
          settingsNix // {
            other = {
              "window.restoreWindows" = "none";
            };
          }
        );
        tools = [ testCodium testWriteSettings ];
      in
      {
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
        devShells.default = pkgs.mkShell {
          buildInputs = tools;
          shellHook = ''
            codium --list-extensions
          '';
        };
      });

  nixConfig = {
    extra-substituters = [
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

