{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    vscode-extensions_.url = "github:deemp/flakes?dir=source-flake/vscode-extensions";
    vscode-extensions.follows = "vscode-extensions_/vscode-extensions";
    vscode-extensions-selected_.url = "github:deemp/flakes?dir=source-flake/vscode-extensions-selected";
    vscode-extensions-selected.follows = "vscode-extensions-selected_/vscode-extensions-selected";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    my-devshell.url = "github:deemp/flakes?dir=devshell";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , drv-tools
    , vscode-extensions
    , vscode-extensions-selected
    , my-devshell
    , ...
    }:
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        inherit (drv-tools.functions.${system})
          withMan writeJSON toList mergeValues
          mkBin indentStrings4 withDescription;
        man = drv-tools.configs.${system}.man;
        devshell = my-devshell.devshell.${system};

        # A set of VSCodium extensions
        extensions = import ./nix-files/extensions.nix {
          inherit
            system
            vscode-extensions
            vscode-extensions-selected;
        };

        # nixified and restructured settings.json
        settingsNix = import ./nix-files/settings.nix;

        # create a codium with a given set of extensions
        # bashInteractive is necessary for correct work
        mkCodium = { extensions ? { }, runtimeDependencies ? [ ] }:
          let
            testCodium =
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
          withMan
            (
              withDescription
                (
                  pkgs.symlinkJoin {
                    name = "codium";
                    paths = [ testCodium ];
                    buildInputs = [ pkgs.makeBinaryWrapper ];
                    postBuild = ''
                      wrapProgram $out/bin/codium \
                        --prefix PATH : ${pkgs.lib.makeBinPath deps}
                    '';
                  }
                ) "`VSCodium` with extensions and executables on `PATH`."
            )
            (x: ''
              ${man.DESCRIPTION}
              ${x.meta.description}
              Its default runtime dependencies include `bashInteractive`, `rnix-lsp`, `nixpkgs-fmt`.

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
              "Write `.vscode/settings.json`")
            (x: ''
              ${man.DESCRIPTION}
              ${x.meta.description}
            '');

        writeTasksJSON = tasks:
          withMan
            (withDescription
              (writeJSON "tasks" "./.vscode/tasks.json" tasks)
              "Write `.vscode/tasks.json`"
            )
            (x: ''
              ${man.DESCRIPTION}
              ${x.meta.description}
            '');

        # stuff for testing

        # codium with all extensions enabled
        testCodium = mkCodium { inherit extensions; };

        # test write settings
        testWriteSettings = writeSettingsJSON settingsNix;
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
        devShells.default = devshell.mkShell {
          packages = [ testCodium testWriteSettings ];
          commands = [
            {
              name = testWriteSettings.name;
              category = "ide";
              help = testWriteSettings.meta.description;
            }
            {
              name = "codium";
              category = "ide";
              help = testCodium.meta.description;
            }
          ];
        };
      });

  nixConfig = {
    extra-substituters = [
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

