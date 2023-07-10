{
  inputs.flakes.url = "github:deemp/flakes";

  outputs =
    inputs:
    let
      inputs_ =
        let flakes = inputs.flakes.flakes; in
        {
          inherit (flakes.source-flake) flake-utils nixpkgs nix-vscode-extensions;
          inherit (flakes) drv-tools;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem
          (system:
          let
            pkgs = inputs.nixpkgs.legacyPackages.${system};

            inherit (inputs.drv-tools.lib.${system})
              withMan writeJSON toList mergeValues
              mkBin indentStrings4 withDescription
              withAttrs man;

            # A set of VSCodium extensions
            extensions = import ./nix-files/extensions.nix {
              inherit system pkgs;
              inherit (inputs) nix-vscode-extensions;
            };

            # common extensions
            extensionsCommon = { inherit (extensions) nix misc github markdown; };

            # settings.json translated to .nix
            settingsNix = import ./nix-files/settings.nix;

            # common settings
            settingsCommonNix = {
              inherit (settingsNix)
                editor errorlens nix-ide explorer terminal
                files git gitlens json-language-features
                markdown-all-in-one markdown-language-features
                todo-tree workbench
                ;
            };

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
                  # pkgs.nixd
                  pkgs.nixpkgs-fmt
                  # pkgs.direnv
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
            lib = {
              inherit
                extensions
                extensionsCommon
                mkCodium
                settingsCommonNix
                settingsNix
                writeSettingsJSON
                writeTasksJSON
                ;
            };
            devShells.default = pkgs.mkShell {
              buildInputs = tools;
              shellHook = ''
                codium --list-extensions
              '';
            };
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
