{
  inputs.flakes.url = "github:deemp/flakes";

  outputs =
    inputs:
    let
      inputs_ =
        let flakes = inputs.flakes.flakes; in
        {
          inherit (flakes.source-flake) flake-utils nixpkgs;
          inherit (flakes) drv-tools;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = inputs.nixpkgs.outputs.legacyPackages.${system};
          inherit (inputs.drv-tools.outputs.lib.${system})
            withMan mkShellApp wrapShellApp
            mkShellApps getExe framedBrackets
            runInEachDir genAttrsId
            ;
          inherit (pkgs.lib.lists) flatten;

          cachix = pkgs.cachix;

          env = genAttrsId [ "CACHIX_AUTH_TOKEN" "CACHIX_CACHE" "PROFILES_FOR_DEVSHELLS" "PROFILES_FOR_PACKAGES" ];

          man = inputs.drv-tools.lib.${system}.man // {
            ENV = "# EXPECTED ENV VARIABLES";
            CACHIX_CACHE = ''
              `${env.CACHIX_CACHE}`
              :   cachix cache name  
            '';
            CACHIX_AUTH_TOKEN = ''
              `${env.CACHIX_AUTH_TOKEN}`
              :   cachix authorization token
            '';
            PROFILES_FOR_DEVSHELLS = ''
              `${env.PROFILES_FOR_DEVSHELLS}`
              :   cachix authorization token
            '';
          };

          saveAll = { doPushToCachix ? true }:
            withMan
              (mkShellApp {
                name = "save-all";
                text =
                  (
                    if doPushToCachix
                    then
                      ''
                        if [ -z ''${${env.CACHIX_CACHE}+x} ];
                        then 
                          printf "${framedBrackets "The environment variable ${env.CACHIX_CACHE} is not set. Can't find a Cachix cache."}"
                          exit 1
                        else
                          printf "${framedBrackets "Using the Cachix cache from ${env.CACHIX_CACHE} environment variable."}"
                        fi
                      ''
                    else ""
                  ) + ''
                    source ${./scripts.sh}
                    save-all ${builtins.toString doPushToCachix}
                  '';
                description = "Push inputs and outputs (`packages` and `devShells`) of a flake to `cachix`";
                runtimeInputs =
                  (if doPushToCachix then [ cachix ] else [ ]) ++
                  [
                    pkgs.jq
                    pkgs.findutils
                    pkgs.nix
                  ];
              })
              (x:
                ''
                  ${man.DESCRIPTION}
                  ${x.meta.description}

                  ${man.ENV}
                  ${man.CACHIX_CACHE}
                ''
              );

          flakesUpdate = dirs:
            runInEachDir
              {
                inherit dirs;
                name = "flakes-update";
                command = "${pkgs.nix}/bin/nix flake update";
                description = ''Update `flake.lock`s'';
              };

          logInToCachix = withMan
            (mkShellApp {
              name = "logInToCachix";
              text = ''
                if [ -z ''${${env.CACHIX_AUTH_TOKEN}+x} ];
                then 
                  printf "${framedBrackets "Environment variable ${env.CACHIX_AUTH_TOKEN} is not set. Can't log in to Cachix"}"
                  exit 1
                else
                  printf "${framedBrackets "Logging in to Cachix using ${env.CACHIX_AUTH_TOKEN} environment variable"}"
                  ${getExe cachix} authtoken ${env.CACHIX_AUTH_TOKEN}
                fi
              '';
              description = "Log in to `Cachix`";
            })
            (x:
              ''
                ${man.DESCRIPTION}
                ${x.meta.description}

                ${man.ENV}
                ${man.CACHIX_AUTH_TOKEN}
              ''
            );

          flakesSaveAll = { dirs ? [ ], doPushToCachix ? false }:
            let
              description = "Save and conditionally push to `Cachix` inputs and outputs of flakes in specified directories relative to `CWD`.";
              saveInEachDir = runInEachDir {
                inherit dirs;
                name = "flakes-save";
                command = getExe (saveAll { inherit doPushToCachix; });
                inherit description;
                longDescription = ''
                  ${man.ENV}
                '' +
                (if doPushToCachix then ''
                  ${man.CACHIX_CACHE}
                  ${man.CACHIX_AUTH_TOKEN}
                '' else "");
              };
            in
            mkShellApp {
              name = saveInEachDir.pname;
              text = ''
                ${getExe logInToCachix}
                ${getExe saveInEachDir}
              '';
              inherit (saveInEachDir.meta) description longDescription;
            };

          # format all .nix files with the formatter specified in the flake in the CWD
          flakesFormat = dirs:
            withMan
              (mkShellApp {
                name = "flakes-format";
                text = ''${pkgs.nix}/bin/nix fmt ${builtins.concatStringsSep " " dirs}'';
                description = "Format `.nix` files in `CWD` and its subdirectories";
              })
              (x: ''
                ${man.DESCRIPTION}
                ${x.meta.description} using the formatter set in the `CWD` `flake.nix`
              ''
              );

          # all flake tools together
          mkFlakesTools =
            dirs_:
            let dirs = pkgs.lib.lists.flatten dirs_; in
            (__mapAttrs (name: app: wrapShellApp { inherit name app; })
              {
                updateLocks = flakesUpdate dirs;
                pushToCachix = flakesSaveAll { inherit dirs; doPushToCachix = true; };
                saveAll = flakesSaveAll { inherit dirs; };
                format = flakesFormat dirs;
                inherit logInToCachix;
              })
          ;

          testFlakesTools = mkFlakesTools [ "." ];
        in
        {
          lib = {
            inherit
              flakesUpdate
              flakesSaveAll
              mkFlakesTools
              flakesFormat
              logInToCachix
              ;
          };

          inherit testFlakesTools;

          devShells.default = pkgs.mkShell {
            buildInputs = [ (builtins.attrValues testFlakesTools) ];
          };
        });
    in
    outputs;
}
