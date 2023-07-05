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
            withMan runFishScript mkShellApp wrapShellApp
            mkShellApps mkBin framedBrackets
            concatStringsNewline mkDevShellsWithDefault runInEachDir
            indentStrings4 withDescription
            ;
          inherit (pkgs.lib.lists) flatten;

          cachix = pkgs.cachix;

          env = {
            CACHIX_AUTH_TOKEN = "CACHIX_AUTH_TOKEN";
            CACHIX_CACHE = "CACHIX_CACHE";
          };

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
          };
          pushXToCachix = inp@{ name, fishScriptPath, runtimeInputs ? [ ], text ? "" }:
            withMan
              (runFishScript
                (
                  inp // {
                    name = "push-${name}-to-cachix";
                    runtimeInputs =
                      runtimeInputs ++
                      [
                        cachix
                        pkgs.jq
                        pkgs.findutils
                        pkgs.nix
                      ];
                  }
                )
              )
              (_: ''A helper function for pushing to `cachix`'');

          pushPackagesToCachix =
            withMan
              (pushXToCachix { name = "packages"; fishScriptPath = ./scripts/cache-packages.fish; })
              (
                x:
                ''
                  ${man.DESCRIPTION}
                  ${x.meta.description}
          
                  ${man.ENV}

                  `PATHS_FOR_PACKAGES`
                  :   (optional) temporary file where to store the build output paths
                ''
              );

          pushDevShellsToCachix =
            withMan
              (withDescription
                (pushXToCachix { name = "devshells"; fishScriptPath = ./scripts/cache-devshells.fish; })
                (_: "Push full closures (build and runtime dependencies) of all flake's devshells to `cachix`")
              )
              (x:
                ''
                  ${man.DESCRIPTION}
                  ${x.meta.description}
            
                  ${man.ENV}

              
                  `PROFILES_FOR_DEVSHELLS`
                  :  (optional) temporary dir where to store the dev profiles
                ''
              )
          ;

          pushInputsToCachix =
            withMan
              (withDescription (pushXToCachix { name = "flake-inputs"; fishScriptPath = ./scripts/cache-inputs.fish; })
                (_: "Push all flake inputs to `cachix`")
              )
              (x:
                ''
                  ${man.DESCRIPTION}
                  ${x.meta.description}
          
                  ${man.ENV}
                  ${man.CACHIX_CACHE}
                ''
              )
          ;
          pushAllToCachix =
            withMan
              (mkShellApp {
                name = "push-all-to-cachix";
                text = ''
                  if [ -z ''${${env.CACHIX_CACHE}+x} ];
                  then 
                    printf "${framedBrackets "The environment variable ${env.CACHIX_CACHE} is not set. Can't find a Cachix cache"}"
                    exit 1
                  else
                    printf "${framedBrackets "Using the Cachix cache from ${env.CACHIX_CACHE} environment variable"}"
                  fi
                  ${mkBin pushInputsToCachix}
                  ${mkBin pushDevShellsToCachix}
                  ${mkBin pushPackagesToCachix}
                '';
                description = "Push inputs and outputs (`packages` and `devShells`) of a flake to `cachix`";
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
              rec {
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
                  ${cachix}/bin/cachix authtoken ${env.CACHIX_AUTH_TOKEN}
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

          # push to cachix all about flakes in specified directories relative to CWD
          flakesPushToCachix = dirs:
            let description = "Push flakes inputs and outputs to `Cachix` in given directories";
            in
            runInEachDir {
              inherit dirs;
              name = "flakes-push-to-cachix";
              command = ''
                ${mkBin logInToCachix}
                ${mkBin pushAllToCachix}
              '';
              inherit description;
              longDescription = ''
                ${man.ENV}
                ${man.CACHIX_CACHE}
                ${man.CACHIX_AUTH_TOKEN}
              '';
            };

          # update and push flakes to cachix in specified directories relative to CWD
          flakesUpdateAndPushToCachix = dirs:
            let
              flakesUpdate_ = flakesUpdate dirs;
              flakesPushToCachix_ = flakesPushToCachix dirs;
              dirs_ = flatten dirs;
            in
            withMan
              (mkShellApp {
                name = "flakes-update-and-push-to-cachix";
                text = ''
                  ${mkBin flakesUpdate_}
                  ${mkBin flakesPushToCachix_}
                '';
                description = "Update and push flakes to `cachix` in specified directories relative to `CWD`.";
              })
              (x: ''
                ${man.DESCRIPTION}
                ${x.meta.description}
            
                ${man.ENV}
                ${man.CACHIX_CACHE}

                ${man.NOTES}
                The given directories relative to `CWD` are:
                ${indentStrings4 dirs_}
              ''
              );

          # dump a devshell by running a dummy command in it
          dumpDevShells = runFishScript {
            name = "dump-devshells";
            fishScriptPath = ./scripts/dump-devshells.fish;
            runtimeInputs = [ pkgs.jq pkgs.findutils ];
          };

          # dump devshells in given directories
          # can be combined with updating flake locks
          flakesDumpDevshells = dirs:
            let description = "Evaluate devshells to in given directories to dump them"; in
            runInEachDir {
              inherit dirs;
              name = "flakes-dump-devshells";
              command = ''
                ${mkBin dumpDevShells}
              '';
              inherit description;
              longDescription = ''
                ${man.DESCRIPTION}
                ${description}
              '';
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
                pushToCachix = flakesPushToCachix dirs;
                logInToCachix = logInToCachix;
                updateAndPushToCachix = flakesUpdateAndPushToCachix dirs;
                format = flakesFormat dirs;
              })
          ;
        in
        {
          lib = {
            inherit
              flakesDumpDevshells
              flakesPushToCachix
              flakesUpdate
              flakesUpdateAndPushToCachix
              mkFlakesTools
              pushXToCachix
              ;
          };

          packages = {
            inherit
              dumpDevShells flakesFormat pushAllToCachix
              pushInputsToCachix pushDevShellsToCachix
              pushPackagesToCachix logInToCachix;
          };

          devShells.default = pkgs.mkShell {
            buildInputs = [ (builtins.attrValues (mkFlakesTools [ "." ])) ];
          };
        });
    in
    outputs;
}
