{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs: inputs.flakes.makeFlake {
    inputs = { inherit (inputs.flakes.all) nixpkgs drv-tools; };
    perSystem = { inputs, system }:
      let
        pkgs = inputs.nixpkgs.outputs.legacyPackages.${system};
        inherit (inputs.drv-tools.outputs.lib.${system})
          withMan mkShellApp mkShellApps
          getExe framedBrackets singletonIf
          runInEachDir genAttrsId subDirectories
          concatStringsNewline
          ;
        inherit (pkgs.lib.lists) flatten unique concatMap;
        inherit (pkgs.lib.attrsets) mapAttrsToList hasAttrByPath attrByPath;
        inherit (pkgs.lib.strings) concatMapStringsSep;
        inherit (builtins) hasAttr;

        cachix = pkgs.cachix;

        env = genAttrsId [ "CACHIX_AUTH_TOKEN" "CACHIX_CACHE" "NIX_CACHE_PROFILE" "CACHE_DIRECTORY" ];

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
          NIX_CACHE_PROFILE = ''
            `${env.NIX_CACHE_PROFILE}`
            :   Profile to install tools to
          '';
        };

        CACHE_DIRECTORY = "/nix/var/nix/profiles/cache";

        saveFlakes_ = { doPushToCachix ? false, doInstall ? false }:
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
                )
                + ''
                  if [ -z ''${${env.CACHE_DIRECTORY}+x} ]; then
                    export ${env.CACHE_DIRECTORY}="${CACHE_DIRECTORY}"
                    printf "${framedBrackets "The environment variable ${env.CACHE_DIRECTORY} is not set. Using the \$${env.CACHE_DIRECTORY} directory for profiles."}"
                  else
                    printf "${framedBrackets "Using the \$${env.CACHE_DIRECTORY} directory for profiles."}"
                  fi
                  source ${./scripts.sh}
                  saveFlakes ${if doPushToCachix then "true" else "false"}
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
                ${man.NIX_CACHE_PROFILE}
              ''
            );

        saveFlakes = args: saveFlakes_ ({ doInstall = true; } // args);

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

        # should be flake dirs
        flakesSave = { dirs ? [ ], doPushToCachix ? false, sources ? [ ], saveFlakesArgs ? { } }:
          let
            description = "Save and conditionally push to `Cachix` inputs and outputs of flakes in specified directories relative to `CWD`.";
            # attrs = map
            #   (x:
            #     let
            #       flake = import x;
            #       collect = attr: __attrValues (attrByPath [ attr system ] { } flake);
            #     in
            #     collect "packages" ++ collect "devShells"
            #   )
            #   dirs;
            save = pkgs.writeText "txt" (
              concatMapStringsSep "\n" (x: x) (__attrValues (import (__head dirs)).devShells.${system})
            )
              # pkgs.hello.overrideAttrs (x: {
              #   buildInputs = __attrValues (import (__head dirs)).devShells.${system};
              # })
              # concatMap
              #   (x:
              #     let
              #       flake = import x;
              #       collect = attr: __attrValues (attrByPath [ attr system ] { } flake);
              #     in
              #       # [flake]
              #     concatMap collect [ 
              #       "packages" 
              #       "devShells" 
              #     ]
              #   )
              #   dirs
            ;
            # pkgs.hello.overrideAttrs (x: {
            #   buildInputs = [
            #     pkgs.hello
            #   ];
            # });
            # pkgs.stdenv.mkDerivation {
            #   name = "save";
            #   src = ./.;
            #   buildInputs =
            #     concatMap
            #       (x:
            #         let
            #           flake = import x;
            #           collect = attr: __attrValues (attrByPath [ attr system ] { } flake);
            #         in
            #         concatMap collect [ "packages" "devShells" ]
            #       )
            #       dirs;
            # };
            # pkgs.writeText "test" (concatStringsNewline (
            #   concatMap
            #     (x:
            #       let
            #         flake = import x;
            #         collect = attr: __attrValues (attrByPath [ attr system ] { } flake);
            #       in
            #       concatMap collect [ "packages" "devShells" ]
            #     )
            #     dirs
            # ));

            # saveInEachDir = runInEachDir {
            #   inherit dirs;
            #   name = "flakes-save";
            #   command = getExe (saveFlakes ({ inherit doPushToCachix; } // saveFlakesArgs));
            #   inherit description;
            #   longDescription =
            #     concatStringsNewline [
            #       man.ENV
            #       (singletonIf doPushToCachix [
            #         man.CACHIX_CACHE
            #         man.CACHIX_AUTH_TOKEN
            #       ])
            #       man.NIX_CACHE_PROFILE
            #     ];
            # };
            # writeSources = pkgs.symlinkJoin { name = "inputs"; paths = sources; };
          in
          save;
        # mkShellApp {
        #   name = "flakes-${if doPushToCachix then "push-to-cachix" else "save"}";
        #   text =
        #     concatStringsNewline [
        #       (singletonIf doPushToCachix (getExe logInToCachix))
        #       # (getExe saveInEachDir)
        #     ];
        #   # inherit (saveInEachDir.meta) description longDescription;
        #   # TODO can't symlink inputs - error No such file or directory
        #   runtimeInputs = [ save ];
        # };

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

        flakesGetSources = s:
          if builtins.isString s
          then [ s ]
          else
            let
              getOutPath = s: if builtins.hasAttr "outPath" s then builtins.match "(/nix/store/[^/]+).*" "${s.outPath}" else [ ];
              sources =
                getOutPath s ++
                (if hasAttrByPath [ "outputs" "inputs" ] s
                then mapAttrsToList (name: value: flakesGetSources value) s.outputs.inputs
                else if hasAttr "inputs" s then mapAttrsToList (name: value: getOutPath value) s.inputs
                else [ ]
                );
            in
            unique (flatten sources);

        # all flake tools together
        mkFlakesTools =
          { dirs ? [ ], subDirs ? [ ], root, flakesSaveArgs ? { } }:
          let
            args = {
              dirs = map (x: "${root}/${x}") (flatten (dirs ++ builtins.map (subDirectories root) subDirs));
              sources = flakesGetSources (import root);
            };
          in
          mkShellApps {
            updateLocks = flakesUpdate args.dirs;
            pushToCachix = flakesSave (args // flakesSaveArgs // { doPushToCachix = true; });
            saveFlakes = flakesSave (args // flakesSaveArgs);
            format = flakesFormat args.dirs;
          }
        ;

        testFlakesTools = mkFlakesTools { dirs = [ "." ]; root = ./.; };
        devShells.default = pkgs.mkShell {
          buildInputs = builtins.filter (pkgs.lib.attrsets.isDerivation) (with testFlakesTools; [
            updateLocks pushToCachix format
          ]);
        };
      in
      {
        lib = {
          inherit
            CACHE_DIRECTORY
            flakesFormat
            flakesGetSources
            flakesSave
            flakesUpdate
            logInToCachix
            mkFlakesTools
            saveFlakes
            saveFlakes_
            ;
        };
        packages = testFlakesTools;
        inherit devShells;
      };
  };
}
