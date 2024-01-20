{ system ? builtins.currentSystem
, pkgs ? (import ../.).inputs.nixpkgs.legacyPackages.${system}
, drv-tools ? (import ../drv-tools { inherit system pkgs; })
}:
let
  lib = drv-tools.lib.mergeAttrsRecursive [ builtins pkgs.lib drv-tools.lib ];

  cachix = lib.withMeta (pkgs.cachix) (_: { mainProgram = "cachix"; });

  env = lib.genAttrsId [ "CACHIX_AUTH_TOKEN" "CACHIX_CACHE" "NIX_CACHE_PROFILE" "CACHE_DIRECTORY" ];

  man = lib.man // {
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
    lib.withMan
      (lib.mkShellApp {
        name = "save-all";
        text =
          (
            if doPushToCachix
            then
              ''
                if [ -z ''${${env.CACHIX_CACHE}+x} ];
                then 
                  printf "%s" "${lib.framedBrackets "The environment variable ${env.CACHIX_CACHE} is not set. Can't find a Cachix cache."}"
                  exit 1
                else
                  printf "%s" "${lib.framedBrackets "Using the Cachix cache from ${env.CACHIX_CACHE} environment variable."}"
                fi
              ''
            else ""
          )
          + ''
            if [ -z ''${${env.CACHE_DIRECTORY}+x} ]; then
              export ${env.CACHE_DIRECTORY}="${CACHE_DIRECTORY}"
              printf "%s" "${lib.framedBrackets "The environment variable ${env.CACHE_DIRECTORY} is not set. Using the \$${env.CACHE_DIRECTORY} directory for profiles."}"
            else
              printf "%s" "${lib.framedBrackets "Using the \$${env.CACHE_DIRECTORY} directory for profiles."}"
            fi
            
            ${builtins.readFile ./scripts.sh}

            saveFlakes ${if doPushToCachix then "true" else "false"}
          '';
        excludeShellChecks = [ "SC1091" ];
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
    lib.runInEachDir
      {
        inherit dirs;
        name = "flakes-update";
        command = "${pkgs.nix}/bin/nix flake update";
        description = ''Update `flake.lock`s'';
      };

  logInToCachix = lib.withMan
    (lib.mkShellApp {
      name = "logInToCachix";
      text = ''
        if [ -z ''${${env.CACHIX_AUTH_TOKEN}+x} ];
        then 
          printf "${lib.framedBrackets "Environment variable ${env.CACHIX_AUTH_TOKEN} is not set. Can't log in to Cachix"}"
          exit 1
        else
          printf "${lib.framedBrackets "Logging in to Cachix using ${env.CACHIX_AUTH_TOKEN} environment variable"}"
          ${lib.getExe cachix} authtoken ${env.CACHIX_AUTH_TOKEN}
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

  flakesSave = { dirs ? [ ], doPushToCachix ? false, sources ? [ ], saveFlakesArgs ? { } }:
    let
      description = "Save and conditionally push to `Cachix` inputs and outputs of flakes in specified directories relative to `CWD`.";
      saveInEachDir = lib.runInEachDir {
        inherit dirs;
        name = "flakes-save";
        command = lib.getExe (saveFlakes ({ inherit doPushToCachix; } // saveFlakesArgs));
        inherit description;
        longDescription =
          lib.concatStringsNewline [
            man.ENV
            (lib.singletonIf doPushToCachix [
              man.CACHIX_CACHE
              man.CACHIX_AUTH_TOKEN
            ])
            man.NIX_CACHE_PROFILE
          ];
      };
      writeSources = pkgs.symlinkJoin { name = "appa"; paths = sources; };
    in
    lib.mkShellApp {
      name = "flakes-${if doPushToCachix then "push-to-cachix" else "save"}";
      text =
        lib.concatStringsNewline [
          (lib.singletonIf doPushToCachix (lib.getExe logInToCachix))
          (lib.getExe saveInEachDir)
        ];
      inherit (saveInEachDir.meta) description longDescription;
      # TODO can't symlink inputs - error No such file or directory
      # runtimeInputs = [ writeSources ];
    };

  # format all .nix files with the formatter specified in the flake in the CWD
  flakesFormat = dirs:
    lib.withMan
      (lib.mkShellApp {
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
        inherit (builtins) hasAttr;
        inherit (pkgs.lib.attrsets) hasAttrByPath mapAttrsToList;
        sources =
          getOutPath s ++
          (if hasAttrByPath [ "outputs" "inputs" ] s
          then mapAttrsToList (name: value: flakesGetSources value) s.outputs.inputs
          else if hasAttr "inputs" s then mapAttrsToList (name: value: getOutPath value) s.inputs
          else [ ]
          );
      in
      lib.unique (lib.flatten sources);

  # all flake tools together
  mkFlakesTools =
    { dirs ? [ ], subDirs ? [ ], root, flakesSaveArgs ? { } }:
    let
      args = {
        dirs = lib.flatten (dirs ++ map (lib.subDirectories root) subDirs);
        sources = flakesGetSources (import root);
      };
    in
    lib.mkShellApps {
      updateLocks = flakesUpdate args.dirs;
      pushToCachix = flakesSave (args // flakesSaveArgs // { doPushToCachix = true; });
      saveFlakes = flakesSave (args // flakesSaveArgs);
      format = flakesFormat args.dirs;
    }
  ;

  test = mkFlakesTools { dirs = [ "." ]; root = ./.; };
  
  devShells.default = pkgs.mkShell {
    buildInputs = lib.filter lib.isDerivation (lib.attrValues test);
  };
in
{
  lib = {
    inherit
      flakesFormat
      flakesGetSources
      flakesSave
      flakesUpdate
      logInToCachix
      mkFlakesTools
      saveFlakes
      saveFlakes_
      env
      ;
  };
  inherit test devShells;
}
