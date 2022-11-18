{
  inputs = {
    nixpkgs_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    cachix_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/cachix";
    cachix.follows = "cachix_/cachix";
    drv-tools.url = "github:br4ch1st0chr0n3/flakes?dir=drv-tools";
  };
  outputs =
    { self
    , nixpkgs
    , drv-tools
    , flake-utils
    , cachix
    , ...
    }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (drv-tools.functions.${system})
        withMan
        runFishScript
        mkShellApp
        mkBin
        framedBrackets
        concatStringsNewline
        mkDevShellsWithDefault
        runInEachDir
        indentStrings4
        ;
      inherit (pkgs.lib.lists) flatten;
      man = drv-tools.configs.${system}.man // {
        ENV = "# EXPECTED ENV VARIABLES";
      };
      pushXToCachix = inp@{ name, fishScriptPath, runtimeInputs ? [ ], text ? "" }:
        withMan
          (runFishScript
            (
              inp // {
                name = "push-${name}-to-cachix";
                runtimeInputs = runtimeInputs ++ [ cachix.packages.${system}.cachix ];
              }
            )
          ) ''a helper function for pushing to cachix'';

      pushPackagesToCachix = withMan
        (pushXToCachix { name = "packages"; fishScriptPath = ./scripts/cache-packages.fish; })
        ''
          ${man.DESCRIPTION}
          Push full closures (build and runtime dependencies) of all flake's packages to **cachix**
          
          ${man.ENV}

              **PATHS_FOR_PACKAGES** - (optional) temporary file where to store the build output paths
        '';

      pushDevShellsToCachix =
        withMan
          (pushXToCachix { name = "devshells"; fishScriptPath = ./scripts/cache-devshells.fish; })
          ''
            ${man.DESCRIPTION}
            
            Push full closures (build and runtime dependencies) of all flake's devshells to **cachix**
            
            ${man.ENV}

            **CACHIX_CACHE**
            :   cachix cache name
            
            **PROFILES_FOR_DEVSHELLS**
            :  (optional) temporary dir where to store the dev profiles
          ''
      ;

      pushInputsToCachix =
        withMan
          (pushXToCachix { name = "flake-inputs"; fishScriptPath = ./scripts/cache-inputs.fish; })
          ''
            ${man.DESCRIPTION}
            Push all flake inputs to **cachix**
          
            ${man.ENV}
            **CACHIX_CACHE**
            :   cachix cache name
          ''
      ;

      pushAllToCachix =
        withMan (mkShellApp {
          name = "push-all-to-cachix";
          text = ''
            ${mkBin pushInputsToCachix}
            ${mkBin pushDevShellsToCachix}
            ${mkBin pushPackagesToCachix}
          '';
        })
        ''
            ${man.DESCRIPTION}
            Push inputs and outputs (packages and devShells) of a flake to **cachix**
          '';
        

      flakesUpdate = dirs:
        runInEachDir
          {
            inherit dirs;
            name = "flakes-update";
            command = "nix flake update";
            longDescription = ''Update `flake.lock`-s'';
          };


      # push to cachix all about flakes in specified directories relative to CWD
      flakesPushToCachix = dirs: runInEachDir {
        inherit dirs;
        name = "flakes-push-to-cachix";
        command = "${mkBin pushAllToCachix}";
        longDescription = ''
          ${man.DESCRIPTION}
          Push flakes' inputs and outputs to **cachix**
        '';
      };

      # update and push flakes to cachix in specified directories relative to CWD
      flakesUpdateAndPushToCachix = dirs:
        let
          flakesUpdate_ = flakesUpdate dirs;
          flakesPushToCachix_ = flakesPushToCachix dirs;
          dirs_ = flatten dirs;
        in
        mkShellApp {
          name = "flakes-update-and-push-to-cachix";
          text = ''
            ${mkBin flakesUpdate_}
            ${mkBin flakesPushToCachix_}
          '';
          longDescription = ''
            ${man.DESCRIPTION}

            Update and push flakes to **cachix** in specified directories relative to **CWD**.
            The directories are:
            ${indentStrings4 dirs_}
            
            ${man.ENV}

            **CACHIX_CACHE**
            :   cachix cache name
          '';
        };

      # dump a devshell by running a dummy command in it
      dumpDevShells = runFishScript { name = "dump-devshells"; fishScriptPath = ./scripts/dump-devshells.fish; };

      # dump devshells in given directories
      # can be combined with updating flake locks
      flakesDumpDevshells = dirs: runInEachDir {
        inherit dirs;
        name = "flakes-dump-devshells";
        command = ''
          ${mkBin dumpDevShells}
        '';
        longDescription = ''
          ${man.DESCRIPTION}
          Evaluate devshells to dump them
        '';
      };

      # watch nix files existing at the moment
      flakesWatchDumpDevshells = dirs:
        let dirs_ = flatten dirs; in
        mkShellApp {
          name = "flakes-watch-dump-devshells";
          text = ''
            printf "${framedBrackets "watcher set"}"
            inotifywait -qmr -e close_write ./ | \
            while read dir action file; do
              if [[ $file =~ .*nix$ ]]; then
                set +e
                printf "${framedBrackets "started dumping devshells"}"
                ${mkBin (flakesUpdate dirs)}
                ${mkBin (flakesDumpDevshells dirs)}
                printf "${framedBrackets "finished dumping devshells"}"
                set -e
              fi
            done
          '';
          runtimeInputs = [ pkgs.inotify-tools ];
          longDescription = ''
            ${man.DESCRIPTION}
            Start a watcher that will update locks and dump (evaluate) devshells 
            in the following directories relative to **CWD**:
            ${indentStrings4 dirs_}
          '';
        };
      # format all .nix files with the formatter specified in the flake in the CWD
      flakesFormat = mkShellApp {
        name = "flakes-format";
        text = ''
          nix fmt **/*.nix
        '';
        longDescription = ''
          ${man.DESCRIPTION}
          Format **.nix** files in **CWD** and its subdirectories 
          using the formatter set in the **CWD** flake
        '';
      };

      # just inherit necessary functions
      mkFlakesTools = dirs: {
        updateLocks = flakesUpdate dirs;
        pushToCachix = flakesPushToCachix dirs;
        updateAndPushToCachix = flakesUpdateAndPushToCachix dirs;
        dumpDevshells = flakesDumpDevshells dirs;
        watchDumpDevshells = flakesWatchDumpDevshells dirs;
        format = flakesFormat;
      };
    in
    {
      functions = {
        inherit
          dumpDevShells
          flakesDumpDevshells
          flakesFormat
          flakesPushToCachix
          flakesUpdate
          flakesUpdateAndPushToCachix
          flakesWatchDumpDevshells
          mkFlakesTools
          pushAllToCachix
          pushDevShellsToCachix
          pushInputsToCachix
          pushPackagesToCachix
          pushXToCachix
          ;
      };


      devShells.default = pkgs.mkShell {
        buildInputs = [ (builtins.attrValues (mkFlakesTools [ "." ])) ];
      };
    });
}
