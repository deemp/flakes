{
  inputs = {
    nixpkgs_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs;
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;
    drv-tools.url = github:br4ch1st0chr0n3/flakes?dir=drv-tools;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs =
    { self
    , nixpkgs
    , drv-tools
    , flake-utils
    , ...
    }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (drv-tools.functions.${system})
        withLongDescription
        runFishScript
        mkShellApp
        mkBin
        framedBrackets
        printStringsLn
        ;
      pushXToCachix = inp@{ name, fishScriptPath, runtimeInputs ? [ ], text ? "" }:
        withLongDescription
          (runFishScript
            (
              inp // {
                name = "push-${name}-to-cachix";
                runtimeInputs = runtimeInputs ++ [ pkgs.cachix ];
              }
            )
          ) ''a helper function for pushing to cachix'';

      pushPackagesToCachix = withLongDescription
        (pushXToCachix { name = "packages"; fishScriptPath = ./scripts/cache-packages.fish; })
        ''
          push full closures (build and runtime dependencies) of all flake's packages to `cachix`
          expected env variables:
          `[PATHS_FOR_PACKAGES]` - (optional) temporary file where to store the build output paths
        '';

      pushDevShellsToCachix =
        withLongDescription
          (pushXToCachix { name = "devshells"; fishScriptPath = ./scripts/cache-devshells.fish; })
          ''
            push full closures (build and runtime dependencies) of all flake's devshells to `cachix`
            expected env variables:
            `CACHIX_CACHE` - cachix cache name
            `[PROFILES_FOR_DEVSHELLS]` - (optional) temporary dir where to store the dev profiles
          ''
      ;

      pushInputsToCachix = withLongDescription
        (pushXToCachix { name = "flake-inputs"; fishScriptPath = ./scripts/cache-inputs.fish; })
        ''
          push all flake inputs to `cachix`
          expected env variables:
          `CACHIX_CACHE` - cachix cache name
        ''
      ;

      pushAllToCachix =
        (mkShellApp {
          name = "push-all-to-cachix";
          text = ''
            ${mkBin pushInputsToCachix}
            ${mkBin pushDevShellsToCachix}
            ${mkBin pushPackagesToCachix}
          '';
          longDescription = ''
            Push inputs and outputs (packages and devShells) of a flake to `cachix`
          '';
        });


      runInEachDir = args@{ dirs, command, name, preMessage ? "", postMessage ? "", runtimeInputs ? [ ], longDescription ? "" }:
        (mkShellApp {
          name = "${name}-in-each-dir";
          inherit runtimeInputs;
          text =
            let INITIAL_PWD = "INITIAL_PWD";
            in
            ''
              ${INITIAL_PWD}=$PWD
              printf "%s" '${preMessage}'

            '' +
            builtins.concatStringsSep "\n"
              (builtins.map
                (dir: ''
                  printf "${framedBrackets "${name} : %s/flake.nix"}" "${"$" + INITIAL_PWD}/${dir}"

                  cd ${"$" + INITIAL_PWD}/${dir}
            
                  ${command}

                '')
                dirs) +
            ''

              printf "%s" '${postMessage}'
            '';
          longDescription = ''
            ${longDescription}

            Run the command
            
              ```sh
              ${command}
              ```
            
            relative to `CWD` in directories:

              ```sh
              ${printStringsLn dirs}
              ```
          '';
        });

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
        longDescription = ''Push flakes' inputs and outputs to `cachix`'';
      };

      # update and push flakes to cachix in specified directories relative to CWD
      flakesUpdateAndPushToCachix = dirs:
        let
          flakesUpdate_ = flakesUpdate dirs;
          flakesPushToCachix_ = flakesPushToCachix dirs;
        in
        mkShellApp {
          name = "flakes-update-and-push-to-cachix";
          text = ''
            ${mkBin flakesUpdate_}
            ${mkBin flakesPushToCachix_}
          '';
          longDescription = ''
            Update and push flakes to cachix in specified directories relative to `CWD`.
            The directories are:
              
              ```
              ${printStringsLn dirs}
              ```

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
        longDescription = ''Evaluate devshells to dump them'';
      };

      # watch nix files existing at the moment
      flakesWatchDumpDevshells = dirs: mkShellApp {
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
          Start a watcher that will update locks and dump (evaluate) devshells in the following directories relative to `CWD`:

              ```sh
              ${printStringsLn dirs}
              ```
        '';
      };
      # format all .nix files with the formatter specified in the flake in the CWD
      flakesFormat = mkShellApp {
        name = "flakes-format";
        text = ''
          nix fmt **/*.nix
        '';
        longDescription = ''
          Format `.nix` files in `PWD` and its subdirectories using the formatter set in the `CWD` flake
        '';
      };

      # just inherit necessary functions
      mkFlakesUtils = dirs: {
        flakesUpdate = flakesUpdate dirs;
        flakesPushToCachix = flakesPushToCachix dirs;
        flakesUpdateAndPushToCachix = flakesUpdateAndPushToCachix dirs;
        flakesDumpDevshells = flakesDumpDevshells dirs;
        flakesWatchDumpDevshells = flakesWatchDumpDevshells dirs;
        flakesFormat = flakesFormat;
      };

      flakesToggleRelativePaths = toggleConfig: flakesUpdate_: mkShellApp (
        let
          flakeNix = "flake.nix";
          inherit (pkgs.lib.strings) concatMapStringsSep concatStrings concatStringsSep;
          inherit (pkgs.lib.attrsets) mapAttrsToList;
        in
        {
          name = "flakes-toggle-relative-paths";
          runtimeInputs = [ pkgs.gawk ];
          text =
            let
              INITIAL_PWD = "INITIAL_PWD";
              toggler = dir: name: ''
                cd ${"$" + INITIAL_PWD}/${dir}

                printf "\n[ toggle-relative-path in %s ]\n" "${"$" + INITIAL_PWD}/${dir}"
                
                cat ${flakeNix} | \
                  awk '
                  {
                    if ($1 == "#" && $2 == "${name}.url") {
                      $1=""; print "  ", $0; {next}
                    }
                  }
                  {
                    if ($1 == "${name}.url") {
                      print "    #", $1, $2, $3; {next}
                    }
                  } 
                  {print}' > ${flakeNix}.toggled &&
                  mv ${flakeNix}.toggled ${flakeNix}
              '';
            in
            ''
              ${INITIAL_PWD}=$PWD
              
            '' +
            (concatMapStringsSep "\n"
              (
                configEntry: concatStrings
                  (
                    mapAttrsToList
                      (
                        dir: entries: concatMapStringsSep "\n" (entry: toggler dir entry) entries
                      )
                      configEntry
                  )
              )
              toggleConfig
            )
            +
            ''
              cd ${"$" + INITIAL_PWD}

              ${mkBin flakesUpdate_}
            ''
          ;
          longDescription = ''
            Traverse the flakes in given directories relative to `CWD` and toggle comments at the set entries, 
            switching relative paths to the absolute ones. Next, update the `flake.lock`-s.
            
            The set entries are:
            
              ```sh
                ${
                concatMapStringsSep "\n"
                (
                  configEntry: concatStrings
                    (
                      mapAttrsToList
                        (
                          dir: entries: "at ${dir}/${flakeNix} : ${concatStringsSep ", " entries}"
                        )
                        configEntry
                    )
                )
                toggleConfig
              }
              ```
          '';
        }
      );

    in
    {
      functions = {
        inherit
          pushXToCachix
          pushPackagesToCachix
          pushDevShellsToCachix
          pushInputsToCachix
          pushAllToCachix
          runInEachDir
          flakesUpdate
          flakesPushToCachix
          flakesUpdateAndPushToCachix
          dumpDevShells
          flakesDumpDevshells
          flakesWatchDumpDevshells
          flakesFormat
          mkFlakesUtils
          flakesToggleRelativePaths;
      };
    });
}
