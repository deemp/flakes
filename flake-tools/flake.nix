{
  inputs = {
    nixpkgs_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs";
    flake-utils_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils";
    cachix_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/cachix";
    drv-tools.url = "github:br4ch1st0chr0n3/flakes?dir=drv-tools";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils.follows = "flake-utils_/flake-utils";
    cachix.follows = "cachix_/cachix";
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
        withLongDescription
        runFishScript
        mkShellApp
        mkBin
        framedBrackets
        concatStringsNewline
        mkDevShellsWithDefault
        runInEachDir
        ;
      pushXToCachix = inp@{ name, fishScriptPath, runtimeInputs ? [ ], text ? "" }:
        withLongDescription
          (runFishScript
            (
              inp // {
                name = "push-${name}-to-cachix";
                runtimeInputs = runtimeInputs ++ [ cachix.packages.${system}.cachix ];
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
              ${concatStringsNewline dirs}
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
              ${concatStringsNewline dirs}
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
      mkFlakesTools = dirs: {
        update = flakesUpdate dirs;
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
