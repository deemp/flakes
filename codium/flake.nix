{
  inputs = {
    my-inputs.url = "path:../source";
    nixpkgs.follows = "my-inputs/nixpkgs";
    flake-utils.follows = "my-inputs/flake-utils";
    gitignore.follows = "my-inputs/gitignore";
    easy-purescript-nix.follows = "my-inputs/easy-purescript-nix";
    haskell-language-server.follows = "my-inputs/haskell-language-server";
    nix-vscode-marketplace.follows = "my-inputs/nix-vscode-marketplace";
    vscodium-extensions.follows = "my-inputs/vscodium-extensions";
  };
  outputs =
    { self
    , my-inputs
    , flake-utils
    , nixpkgs
    , nix-vscode-marketplace
    , easy-purescript-nix
    , vscodium-extensions
    , gitignore
    , haskell-language-server
    ,
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};

      # A set of VSCodium extensions
      extensions = import ./extensions.nix {
        inherit
          system
          nix-vscode-marketplace
          vscodium-extensions;
      };

      # if a set's attribute values are all sets, merge these values
      # Examples:
      # mergeValues {a = {b = 1;}; c = {d = 2;};} => {b = 1; d = 2;}
      mergeValues = set@{ ... }:
        builtins.foldl' pkgs.lib.mergeAttrs { } (builtins.attrValues set);

      # nixified and restructured settings.json
      settingsNix = import ./settings.nix;

      # a convenience function that flattens a set with set attribute values
      # toList {a = {b = 1;}; c = {d = 2;};} => [1 2]
      toList = x: builtins.attrValues (mergeValues x);

      # shell tools for development
      shellTools = {
        purescript =
          let easy-ps = import easy-purescript-nix { inherit pkgs; };
          in
          {
            inherit (pkgs) dhall-lsp-server;
            inherit (easy-ps)
              purs-0_15_4 spago purescript-language-server purs-tidy;
          };

        nix = {
          inherit (pkgs) rnix-lsp nixpkgs-fmt;
          inherit json2nix;
        };

        haskell = {
          inherit (pkgs.haskellPackages)
            # formatters
            ormolu
            floskell
            brittany
            stylish-haskell
            # Lookup Haskell documentation
            hoogle
            # auto generate LSP hie.yaml file from cabal
            implicit-hie
            # Automatically discover and run Hspec tests
            hspec-discover
            # Automation of Haskell package release process.
            releaser
            # Simple Hackage release workflow for package maintainers
            hkgr
            # Easy dependency management for Nix projects.
            niv
            # Convert package.yaml to *.cabal
            hpack
            # GHCi based bare bones IDE
            ghcid
            ;
          # Haskell-language-server
          inherit (pkgs) haskell-language-server;
        };

        markdown = { };
      };

      # Wrap Stack to work with our Nix integration.
      stack-wrapped = pkgs.symlinkJoin {
        # will be available as the usual `stack` in terminal
        name = "stack";
        paths = [ pkgs.stack ];
        buildInputs = [ pkgs.makeWrapper ];
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        postBuild = ''
          wrapProgram $out/bin/stack \
            --add-flags "\
              --system-ghc \
              --no-install-ghc \
            "
        '';
      };

      # create a codium with a given set of extensions
      # bashInteractive is necessary for correct work
      mkCodium = extensions@{ ... }:
        let inherit (pkgs) vscode-with-extensions vscodium;
        in
        [
          (vscode-with-extensions.override {
            vscode = vscodium;
            vscodeExtensions = toList extensions;
          })
          pkgs.bashInteractive
        ];

      # ignore shellcheck when writing a shell application
      mkShellApp = args@{ name, text, runtimeInputs ? [ ] }:
        pkgs.writeShellApplication (args // {
          runtimeInputs = pkgs.lib.lists.flatten (args.runtimeInputs or [ ]);
          checkPhase = "";
        });

      # String -> String -> Set -> IO ()
      # make a shell app called `name` which writes `data` (a Nix expression) as json into `path`
      writeJson = name: path: dataNix:
        let
          dataJson = builtins.toJSON dataNix;
          name_ = "write-${name}-json";
          dir = builtins.dirOf path;
          file = builtins.baseNameOf path;
        in
        mkShellApp {
          name = name_;
          runtimeInputs = [ pkgs.python310 ];
          text = ''
            mkdir -p ${dir}
            printf "%s" ${
              pkgs.lib.escapeShellArg dataJson
            } | python -m json.tool > ${path}
            printf "\n[ok %s]\n" "${name_}"
          '';
        };

      # write .vscode/settings.json
      writeSettingsJson = settings:
        writeJson "settings" "./.vscode/settings.json" (mergeValues settings);

      # write .vscode/tasks.json
      writeTasksJson = tasks: writeJson "tasks" "./.vscode/tasks.json" tasks;

      # convert json to nix
      # no need to provide the full path to a file if it's in the cwd
      # json2nix .vscode/settings.json my-settings.nix
      json2nix = mkShellApp {
        name = "json2nix";
        runtimeInputs = [ pkgs.nixpkgs-fmt ];
        text = ''
          json_path=$1
          nix_path=$2
          nix eval --impure --expr "with builtins; fromJSON (readFile ./$json_path)" > $nix_path
          sed -i -E "s/(\[|\{)/\1\n/g" $nix_path
          nixpkgs-fmt $nix_path
        '';
      };

      # codium with all extensions enabled
      codium = [ (mkCodium extensions) ];

      # a convenience function for building haskell packages
      # can be used for a project with GHC 9.0.2 as follows:
      # callCabal = callCabalGHC "902";
      # dep = callCabal "dependency-name" ./dependency-path { };
      # my-package = callCabal "my-package-name" ./my-package-path { inherit dependency; };
      callCabalGHC = ghcVersion: name: path: args:
        let
          inherit (pkgs.haskell.packages."ghc${ghcVersion}") callCabal2nix;
          inherit (gitignore.lib) gitignoreSource;
        in
        callCabal2nix name (gitignoreSource path) args;

      # actually build an executable
      # my-package-exe = justStaticExecutables
      inherit (pkgs.haskell.lib) justStaticExecutables;

      # build an executable without local dependencies (empty args)
      staticExecutable = ghcVersion: name: path:
        let
          inherit (pkgs.haskell.packages."ghc${ghcVersion}") callCabal2nix;
          inherit (gitignore.lib) gitignoreSource;
        in
        justStaticExecutables
          (callCabal2nix name (gitignoreSource path) { });

      # stack and ghc of a specific version
      # they should come together so that stack doesn't use the system ghc
      stack = ghcVersion: [
        stack-wrapped
        pkgs.haskell.compiler."ghc${ghcVersion}"
      ];

      # this version of HLS is only for aarch64-darwin, x86_64-darwin, x86_64-linux
      hls = ghcVersion:
        haskell-language-server.packages.${system}."haskell-language-server-${ghcVersion}";

      # tools for a specific GHC version
      toolsGHC = ghcVersion: {
        hls = hls ghcVersion;
        # see what you need to pass to your shell
        # https://docs.haskellstack.org/en/stable/nix_integration/#supporting-both-nix-and-non-nix-developers
        stack = stack ghcVersion;
        callCabal = callCabalGHC ghcVersion;
        staticExecutable = staticExecutable ghcVersion;
      };

      # has a runtime dependency on fish!
      fishHook = value@{ hook ? "", shellName, fish, }:
        let
          # Name of a variable that accumulates shell names
          MY_SHELL_NAME = "MY_SHELL_NAME";
        in
        ''
          ${hook}

          ${fish.pname} -C '
            set ${MY_SHELL_NAME} ${shellName}

            source ${./scripts/devshells.fish};
          ' -i;
        '';


      runFishScript = { name, fishScriptPath, runtimeInputs ? [ ], text ? "" }: mkShellApp {
        inherit name;
        runtimeInputs = runtimeInputs ++ builtins.attrValues { inherit (pkgs) fish jq cachix; };
        text =
          let CURRENT_SYSTEM = "CURRENT_SYSTEM"; in
          ''
            export CURRENT_SYSTEM=${system}
            
            ${text}

            fish ${fishScriptPath}
          '';
      };

      # a helper function for pushing to cachix
      pushXToCachix = inp@{ name, fishScriptPath, runtimeInputs ? [ ], text ? "" }:
        runFishScript (inp // { name = "push-${name}-to-cachix"; });

      # push full closures (build and runtime dependencies) of all flake's packages to cachix
      # expected env variables:
      # CACHIX_CACHE - cachix cache name
      # [PATHS_FOR_PACKAGES] - (optional) temporary file where to store the build output paths
      pushPackagesToCachix = pushXToCachix { name = "packages"; fishScriptPath = ./scripts/cache-packages.fish; };

      # push full closures (build and runtime dependencies) of all flake's devshells to cachix
      # expected env variables:
      # CACHIX_CACHE - cachix cache name
      # [PROFILES_FOR_DEVSHELLS] - (optional) temporary dir where to store the dev profiles
      pushDevShellsToCachix = pushXToCachix { name = "devshells"; fishScriptPath = ./scripts/cache-devshells.fish; };

      # push all flake inputs to cachi
      # expected env variables:
      # CACHIX_CACHE - cachix cache name
      # [PROFILES_FOR_DEVSHELLS] - (optional) temporary dir where to store the dev profiles
      pushInputsToCachix = pushXToCachix { name = "flake-inputs"; fishScriptPath = ./scripts/cache-inputs.fish; };

      # Push inputs and outputs (packages and devShells) of a flake to cachix
      pushAllToCachix = mkShellApp {
        name = "push-all-to-cachix";
        runtimeInputs = [ pushPackagesToCachix pushDevShellsToCachix pushInputsToCachix ];
        text = ''
          ${pushInputsToCachix.name} &&
          ${pushDevShellsToCachix.name} &&
          ${pushPackagesToCachix.name}
        '';
      };

      runInEachDir = args@{ root, dirs, command, name, runtimeInputs ? [ ] }: mkShellApp {
        name = "${name}-in-each-dir";
        inherit runtimeInputs;
        text = ''
          INITIAL_PWD=$(echo $(pwd))

        '' +
        builtins.concatStringsSep "\n"
          (builtins.map
            (dir: ''
              printf "[ %s ]" "$INITIAL_PWD/${dir}"

              cd $INITIAL_PWD/${dir}
            
              ${command}

            '')
            dirs);
      };

      # create devshells
      # notice the dependency on fish
      mkDevShells = shells@{ ... }:
        { fish }:
        builtins.mapAttrs
          (shellName: shellAttrs:
            pkgs.mkShell (shellAttrs // {
              buildInputs = (shellAttrs.buildInputs or [ ]) ++ [ fish ];
              # We need to exit the shell in which fish runs
              # Otherwise, after a user exits fish, she will return to a default shell
              shellHook = ''
                ${fishHook {
                  inherit shellName fish;
                  hook = shellAttrs.shellHook or "";
                }}
                exit
              '';
            }))
          shells;

      # make shells
      # The default shell will have the ${name} command available
      # This command will run a shell app constructed from ${runtimeInputs} and ${text} and start a fish shell
      mkDevShellsWithEntryPoint =
        defaultEntryAttrs@{ name, runtimeInputs ? [ ], text ? "", }:
        defaultShellAttrs@{ buildInputs ? [ ], shellHook ? "", ... }:
        shells@{ ... }:
        let
          inherit (pkgs) fish;
          shells_ = mkDevShells shells { inherit fish; };
          entryPoint = mkShellApp {
            runtimeInputs = buildInputs ++ runtimeInputs ++ [ fish ];
            inherit name;
            text = fishHook {
              shellName = name;
              hook = text;
              inherit fish;
            };
          };
          default = pkgs.mkShell (defaultShellAttrs // {
            name = "default";
            buildInputs = buildInputs ++ [ entryPoint ];
            shellHook = ''
              ${shellHook}
            '';
          });
          devShells_ = shells_ // { inherit default; };
        in
        devShells_;

      # update flakes in specified directories
      updateFlakes = rootPath: dirs: runInEachDir {
        inherit dirs; name = "update-flakes";
        command = "nix flake update";
        root = rootPath;
      };

      # push to cachix all about flakes in specified directories
      pushToCachix = rootPath: dirs: runInEachDir {
        inherit dirs; name = "push-to-cachix";
        command = "${pushAllToCachix.name}";
        runtimeInputs = [ pushAllToCachix ];
        root = rootPath;
      };

      # combined update and push to cachix
      fullUpdate = rootPath: dirs:
        let
          updateFlakes_ = updateFlakes rootPath dirs;
          pushToCachix_ = pushToCachix rootPath dirs;
        in
        mkShellApp {
          name = "full-update";
          runtimeInputs = [ updateFlakes_ pushToCachix_ ];
          text = ''
            ${updateFlakes_.name}
            ${pushToCachix_.name}
          '';
        };


      # Stuff for tests

      tools902 = builtins.attrValues { inherit (toolsGHC "902") hls stack; };

      writeSettings = writeSettingsJson settingsNix;

      devShells = mkDevShellsWithEntryPoint { name = "enter"; }
        (
          let
            buildInputs = pkgs.lib.lists.flatten [
              (toList shellTools)
              tools902
              [ codium ]
              [
                pushDevShellsToCachix
                pushPackagesToCachix
                pushInputsToCachix
                pushAllToCachix
              ]
            ]
            ;
          in
          {
            inherit buildInputs;
            # shellHook = "stack --version";
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
          }
        )
        {
          checkScripts = { buildInputs = [ pkgs.gawk ]; };
          s = { };
        };

      packages = {
        default = mkShellApp { name = "codium-start"; runtimeInputs = [ codium ]; text = "codium ."; };
        inherit json2nix;
        inherit pushDevShellsToCachix;
      };
    in
    {
      # use just these tools
      # packages and devShells are just for demo purposes
      tools = {
        inherit
          # build inputs
          codium

          # configs
          extensions settingsNix

          # shell apps
          json2nix
          pushDevShellsToCachix
          pushPackagesToCachix
          pushInputsToCachix
          pushAllToCachix

          # functions
          justStaticExecutables
          mergeValues
          mkCodium
          mkDevShells
          mkDevShellsWithEntryPoint
          runInEachDir
          toList
          toolsGHC
          writeJson
          writeSettingsJson
          mkShellApp
          writeTasksJson
          fullUpdate
          updateFlakes
          pushToCachix

          # tool sets
          shellTools;
      };
      inherit devShells;
      inherit packages;
      formatter = pkgs.nixpkgs-fmt;
    });

  nixConfig = {
    extra-substituters = [
      https://haskell-language-server.cachix.org
      https://nix-community.cachix.org
      https://hydra.iohk.io
      https://br4ch1st0chr0n3.cachix.org
    ];
    extra-trusted-public-keys = [
      haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8=
      nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
      hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
      br4ch1st0chr0n3.cachix.org-1:o1FA93L5vL4LWi+jk2ECFk1L1rDlMoTH21R1FHtSKaU=
    ];
  };
}
