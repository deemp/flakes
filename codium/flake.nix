{
  inputs = {
    my-inputs.url = github:br4ch1st0chr0n3/flakes?dir=inputs;
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
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
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
            let
              easy-ps = import easy-purescript-nix { inherit pkgs; };
            in
            {
              inherit (pkgs) dhall-lsp-server;
              inherit (easy-ps) purs-0_15_4 spago purescript-language-server purs-tidy;
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
              # Easy dependency management for Nix projects.
              hpack
              # GHCi based bare bones IDE
              ghcid
              ;
            # The Haskell Tool Stack
            inherit (pkgs) haskell-language-server;
          };
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

        # a set of all shell tools
        allShellTools = mergeValues shellTools;

        # create a codium with a given set of extensions
        # bashInteractive is necessary for correct work
        mkCodium = extensions@{ ... }:
          let
            inherit (pkgs) vscode-with-extensions vscodium;
          in
          [
            (vscode-with-extensions.override {
              vscode = vscodium;
              vscodeExtensions = toList extensions;
            })
            pkgs.bashInteractive
          ];

        # ignore shellcheck when writing a shell application
        writeShellApp = args@{ ... }: pkgs.writeShellApplication (args // {
          runtimeInputs = pkgs.lib.lists.flatten args.runtimeInputs;
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
          writeShellApp {
            name = name_;
            runtimeInputs = [ pkgs.python310 ];
            text = ''
              mkdir -p ${dir}
              printf "%s" ${pkgs.lib.escapeShellArg dataJson} | python -m json.tool > ${path}
              printf "\n[ok %s]\n" "${name_}"
            '';
          };

        # write .vscode/settings.json
        writeSettingsJson = settings: writeJson "settings" "./.vscode/settings.json" (mergeValues settings);

        # write .vscode/tasks.json
        writeTasksJson = tasks: writeJson "tasks" "./.vscode/tasks.json" tasks;

        # convert json to nix
        # no need to provide the full path to a file if it's in the cwd
        # json2nix .vscode/settings.json my-settings.nix
        json2nix = writeShellApp {
          name = "json2nix";
          runtimeInputs = [ pkgs.nixpkgs-fmt ];
          text =
            ''
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
        # dep = callCabal "dep-name" ./dep-path { };
        # my-package = callCabal "my-package-name" ./my-package-path { inherit dep; };
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
          justStaticExecutables (callCabal2nix name (gitignoreSource path) { });

        # stack and ghc of a specific version
        # they should come together so that stack doesn't use the system ghc
        stack = ghcVersion: [
          stack-wrapped
          pkgs.haskell.compiler."ghc${ghcVersion}"
        ];

        # this version of HLS is only for aarch64-darwin, x86_64-darwin, x86_64-linux
        hls = ghcVersion: haskell-language-server.packages.${system}."haskell-language-server-${ghcVersion}";

        # tools for a specific GHC version
        toolsGHC = ghcVersion: {
          hls = hls ghcVersion;
          stack = stack ghcVersion;
          callCabal = callCabalGHC ghcVersion;
          staticExecutable = staticExecutable ghcVersion;
        };

        tools902 = builtins.attrValues ({
          inherit (toolsGHC "902") hls stack;
        });
        writeSettings = writeSettingsJson settingsNix;

        # create compilable devshells
        mkDevShells = shells@{ ... }: builtins.mapAttrs
          (name: value:
            writeShellApp (value // {
              inherit name;
              text =
                let
                  MY_SHELL_NAME = "MY_SHELL_NAME";
                in
                ''${MY_SHELL_NAME}=${name} bash --rcfile ${./scripts/devshells.sh}'';
            })
          )
          shells;

        devShells = mkDevShells {
          myShell = {
            runtimeInputs = [ json2nix writeSettings ];
          };
        };
      in
      {
        # use just these tools
        # packages and devShells are just for demo purposes
        tools = {
          inherit
            allShellTools
            codium
            extensions
            json2nix
            justStaticExecutables
            mergeValues
            mkCodium
            settingsNix
            shellTools
            toList
            toolsGHC
            writeJson
            writeSettingsJson
            writeShellApp
            writeTasksJson
            mkDevShells
            ;
        };
        packages = {
          inherit writeSettings json2nix codium;
        } // devShells;
        devShells =
          let
            myDevTools =
              [
                (toList shellTools)
                tools902
              ];
          in
          {
            default = pkgs.mkShell {
              name = "my-codium";
              buildInputs = myDevTools;
            };

            # if you want to use codium, you'd want to have the appropriate settings
            codium = pkgs.mkShell {
              buildInputs = [ codium writeSettings ];
              shellHook = ''
                write-settings-json
                codium .
              '';
            };

            # From here: https://docs.haskellstack.org/en/stable/nix_integration/
            # Make external Nix c libraries like zlib known to GHC, like pkgs.haskell.lib.buildStackProject does
            # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
          };
      }
    );

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
