{
  inputs = {
    inputs.url = github:br4ch1st0chr0n3/flakes?dir=inputs;
    nixpkgs.follows = "inputs/nixpkgs";
    flake-utils.follows = "inputs/flake-utils";
    gitignore.follows = "inputs/gitignore";
    easy-purescript-nix.follows = "inputs/easy-purescript-nix";
    haskell-language-server.follows = "inputs/haskell-language-server";
    nix-vscode-marketplace.follows = "inputs/nix-vscode-marketplace";
    vscodium-extensions.follows = "inputs/vscodium-extensions";
  };
  outputs =
    { self
    , inputs
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
        # mergeValues {a = {b = 1;}; c = {d = 1;};} => {b = 1; d = 1;}
        # mergeValues ({inherit (vscode-extensions) haskell purescript;})
        mergeValues = set@{ ... }:
          builtins.foldl' pkgs.lib.mergeAttrs { } (builtins.attrValues set);

        # nixified and restructured settings.json
        # one can combine the settings as follows:
        # settingsNix.haskell // settingsNix.purescript
        settingsNix = import ./settings.nix;

        # a convenience function that flattens
        toList = x: builtins.attrValues (mergeValues x);

        # shell tools for development
        # Example
        # mergeValues { inherit (settings) todo-tree purescript; }
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
        # Examples: 
        # see `codium`
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

        # write settings.json into ./.vscode
        # Example:
        # see devShells
        writeSettingsJson = settings:
          let
            vscode = ".vscode";
            s = "settings.json";
            settingsJson = builtins.toJSON (mergeValues settings);
          in
          pkgs.writeShellApplication {
            name = "write-settings-json";
            runtimeInputs = [ pkgs.python38 ];
            text = ''
              mkdir -p ${vscode}
              printf "%s" ${pkgs.lib.escapeShellArg settingsJson} | python -m json.tool > ${vscode}/${s}
            '';
            checkPhase = "";
          };

        # convert json to nix
        # no need to provide the full path to a file if it's in the cwd
        # Example: 
        # nix run .#json2nix settings.json settings.nix
        json2nix = pkgs.writeScriptBin "json2nix" ''
          json_path=$1
          nix_path=$2
          pkgs="with import ${nixpkgs} { }"
          p="$pkgs; with builtins; fromJSON (readFile ./$json_path)"
          nix-instantiate --eval "$p" -E  > $nix_path
          sed -i -E "s/(\[|\{)/\1\n/g" $nix_path
          nix run ${nixpkgs}#nixpkgs-fmt $nix_path
        '';

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
      in
      {
        tools = {
          inherit
            allShellTools
            codium
            json2nix
            mergeValues
            mkCodium
            shellTools
            settingsNix
            extensions
            writeSettingsJson
            toList
            toolsGHC
            justStaticExecutables
            ;
        };
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
              buildInputs = [ codium (writeSettingsJson settingsNix) ];
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
      "https://haskell-language-server.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
  };
}
