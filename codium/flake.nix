{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/0e304ff0d9db453a4b230e9386418fd974d5804a";
    flake-utils.url = "github:numtide/flake-utils/7e2a3b3dfd9af950a856d66b0a7d01e3c18aa249";
    vscode-marketplace = {
      url = "github:AmeerTaweel/nix-vscode-marketplace/499969e5c64daf3d20cb077a6230438d490200c1";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix/5926981701ac781f08b02e31e4705e46b799299d";
      flake = false;
    };
    nix-vsode-marketplace = {
      url = "github:br4ch1st0chr0n3/nix-vscode-marketplace/d567fba043784bb456407f60c21230ea4d82253f";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , vscode-marketplace
    , easy-purescript-nix
    , nix-vsode-marketplace
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # A set of VS Code extensions
        # Biased towards FP languages
        vscodeExtensions =
          let
            inherit (vscode-marketplace.packages.${system}) vscode open-vsx;
            my-vscode-extensions = nix-vsode-marketplace.packages.${system}.vscode;
          in
          {
            haskell = {
              inherit (open-vsx.haskell) haskell;
              inherit (open-vsx.justusadam) language-haskell;
              inherit (my-vscode-extensions.visortelle) haskell-spotlight;
              inherit (open-vsx.redhat) vscode-yaml;
            };
            purescript = {
              inherit (open-vsx.nwolverson) ide-purescript language-purescript;
              inherit (open-vsx.dhall) dhall-lang vscode-dhall-lsp-server;
              inherit (my-vscode-extensions.br4ch1st0chr0n3) purs-keybindings;
              inherit (my-vscode-extensions.ryuta46) multi-command;
              inherit (my-vscode-extensions.chunsen) bracket-select;
            };
            nix = {
              inherit (open-vsx.mkhl) direnv;
              # inherit (open-vsx.arrterian) nix-env-selector;
              inherit (open-vsx.jnoortheen) nix-ide;
            };
            github = {
              inherit (open-vsx.github) vscode-pull-request-github;
              inherit (open-vsx.eamodio) gitlens;
            };
            typescript = {
              inherit (open-vsx.ms-vscode) vscode-typescript-next;
            };
            misc = {
              inherit (open-vsx.usernamehw) errorlens;
              inherit (open-vsx.gruntfuggly) todo-tree;
            };
            docker = {
              # TODO update vscodium to 1.71.0
              inherit (my-vscode-extensions.ms-vscode-remote) remote-containers;
            };
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

          node = {
            inherit (pkgs) nodejs-16_x;
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
              # LSP server for GHC
              ;
            # The Haskell Tool Stack
            inherit (pkgs) haskell-language-server;
            inherit stack-wrapped;
          };
        };

        # Wrap Stack to work with our Nix integration. 
        # If you use this stack version, you should supply the appropriate ghc in your .nix
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
        # Examples: 
        # see `codium`
        mkCodium = extensions@{ ... }:
          let
            inherit (pkgs) vscode-with-extensions vscodium;
          in
          (vscode-with-extensions.override {
            vscode = vscodium;
            vscodeExtensions = toList extensions;
          });

        # write settings.json into ./.vscode
        # Example:
        # see devShells.default.shellHook
        writeSettingsJson = settings:
          let
            s = "settings.json";
            settingsJson = builtins.toJSON (mergeValues settings);

            writeSettings = pkgs.mkShell {
              name = "write-to-store";
              buildInputs = [ pkgs.python38 ];
              buildPhase = ''
                mkdir -p $out
                printf "%s" '${settingsJson}' | python -m json.tool > $out/${s}
              '';
            };
            path = "${writeSettings.out}/${s}";
            vscode = ".vscode";
          in
          pkgs.writeShellApplication
            {
              name = "write-settings";
              text = ''
                mkdir -p ${vscode}
                cat ${path} > ${vscode}/${s}
              '';
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
        codium = [ (mkCodium vscodeExtensions) pkgs.bashInteractive ];
      in
      {
        packages = {
          inherit
            allShellTools
            codium
            json2nix
            mergeValues
            mkCodium
            shellTools
            settingsNix
            vscodeExtensions
            writeSettingsJson
            toList
            ;
        };
        devShells =
          let
            myDevTools = pkgs.lib.lists.flatten [
              (toList shellTools)
              codium
              (writeSettingsJson settingsNix)
            ];
          in
          {
            default = pkgs.mkShell {
              name = "my-codium";
              buildInputs = myDevTools;
              shellHook = ''
                write-settings
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
