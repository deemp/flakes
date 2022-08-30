{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/cd8bbdd4fdb15f7758fd3c8df531e9d42bee239d";
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
    haskell-language-server = {
      url = "github:haskell/haskell-language-server/7760340e999693d07fdbea49c9e20a3dd5458ad3";
      inputs.poetry2nix.inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    nix-vsode-marketplace = {
      url = "github:br4ch1st0chr0n3/nix-vscode-marketplace/a582ecb728bf4d49671210d110f1764271467e1c";
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
    , haskell-language-server
    }:
    flake-utils.lib.eachDefaultSystem (system:
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
          };
          nix = {
            inherit (open-vsx.mkhl) direnv;
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
            inherit (my-vscode-extensions.ms-vscode-remote) remote-containers;
          };
        };

      # if a set's attribute values are all sets, merge these values
      # Examples:
      # mergeValues {a = {b = 1;}; c = {d = 1;};} => {b = 1; d = 1;}
      # mergeValues ({inherit (vscode-extensions) haskell purescript;})
      mergeValues = set@{ ... }:
        builtins.foldl' pkgs.lib.mergeAttrs { } (builtins.attrValues set);
      # builtins.foldl' (x: y: x // y) { } (builtins.attrValues set);

      # a set of all extensions
      allVSCodeExtensions = mergeValues vscodeExtensions;

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
          inherit (haskell-language-server.packages.${system})
            haskell-language-server-902
            # haskell-language-server-924
            ;
        };
      };

      # a set of all shell tools
      allShellTools = mergeValues shellTools;

      # create a codium with a given set of extensions
      # Example:
      # mkCodium {inherit (vscode-extensions) haskell purescript;}
      mkCodium = extensions@{ ... }:
        let
          inherit (pkgs) vscode-with-extensions vscodium;
        in
        (vscode-with-extensions.override {
          vscode = vscodium;
          vscodeExtensions = builtins.attrValues (mergeValues extensions);
        });


      # nixified settings.json
      # Example:
      # mergeValues { inherit (settings) todo-tree purescript; }
      settingsNix = import ./settings.nix;

      # write settings.json somewhere into nix/store and create a symlink in .vscode
      writeSettingsJson = settings:
        let
          s = "settings.json";
          settingsJson = builtins.toJSON settings;
          writeSettings = pkgs.mkShell {
            buildInputs = [ pkgs.python38 ];
            buildPhase = ''
              mkdir -p $out
              printf "%s" '${settingsJson}' | python -m json.tool > $out/${s}
            '';
          };
        in
        pkgs.mkShell {
          buildInputs = [ writeSettings ];
          shellHook = ''
            mkdir -p .vscode
            ln -sf ${writeSettings}/${s} .vscode/${s}
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

      codium = mkCodium vscodeExtensions;
    in
    {
      packages = {
        inherit
          allVSCodeExtensions
          codium
          json2nix
          mergeValues
          mkCodium
          shellTools
          settingsNix
          vscodeExtensions
          writeSettingsJson
          ;
      };
      devShells = {
        default = pkgs.mkShell rec {
          buildInputs = (builtins.attrValues (mergeValues shellTools)) ++ [ codium ]
          ;
        };
      };
    });

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
  };
}
