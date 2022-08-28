{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/cd8bbdd4fdb15f7758fd3c8df531e9d42bee239d";
    flake-utils.url = "github:numtide/flake-utils/7e2a3b3dfd9af950a856d66b0a7d01e3c18aa249";
    vscode-marketplace = {
      url = "github:AmeerTaweel/nix-vscode-marketplace/499969e5c64daf3d20cb077a6230438d490200c1";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    npmlock2nix_ = {
      url = "github:nix-community/npmlock2nix/5c4f247688fc91d665df65f71c81e0726621aaa8";
      # url = "github:tlxzilia/npmlock2nix/f63dc087b144fb608e99e6239ceb69c68449482b";
      flake = false;
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix/5926981701ac781f08b02e31e4705e46b799299d";
      flake = false;
    };
    my-vscode-marketplace = {
      url = "github:br4ch1st0chr0n3/nix-vscode-marketplace/94fd67826a10bc4d9c27f06b342cf2e17b848841";
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
    , npmlock2nix_
    , my-vscode-marketplace
    }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};

      # A set of VS Code extensions
      # Biased towards FP languages
      vscode-extensions =
        let
          inherit (vscode-marketplace.packages.${system}) vscode open-vsx;
          my-vscode-extensions = my-vscode-marketplace.packages.${system}.vscode;
        in
        {
          inherit (open-vsx.nwolverson) ide-purescript language-purescript;
          inherit (open-vsx.haskell) haskell;
          inherit (open-vsx.mkhl) direnv;
          inherit (open-vsx.dhall) dhall-lang vscode-dhall-lsp-server;
          inherit (open-vsx.usernamehw) errorlens;
          inherit (open-vsx.github) vscode-pull-request-github;
          inherit (open-vsx.eamodio) gitlens;
          inherit (open-vsx.justusadam) language-haskell;
          inherit (open-vsx.ms-vscode) vscode-typescript-next;
          inherit (vscode.ryuta46) multi-command;
          inherit (open-vsx.jnoortheen) nix-ide;
          inherit (open-vsx.gruntfuggly) todo-tree;
          inherit (open-vsx.redhat) vscode-yaml;
          inherit (my-vscode-extensions.ms-vscode-remote) remote-containers;
          inherit (my-vscode-extensions.visortelle) haskell-spotlight;
          inherit (my-vscode-extensions.br4ch1st0chr0n3) purs-hotkeys;
        };

      # tools you may need for PureScript development
      pursTools =
        let
          easy-ps = import easy-purescript-nix { inherit pkgs; };
        in
        {
          inherit (pkgs) nodejs-16_x dhall-lsp-server;
          inherit (easy-ps) purs-0_15_4 spago purescript-language-server;
        };

      nodeTools = {
        npmlock2nix = import npmlock2nix_ { inherit pkgs; };
      };

      # create a codium with a given set of extensions
      mkCodium =
        let
          inherit (pkgs) vscode-with-extensions vscodium;
        in
        attrSet: (vscode-with-extensions.override {
          vscode = vscodium;
          vscodeExtensions = builtins.attrValues attrSet;
        });


      # a set of settings for settings.json
      # pre-defined settings can be combined as follows
      # settings.todo-tree // settings.purescript
      settings-nix = import ./settings.nix;

      # write project settings into nix/store and create a symlink in the project directory
      # set -> IO ()
      writeSettingsJson = settings:
        let
          s = "settings.json";
          settingsJson = builtins.toJSON settings;
          write-settings = pkgs.mkShell {
            buildInputs = [ pkgs.python38 ];
            buildPhase = ''
              mkdir -p $out
              printf "%s" '${settingsJson}' | python -m json.tool > $out/${s}
            '';
          };
        in
        pkgs.mkShell {
          buildInputs = [ write-settings ];
          shellHook = ''
            mkdir -p .vscode
            ln -sf ${write-settings}/${s} .vscode/${s}
          '';
        };

      # convert json to nix
      # Example: nix develop; json2nix ./settings.json ./settings.nix
      json2nix = pkgs.writeScriptBin "json2nix" ''
        json_path=$1
        nix_path=$2
        pkgs="with import ${nixpkgs} { }"
        p="$pkgs; with builtins; fromJSON (readFile $json_path)"
        nix-instantiate --eval "$p" -E  > $nix_path
        sed -i -E "s/(\[|\{)/\1\n/g" $nix_path
        nix run ${nixpkgs}#nixpkgs-fmt $nix_path
      '';
    in
    {
      packages = {
        inherit vscode-extensions pursTools nodeTools
          mkCodium settings-nix writeSettingsJson json2nix;
      };
      devShells.default = pkgs.mkShell {
        buildInputs = [ json2nix ];
      };
    });
}
