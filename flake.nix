{
  inputs = {
    # nixpkgs-stable.url = "github:NixOS/nixpkgs/67e45078141102f45eff1589a831aeaa3182b41e";
    nixpkgs.url = "github:NixOS/nixpkgs/0e304ff0d9db453a4b230e9386418fd974d5804a";
    flake-utils.url = "github:numtide/flake-utils";
    my-codium = {
      url = "github:br4ch1st0chr0n3/flakes?dir=codium&rev=5317c7a3882a96bc8ee92bb4979edaac6047a183";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat/b4a34015c698c7793d592d66adbab377907a2be8";
      flake = false;
    };
    hls = {
      url = "github:haskell/haskell-language-server/7760340e999693d07fdbea49c9e20a3dd5458ad3";
      # inputs.nixpkgs.follows = "nixpkgs-stable";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix/a20de23b925fd8264fd7fad6454652e142fd7f73";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , my-codium
    , flake-compat
    , hls
    , gitignore
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      ghcV = "902";
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (pkgs.haskell.packages."ghc${ghcV}") callCabal2nix;
      inherit (pkgs.haskell.lib) justStaticExecutables;
      inherit (gitignore.lib) gitignoreSource;

      manager = justStaticExecutables (callCabal2nix "manager" (gitignoreSource ./manager) { });

      inherit (my-codium.packages.${system})
        writeSettingsJson
        settingsNix
        extensions
        codium
        mergeValues
        toList
        shellTools
        json2nix
        ;
      writeSettings = writeSettingsJson
        {
          inherit (settingsNix) haskell todo-tree files editor gitlens git nix-ide;
          window."window.zoomLevel" = 0.3;
          workbench = settingsNix.workbench // { "workbench.colorTheme" = "Monokai"; };
        };
    in
    {
      devShells =
        {
          default = pkgs.mkShell {
            name = "codium";
            buildInputs = pkgs.lib.lists.flatten
              [
                (toList { inherit (shellTools) nix; haskell = builtins.removeAttrs shellTools.haskell [ "haskell-language-server" ]; })
                (pkgs.haskell.compiler."ghc${ghcV}")
                codium
                json2nix
                writeSettings
                manager
                hls.packages.${system}."haskell-language-server-${ghcV}"
              ];
            shellHook = ''
              write-settings
              source <(manager --bash-completion-script `which manager`)
            '';
          };
        };

      stack-shell = { ghcVersion }:

        pkgs.haskell.lib.buildStackProject {
          name = "nix-managed-stack-shell";

          ghc = pkgs.haskell.compiler.${ghcVersion};

          buildInputs = [
            pkgs.zlib
          ];
        };
    });

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://br4ch1st0chr0n3-acpoj.cachix.org"
      "https://br4ch1st0chr0n3-flakes.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "br4ch1st0chr0n3-acpoj.cachix.org-1:OYzTMty0XgyEIcm+o9tjKotr9ZjNeC4JCWmc0P0nx3U="
      "br4ch1st0chr0n3-flakes.cachix.org-1:Dyc2yLlRIkdbq8CtfOe24QQhQVduQaezkyV8J9RhuZ8="
    ];
  };
}
