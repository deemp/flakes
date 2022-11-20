{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = "github:deemp/flakes?dir=codium";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    vscode-extensions_.url = "github:deemp/flakes?dir=source-flake/vscode-extensions";
    vscode-extensions.follows = "vscode-extensions_/vscode-extensions";
    my-devshell.url = "github:deemp/flakes?dir=devshell";
  };
  outputs =
    { self
    , nixpkgs
    , my-codium
    , flake-utils
    , vscode-extensions
    , my-devshell
    , ...
    }: flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (my-codium.functions.${system}) mkCodium;
        inherit (my-codium.configs.${system}) extensions;
        inherit (vscode-extensions.packages.${system}) vscode open-vsx;
        codium = mkCodium {
          extensions = {
            inherit (extensions) nix misc;
            haskell = {
              inherit (open-vsx.haskell) haskell;
              inherit (vscode.visortelle) haskell-spotlight;
            };
          };
          runtimeDependencies = [ pkgs.hello ];
        };
        devshell = my-devshell.devshell.${system};
      in
      {
        devShells.default = devshell.mkShell
          {
            packages = [ codium ];
            bash = {
              extra = ''
                printf "Hello!\n"
              '';
            };
            commands = [
              {
                name = "codium";
                help = "VSCodium with `hello` binary on `PATH` and a couple of extensions";
                category = "ide";
              }
            ];
          };
      });

  nixConfig = {
    extra-trusted-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://hydra.iohk.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
