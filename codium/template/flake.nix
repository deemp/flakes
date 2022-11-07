{
  inputs = {
    nixpkgs_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = "github:br4ch1st0chr0n3/flakes?dir=codium";
    flake-utils_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    vscode-extensions_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/vscode-extensions";
    vscode-extensions.follows = "vscode-extensions_/vscode-extensions";
  };
  outputs =
    { self
    , nixpkgs
    , my-codium
    , flake-utils
    , vscode-extensions
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
              inherit (open-vsx.justusadam) language-haskell;
              inherit (vscode.visortelle) haskell-spotlight;
            };
          };
          runtimeDependencies = [ pkgs.haskell-language-server ];
        };
      in
      {
        packages.default = codium;
      });
}
