{
  inputs = {
    nixpkgs_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = github:br4ch1st0chr0n3/flakes?dir=codium;
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs =
    { self
    , nixpkgs
    , my-codium
    , flake-utils
    , ...
    }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.functions.${system}) mkCodium;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      codium = mkCodium {
        extensions = { inherit (extensions) nix misc haskell; };
        runtimeDependencies = [ pkgs.haskell-language-server ];
      };
    in
    {
      packages.default = codium;
    });
}
