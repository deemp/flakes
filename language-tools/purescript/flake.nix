{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      shellTools = {
        inherit (pkgs) dhall-lsp-server spago nodejs-16_x purescript;
        inherit (pkgs.nodePackages) purescript-language-server purs-tidy;
      };
    in
    {
      toolSets = {
        inherit shellTools;
      };
    }
    );
}
