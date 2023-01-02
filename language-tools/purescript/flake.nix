{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
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
        inherit (pkgs) dhall-lsp-server nodejs-16_x purescript;
        inherit (pkgs.nodePackages) purescript-language-server purs-tidy;
        spago = pkgs.lib.meta.addMetaAttrs { description = "PureScript compiler"; } pkgs.spago;
      };
    in
    {
      inherit shellTools;
    }
    );
}
