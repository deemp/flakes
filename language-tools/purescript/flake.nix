{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    nixpkgs-purescript.url = "github:deemp/nixpkgs/purescript";
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      pkgs-purescript = inputs.nixpkgs-purescript.legacyPackages.${system};
      packages = {
        inherit (pkgs-purescript) purescript;
        inherit (pkgs) dhall-lsp-server nodejs_18;
        inherit (pkgs.nodePackages) purescript-language-server purs-tidy;
        spago = pkgs.lib.meta.addMetaAttrs { description = "PureScript build tool and package manager"; } pkgs.spago;
      };
    in
    {
      inherit packages;
    }
    );
}
