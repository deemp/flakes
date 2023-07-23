{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs: inputs.flakes.makeFlake {
    inputs = { inherit (inputs.flakes.all) nixpkgs nixpkgs-purescript; };
    perSystem = { inputs, system }:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        pkgs-purescript = inputs.nixpkgs-purescript.legacyPackages.${system};
        packages = {
          inherit (pkgs-purescript) purescript;
          inherit (pkgs) dhall-lsp-server nodejs_18;
          inherit (pkgs.nodePackages) purescript-language-server purs-tidy bower;
          spago = pkgs.lib.meta.addMetaAttrs { description = "PureScript build tool and package manager"; } pkgs.spago;
        };
        devShells.default = pkgs.mkShell { buildInputs = __attrValues packages; };
      in
      {
        inherit packages devShells;
      }
    ;
  };
}
