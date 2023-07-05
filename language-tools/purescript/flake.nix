{
  inputs.flakes.url = "github:deemp/flakes";

  outputs = inputsTop:
    let
      inputs_ =
        let flakes = inputsTop.flakes.flakes; in
        {
          inherit (flakes.source-flake) flake-utils nixpkgs nixpkgs-purescript;
        };

      outputs = flake { } // {
        inherit flake;
        inputs = inputs_;
      };

      flake =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
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
        );
    in
    outputs;
}
