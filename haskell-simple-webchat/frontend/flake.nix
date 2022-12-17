{
  description = "Webchat frontend";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/cd8bbdd4fdb15f7758fd3c8df531e9d42bee239d";
    flake-utils.url = "github:numtide/flake-utils/c0e246b9b83f637f4681389ecabcb2681b4f3af0";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , easy-purescript-nix
    }:
      with flake-utils.lib;
      eachSystem [ system.x86_64-linux ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        easy-ps = import easy-purescript-nix { inherit pkgs; };
        myTools = builtins.attrValues {
          inherit (pkgs)
            nodejs-16_x
            dhall-lsp-server
            ;
          inherit (easy-ps)
            purs-0_15_4
            spago
            purescript-language-server
            ;
        };
      in
      {
        devShells.default =
          pkgs.mkShell
            {
              buildInputs = myTools;
            } // { shellHooks = "spago build"; };
      });
}
