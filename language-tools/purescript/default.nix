{ system ? builtins.currentSystem
, pkgs ? (import ../..).inputs.nixpkgs.legacyPackages.${system}
, drv-tools ? (import ../../drv-tools { inherit system pkgs; })
}:
let
  lib = drv-tools.lib.mergeAttrsRecursive [ builtins pkgs.lib drv-tools.lib ];
  packages = {
    inherit (pkgs) dhall-lsp-server nodejs_20 purescript;
    inherit (pkgs.nodePackages) purescript-language-server purs-tidy bower;
    spago = pkgs.lib.meta.addMetaAttrs { description = "PureScript build tool and package manager"; } pkgs.spago;
  };
  devShells.default = pkgs.mkShell { buildInputs = lib.attrValues packages; };
in
{
  inherit packages devShells;
}
