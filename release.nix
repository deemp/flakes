{ pkgs ? import <nixpkgs> {}
, platform ? import ./.
}:

pkgs.writeScriptBin "serve" ''
  #!${pkgs.stdenv.shell}
  ${platform.ghc.webchat-backend}/bin/webchat-backend ${platform.ghcjs.webchat-frontend}/bin/webchat-frontend.jsexe/
''
