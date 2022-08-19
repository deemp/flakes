{ system
, pkgs
}:
let
  backend = pkgs.haskellPackages.callCabal2nix "backend" (pkgs.gitignoreSource ./backend) { };
in
pkgs.haskell.lib.justStaticExecutables backend
