{ lib }:
{ configuration, extraSpecialArgs ? { } }:
let
  module = lib.evalModules {
    modules = [
      ../modules/initial.nix
      ../modules/accessible.nix
      ../modules/clean.nix
    ];
    specialArgs = {
      inherit configuration;
      lib = import ./lib.nix { inherit lib; };
      common = import ./common.nix { inherit lib; };
      utils = import ./utils.nix { inherit lib; };
    } // extraSpecialArgs;
  };
in
{
  inherit (module) config options;
}
