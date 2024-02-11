{ lib }@args:
{ configuration, extraSpecialArgs ? { } }:
let
  module = lib.evalModules {
    modules = [
      ../modules/initial.nix
      ../modules/accessible.nix
      ../modules/clean.nix
    ];
    specialArgs =
      let
        lib = import ./lib.nix { inherit (args) lib; };
        utils = import ./utils.nix { inherit lib; };
      in
      {
        configuration = configuration {
          inherit (module.config.accessible) workflows actions;
          inherit (utils) qq;
          inherit (lib.values) null_;
        };
        common = import ./common.nix { inherit lib; };
        inherit utils;
      } // extraSpecialArgs;
  };
in
{
  inherit (module) config options;
}
