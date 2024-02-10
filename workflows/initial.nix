{ lib }:
let
  common = import ./common.nix { inherit lib; };
in
{
  inherit (common.options) actions;

  workflows = common.mkWorkflowsOption {
    type =
      lib.types.nonEmptyListOf (lib.types.submodule {
        options = {
          inherit (common.options) id name;
          uses = lib.mkOption {
            type = lib.types.oneOf [
              (lib.types.nullOr lib.types.str)
              (
                (lib.types.attrsOf common.options.action) // {
                  check = x:
                    builtins.isAttrs x
                    && builtins.length (builtins.attrValues x) == 1;
                }
              )
            ];
            default = null;
          };
        };
      });
    default = [ ];
  };
}
