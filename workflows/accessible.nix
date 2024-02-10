{ lib, common }:
rec {
  options = rec {
    job = common.options.job // {
      steps = lib.mkOption {
        type = lib.types.attrsOf (lib.types.submodule { options = step; });
        default = { };
      };
    };

    step = common.options.step // {
      uses = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };
    };

    workflows = common.mkWorkflowsOption {
      type =
        lib.types.attrsOf (lib.types.submodule {
          options = {
            inherit (common.options) id name with';
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
      default = { };
    };
  };
}
