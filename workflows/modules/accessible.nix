{ lib
, config
, common
, utils
, ...
}:
{
  options = {
    accessible = lib.mkOption {
      type = lib.types.submodule {
        options = {
          actions = common.options.actions;
          workflows = common.mkWorkflowsOption {
            type =
              lib.types.attrsOf (lib.types.submodule {
                options = common.options.step;
              });
            default = { };
          };
        };
      };
    };
  };

  config = {
    accessible = {
      inherit (config) actions;
      workflows =
        utils.resolveWorkflows {
          inherit config;
          stepsPipe = [
            (
              lib.foldl'
                (res: x: {
                  idx = res.idx + 1;
                  acc = res.acc // {
                    ${
                    if x?id && x.id != null then x.id
                    else if x?name && x.name != null then x.name
                    else builtins.toString res.idx
                    } = x;
                  };
                })
                { idx = 1; acc = { }; }
            )
            (x: x.acc)
          ];
        };
    };
  };
}
