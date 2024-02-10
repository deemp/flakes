{ config
, options
, configuration ? { }
, ...
}@args:
let
  lib = import ./lib.nix { inherit (args) lib; };

  configuration = lib.attrsOrFunc args.configuration {
    inherit (config.accessible) workflows actions;
  };

  initial = import ./initial.nix { inherit lib; };

  resolved = import ./resolved.nix { inherit lib config; };

  common = import ./common.nix { inherit lib; };

  accessible = import ./accessible.nix { inherit lib common; };

  utils = import ./utils.nix { inherit lib; };
in
{
  options = {
    inherit (initial) workflows actions;

    resolved = lib.mkOption {
      type = lib.types.anything;
      default = { };
    };

    accessible = lib.mkOption {
      type = lib.types.submodule {
        options = {
          actions = common.options.actions;
          workflows = accessible.options.workflows;
        };
      };
    };

    clean = lib.mkOption {
      type = lib.types.anything;
      default = { };
    };
  };

  config = {
    inherit (configuration) workflows actions;

    accessible = {
      inherit (config) actions;

      workflows =
        lib.mapAttrs
          (_: workflow:
            workflow
            //
            {
              actions = lib.recursiveUpdate config.actions workflow.actions;
              jobs =
                lib.mapAttrs
                  (_: job:
                    job
                    //
                    {
                      steps =
                        lib.pipe job.steps [
                          (map utils.convertUses)
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
                        ]
                      ;
                    }
                  )
                  workflow.jobs;
            })
          config.workflows;
    };

    clean = utils.cleanWorkflows config.workflows;
  };
}
