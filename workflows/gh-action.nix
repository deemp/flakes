{ config
, options
, lib
, configuration ? { }
  # possible attributes: 
  # `actions` and `workflows`
  # 
  # a function or an attrset
  # 
  # define a `name` and default `with` values
  # 
  # actions = { checkout2, ... }: {
  #   checkout1 = {
  #     name = "actions/checkout@v4";
  #     with' = {
  #       repository = "repo";
  #     };
  #   };
  #   checkout2 = {
  #     name = checkout1.name
  #     with' = {
  #       repository = "repo1";
  #     }
  #   };
  # };
, ...
}@args:
let
  # TODO actionsOptions
  # allow people to provide custom options?

  attrsOrFunc = x: arg: if builtins.isAttrs x then x else x arg;

  configuration = attrsOrFunc args.configuration { inherit (config.accessible) workflows actions; };

  # TODO enable later because it's sugar
  # workflows = attrsOrFunc (configuration.workflows or { }) config.workflows;
  # actions = attrsOrFunc (x.actions or { }) config.accessible.actions;

  # TODO
  # internal
  # 
  # or, better, not expose config at all

  mkActionsOptions =
    lib.mapAttrs
      (name: value: lib.mkOption {
        type = lib.types.submodule {
          options = {
            name = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = value.name or null;
            };
            with' = lib.mkOption {
              type =
                lib.types.nullOr (
                  lib.types.submodule {
                    options =
                      lib.mapAttrs
                        (name': value': lib.mkOption {
                          type = lib.types.nullOr lib.types.str;
                          default = value';
                        })
                        value.with';
                  }
                );
              default = { };
            };
          };
        };
        default = { };
      })
  ;

  stepOptionsCommon = {
    id = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
    };

    name = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
    };

    with' = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
    };
  };

  stepOptions = actions: stepOptionsCommon // {
    uses = lib.mkOption {
      type =
        lib.types.oneOf (
          [
            (lib.types.nullOr lib.types.str)
          ]
          ++
          lib.mapAttrsToList
            (name: value: (lib.types.submodule { options.${name} = value; }))
            (mkActionsOptions actions)
        );
      default = null;
    };
  };

  accessible.stepOptions = stepOptionsCommon // {
    uses = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
    };
  };

  clean.stepOptions = accessible.stepOptions;

  nonEmptyListOf = elemType: lib.types.nonEmptyListOf elemType // {
    # TODO remove after switching to nixpkgs
    substSubModules = m: nonEmptyListOf (elemType.substSubModules m);
  };

  jobOptionsCommon = {
    name = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
    };
  };

  jobOptions = actions: jobOptionsCommon // {
    steps = lib.mkOption {
      type = nonEmptyListOf (lib.types.submodule { options = stepOptions actions; });
      default = [ ];
    };
  };

  accessible.jobOptions = jobOptionsCommon // {
    steps = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule { options = accessible.stepOptions; });
      default = { };
    };
  };

  clean.jobOptions = jobOptionsCommon // {
    steps = lib.mkOption {
      type = nonEmptyListOf (lib.types.submodule { options = clean.stepOptions; });
      default = [ ];
    };
  };

  workflowsOption = jobOptions: lib.mkOption {
    type = lib.types.submodule {
      options = lib.mapAttrs
        (_: workflow: lib.mkOption {
          type = lib.types.submodule {
            options =
              # local actions override global actions
              let actions = lib.recursiveUpdate config.actions (workflow.actions or { }); in
              {
                actions = lib.mkOption {
                  type = lib.types.submodule {
                    options = mkActionsOptions actions;
                  };
                  default = { };
                };

                path = lib.mkOption {
                  type = lib.types.nullOr lib.types.str;
                  default = null;
                };

                jobs = lib.mkOption {
                  type = lib.types.submodule
                    {
                      options =
                        lib.mapAttrs
                          (_: job:
                            let
                              uses = lib.pipe (job.steps or [ ]) [
                                (builtins.catAttrs "uses")
                                (builtins.filter (x: x?uses && builtins.isAttrs uses))
                                (lib.foldl' lib.recursiveUpdate { })
                              ];
                              actions' = lib.recursiveUpdate actions uses;
                            in
                            lib.mkOption {
                              type = lib.types.submodule {
                                options = attrsOrFunc jobOptions actions';
                              };
                              default = { };
                            }
                          )
                          (workflow.jobs or { });
                    }
                  //
                  {
                    # there should be at least one job
                    check = x: builtins.isAttrs x && builtins.length (builtins.attrValues x) > 0;
                  };
                  default = { };
                };
              };
          };
          default = { };
        })
        configuration.workflows;
    };
    default = { };
  };

  inherit (import ./utils.nix { inherit lib; }) convertUses;

  actionsOption = lib.mkOption {
    type = lib.types.submodule { options = mkActionsOptions configuration.actions; };
    default = { };
  };
in
{
  options = {
    actions = actionsOption;
    workflows = workflowsOption jobOptions;

    accessible = lib.mkOption {
      type = lib.types.submodule {
        options = {
          actions = actionsOption;
          workflows = workflowsOption accessible.jobOptions;
        };
      };
    };
  };

  config = {
    workflows = configuration.workflows;

    accessible = {
      actions = config.actions;

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
                          (map convertUses)
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
  };
}
