{ system ? builtins.currentSystem
, pkgs ? (import ../.).inputs.nixpkgs.legacyPackages.${system}
, drv-tools ? (import ../drv-tools { inherit system pkgs; })
}@args:
let
  inherit (pkgs) lib;

  inherit (drv-tools.lib) writeYAML;

  eval =
    { configuration, extraSpecialArgs ? { } }:
    let
      module = lib.evalModules {
        modules = [
          ./gh-action.nix
        ];
        specialArgs = {
          inherit configuration;
        } // extraSpecialArgs;
      };
    in
    {
      inherit (module) config options;
    };

  # cleanConfig = config:
  #   let
  #     clean = lib.filterAttrsRecursive (_: value: value != null) config;
  #     cleanJobs =
  #       lib.mapAttrs
  #         (
  #           _: value:
  #             value
  #             // (
  #               lib.optionalAttrs (value?steps) {
  #                 steps = map (lib.filterAttrsRecursive (_: value: value != null)) value.steps;
  #               }
  #             )
  #         )
  #         (clean.jobs or { });
  #   in
  #   (builtins.removeAttrs clean [ "_" ])
  #   // (lib.optionalAttrs (clean?jobs) { jobs = cleanJobs; });

  # configClean = cleanConfig gh-action.config;

  # TODO
  # writeConfig = name: path: config: writeYAML name path (cleanConfig config);

  # TODO
  # type checking?

  # TODO access step by index

  # TODO expose actions in workflow args

  # Since we need a way to override top-level actions
  # we need a way to override top-level actions
  # on the workflow level and on the job level

  # we can use fixpoints that may hinder type checking

  # alternatively, we can use access via top-level attributes
  # workflows.workflow-1.actions
  # then, we need to set workflows.workflow-1.actions to include global actions

  # TODO
  # it's not these workflows, it's accessible workflows
  # that allow access to steps
  # hence, we need to pass a config.accessible, not use lib.fix

  # TODO
  # priority by attrnames
  # use a simpler name? there's nothing else to name __1-abra
  # { 1-b }
  # _1 = x: 1-BAHD-AASL-ALSS-EIOA-VBDJ-LKAS-CNKD-${x}
  # - accessible should be without such names
  #   because there, order doesn't matter
  # type errors? no such option?

  # use suffixes for priorities because suffixes aren't important
  # b, prio. 2 -> b_2
  # a_1 -> a_1_

  # TODO
  # typecheck workflows without such names?
  # make a copy of workflows with such names

  # suffixed before unsuffixed
  # in yaml

  # TODO
  # priority by structure
  # [ { a = b } { c = d; e = f; } ]

  # TODO use rec
  # no need to use {checkout}: {...}
  # because there are no transformations

  # sugar like actions = { checkout }: { ... }
  # can be replaced by `with actions;`

  # TODO enable per-job actions?

  # TODO typecheck configuration, not constructed version
  # users should provide a valid configuration

  configuration =
    { workflows, actions }:
    {
      actions = {
        checkout = {
          name = "actions/checkout@v4";
          with' = {
            repository = "abra";
            filter = "filter";
          };
        };
      };

      workflows = {
        workflow-1 = {
          path = "action.yaml";
          actions = { };
          jobs = {
            a = {
              name = "Hello";
              steps = [
                {
                  id = "1";
                  uses.checkout = {
                    with' = {
                      filter = "filter-1";
                    };
                  };
                }
                {
                  uses = "actions/cache";
                }
              ];
            };

            b = {
              name = workflows.workflow-1.jobs.a.name;
              steps = [
                {
                  name = "hello";
                  uses.checkout = {
                    with' = {
                      repository = "abra";
                      filter = null;
                    };
                  };
                }
                {
                  uses = workflows.workflow-1.jobs.a.steps."2".uses;
                }
              ];
            };
          };
        };

        workflow-2 = {
          path = "action-2.yaml";

          jobs = {
            a = {
              # name = workflows.workflow-1.jobs.b.name;
              name = "";
              steps = [
                {
                  id = "1";
                }
              ];
            };
          };
        };
      };
    };

  gh-action = eval { inherit configuration; };

  clean = (import ./utils.nix { inherit lib; }).clean gh-action.config.workflows;

  # can be a function
  # { config, ... }: {
  #   options = { a = mkOption { }; };
  # };

  # can be a set with `options` and `config`
  # { options = { a = mkOption { }; }; }

  # can be a set of options
  # { a = mkOption { }; }

  # TODO
  # with_ or with'
  # actions_ or actions'

  # TODO convert actions_ to options

  # actions should be an option so that it can be typechecked
  # TODO
  # maybe use an applied value in typechecking
  # like `actions args`

  # TODO unit tests
  # - golden tests for generated yamls
  # - test for configs

  # TODO
  # hidden _ord attr that defines ordering of keys
  # __ord = [ "a" "b" "c" ]

  # actions = { options, ... }: {
  #   checkout = lib.mkOption {
  #     type = lib.types.submodule {
  #       options = {
  #         name = lib.mkOption {
  #           type = lib.types.str;
  #         };
  #         with_ = lib.mkOption {
  #           type = lib.types.submodule {
  #             options = {
  #               repository = lib.mkOption {
  #                 type = lib.types.str;
  #               };
  #             };
  #           };
  #         };
  #       };
  #     };
  #   };
  # };

  # barType = lib.types.attrsOf (lib.types.submodule {
  #   options = {
  #     name = lib.mkOption {
  #       type = lib.types.str;
  #       default = "bar";
  #     };
  #   };
  # });

  # foo = { config, ... }: {
  #   options.bar = lib.mkOption {
  #     type = barType;
  #     default = { };
  #   };

  #   options._ = lib.mkOption {
  #     type = lib.types.submodule {
  #       options = {
  #         bar = lib.mkOption {
  #           type = barType;
  #         };
  #       };
  #     };
  #   };

  #   config._.bar = lib.filterAttrs  config.bar;
  #   config._.baz = config.bar;
  # };

  # foo_ = lib.evalModules {
  #   modules = [ foo ];
  # };


  # how to enable an action ?
  # any?
  # jobs.steps = [{ name = {  }; run = ""; } {uses = "name";} {uses.name;}]

  # should behave as a submodule for pre-defined attrs and as an attrset for other attributes - do we need this?
  # types.any would do
  # and people should be able to provide own options
  # uses
  # string - as usually
  # config - use options


  # there can be only one action in uses
  # like uses.name1

  # mixing of typed (pre-defined workflow attrs and action-specific?) 
  # won't happen in case of {uses = "name";}
  # because the action keeps the step structure

in
{
  options = gh-action.options;
  config = gh-action.config;
  inherit clean;
  # foo = foo_;
  # script = writeConfig "action" "tmp/action.yaml" configuration;
}
