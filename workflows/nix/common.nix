{ lib }:
rec
{
  options = rec {
    str = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
    };

    path = str;

    name = str;

    id = str;
    
    uses = str;

    with' = lib.mkOption {
      type = lib.types.attrsOf (lib.types.nullOr lib.types.str);
      default = { };
    };

    action = lib.types.submodule {
      options = {
        inherit name with';
      };
    };

    actions = lib.mkOption {
      type = lib.types.attrsOf action;
      default = { };
    };

    step = {
      inherit id name uses with';
    };

    job = {
      inherit name;
    };
  };

  mkWorkflowsOption = steps: lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options = {
        inherit (options) path actions;
        jobs = lib.mkOption {
          type = lib.types.attrsOf (lib.types.submodule {
            options = {
              inherit (options) name;
              steps = lib.mkOption steps;
            };
          });
          default = { };
        };
      };
    });
    default = { };
  };
}
