{ lib
, config
, utils
, ...
}:
let
  clean = utils.cleanWorkflows (
    utils.resolveWorkflows {
      inherit config;
    }
  );
  
  cleanOption = lib.mkOption {
    type = lib.types.attrsOf lib.types.anything;
    default = { };
  };
  
  mkSorted = 
  lib.mapAttrsRecursive (name: x: 
    if builtins.isList x 
    then map mkSorted x 
    else 
  );
in
{
  options = {
    clean = lib.mkOption {
      type = lib.types.submodule {
        options = {
          sorted = cleanOption;
          default = cleanOption;
          unsorted = cleanOption;
        };
      };
      default = { };
    };
  };

  config = {
    clean = {
      # TODO
      sorted = clean;
      default = clean;
      unsorted = clean;
    };
  };
}
