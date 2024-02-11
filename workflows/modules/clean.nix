{ lib
, config
, utils
, ...
}:
{
  options = {
    clean = lib.mkOption {
      type = lib.types.anything;
      default = { };
    };
  };

  config = {
    clean = utils.cleanWorkflows (
      utils.resolveWorkflows {
        inherit config utils;
      }
    );
  };
}
