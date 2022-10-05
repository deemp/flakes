{
  inputs = {
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;
    dream2nix_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/dream2nix;
    dream2nix.follows = "dream2nix_/dream2nix";
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs =
    { self
    , flake-utils
    , dream2nix
    , ...
    }:
    (
      dream2nix.lib.makeFlakeOutputs
        {
          systems = flake-utils.lib.defaultSystems;
          config.projectRoot = ./.;
          source = ./.;
          settings = [
            {
              subsystemInfo.nodejs = 16;
            }
          ];
        }
    )
  ;
}
# 