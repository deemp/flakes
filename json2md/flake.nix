{
  inputs = {
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;
    gitignore_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/gitignore;
    dream2nix_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/dream2nix;
    flake-utils.follows = "flake-utils_/flake-utils";
    gitignore.follows = "gitignore_/gitignore";
    dream2nix.follows = "dream2nix_/dream2nix";
  };
  outputs =
    { self
    , flake-utils
    , gitignore
    , dream2nix
    , ...
    }:
    (
      dream2nix.lib.makeFlakeOutputs
        {
          systems = flake-utils.lib.defaultSystems;
          config.projectRoot = ./.;
          source = gitignore.lib.gitignoreSource ./.;
          settings = [
            {
              subsystemInfo.nodejs = 16;
            }
          ];
        }
    )
  ;
}
