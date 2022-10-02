{
  inputs = {
    source-flake.url = github:br4ch1st0chr0n3/flakes?dir=source-flake;
    flake-utils.follows = "source-flake/flake-utils";
    gitignore.follows = "source-flake/gitignore";
    dream2nix.follows = "source-flake/dream2nix";
  };
  outputs =
    { self
    , source-flake
    , flake-utils
    , gitignore
    , dream2nix
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
    ) // { inherit (source-flake) formatter; }
  ;
}
