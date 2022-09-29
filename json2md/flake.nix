{
  inputs = {
    my-inputs.url = path:../source;
    flake-utils.follows = "my-inputs/flake-utils";
    gitignore.follows = "my-inputs/gitignore";
    dream2nix.follows = "my-inputs/dream2nix";
  };
  outputs =
    { self
    , my-inputs
    , flake-utils
    , gitignore
    , dream2nix
    }:
    dream2nix.lib.makeFlakeOutputs {
      systems = flake-utils.lib.defaultSystems;
      config.projectRoot = ./.;
      source = gitignore.lib.gitignoreSource ./.;
      settings = [
        {
          subsystemInfo.nodejs = 16;
        }
      ];
    };
}
