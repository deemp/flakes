{
  inputs = {
    nixpkgs_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs;
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;
    drv-tools.url = github:br4ch1st0chr0n3/flakes?dir=drv-tools;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      activateVenv = ''
        ${builtins.readFile ./scripts/activate.sh}
        set +e
      '';
      inherit (drv-tools.functions.${system}) runInEachDir;
      createVenvs = dirs: runInEachDir
        {
          name = "create-venvs";
          message = "creating environment in";
          inherit dirs;
          command = ''
            ${activateVenv}
            poetry install --no-root
          '';
          runtimeInputs = [ pkgs.poetry ];
          longDescription = ''
            Create Python `.venv`s via `poetry`.
          '';
        };
    in
    {
      snippets = {
        inherit
          activateVenv
          createVenvs;
      };
    }
    );
}
