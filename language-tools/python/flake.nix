{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs: inputs.flakes.makeFlake {
    inputs = { inherit (inputs.flakes.all) nixpkgs drv-tools; };
    perSystem = { inputs, system }:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        activateVenv = ''
          ${builtins.readFile ./scripts/activate.sh}
          set +e
        '';
        inherit (inputs.drv-tools.lib.${system}) runInEachDir;
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
            description = "Create Python `.venv`s via `poetry` in given directories";
          };
        testCreateVenvs = createVenvs [ "." ];
      in
      {
        lib = {
          inherit
            activateVenv
            createVenvs
            ;
        };
        devShells.default = pkgs.mkShell {
          buildInputs = [ testCreateVenvs ];
        };
      }
    ;
  };
}
