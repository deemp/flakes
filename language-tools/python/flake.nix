{
  inputs = {
    flakes = {
      url = "github:deemp/flakes";
      flake = false;
    };
  };
  outputs =
    inputsTop:
    let
      inputs_ = {
        inherit (import inputsTop.flake) nixpkgs flake-utils drv-tools;
      };

      outputs = flake { } // {
        inherit flake;
        inputs = inputs_;
      };

      flake =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          activateVenv = ''
            ${builtins.readFile ./scripts/activate.sh}
            set +e
          '';
          inherit (drv-tools.lib.${system}) runInEachDir;
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
        );
    in
    outputs;
}
