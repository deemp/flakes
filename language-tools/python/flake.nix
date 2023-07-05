{
  inputs.flakes.url = "github:deemp/flakes";

  outputs = inputsTop:
    let
      inputs_ =
        let flakes = inputsTop.flakes.flakes; in
        {
          inherit (flakes.source-flake) flake-utils nixpkgs;
          inherit (flakes) drv-tools;
        };

      outputs = outputs_ { } // {
        outputs = outputs_;
        inputs = inputs_;
      };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
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
        );
    in
    outputs;
}
