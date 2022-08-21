{
  inputs = {
    backend.url = "path:./backend";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, backend, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      {
        devShells.default = backend.devShells.${system}.default;
      });
}
