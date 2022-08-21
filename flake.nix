{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/cd8bbdd4fdb15f7758fd3c8df531e9d42bee239d";
    flake-utils.url = "github:numtide/flake-utils";
    backend = {
      url = "path:./backend";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    frontend = {
      url = "path:./frontend";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, backend, frontend, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        backShell = backend.devShells.${system}.default;
        frontShell = frontend.devShells.${system}.default;
      in
      {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = backShell.buildInputs ++ frontShell.buildInputs;
            inherit (backShell) shellHook LD_LIBRARY_PATH;
          };
          back = backShell;
          front = frontShell;
        };
      });
}
