{
  inputs = {
    nixpkgs_.url = github:deemp/flakes?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:deemp/flakes?dir=source-flake/flake-utils;
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs = { nixpkgs, flake-utils, self, ... }: flake-utils.lib.eachDefaultSystem (system: {
    formatter = nixpkgs.legacyPackages.${system}.nixpkgs-fmt;
  });
}
