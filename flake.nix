{
  inputs = {
    # my-inputs.url = path:./inputs;
    my-inputs.url = github:br4ch1st0chr0n3/flakes?dir=inputs;
    flake-utils.follows = "my-inputs/flake-utils";
    nixpkgs.follows = "my-inputs/nixpkgs";
    my-codium.url = github:br4ch1st0chr0n3/flakes?dir=codium;
  };
  outputs =
    { self
    , my-inputs
    , flake-utils
    , my-codium
    , nixpkgs
    }: flake-utils.lib.eachDefaultSystem
      (system:
      let
        inherit (my-codium.tools.${system})
          extensions
          toList
          shellTools
          mkCodium
          mkDevShellsWithDefault
          mkFlakesUtils
          ;
        pkgs = nixpkgs.legacyPackages.${system};
        flakesUtils = (mkFlakesUtils [ "source-flake" "codium" "json2md" "inputs" "." ]);
        codium = mkCodium {
          extensions = { inherit (extensions) nix misc github; };
          runtimeDependencies = toList { inherit (shellTools) nix; };
        };
      in
      {
        devShells = mkDevShellsWithDefault
          {
            buildInputs = [ codium (builtins.attrValues flakesUtils) ];
          }
          {
            enter = { };
          };
        packages.default = flakesUtils.flakesPushToCachix;
      }) // { inherit (my-inputs) formatter; };
}
