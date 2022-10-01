{
  inputs = {
    my-inputs.url = path:./inputs;
    flake-utils.follows = "my-inputs/flake-utils";
    nixpkgs.follows = "my-inputs/nixpkgs";
    my-codium.follows = "my-inputs/my-codium";
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
        inherit (mkFlakesUtils [ "source-flake" "codium" "json2md" "inputs" "." ])
          flakesUpdate
          flakesPushToCachix
          flakesUpdateAndPushToCachix
          flakesFormat
          ;
        codium = mkCodium { 
          extensions = { inherit (extensions) nix misc github; };
          runtimeDependencies = toList { inherit (shellTools) nix; };
        };
      in
      {
        devShells = mkDevShellsWithDefault
          {
            buildInputs = [ codium flakesUpdateAndPushToCachix flakesUpdate flakesPushToCachix flakesFormat ];
          }
          {
            enter = { };
          };
        packages.default = flakesUpdateAndPushToCachix;
      }) // { inherit (my-inputs) formatter; };
}
