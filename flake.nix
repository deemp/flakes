{
  inputs = {
    my-inputs.url = path:./inputs;
    flake-utils.follows = "my-inputs/flake-utils";
    my-codium.follows = "my-inputs/my-codium";
    nixpkgs.follows = "my-inputs/nixpkgs";
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
          flakesUpdateAndPushToCachix
          flakesUpdate
          flakesPushToCachix
          ;
        pkgs = nixpkgs.legacyPackages.${system};
        dirs = [ "source-flake" "codium" "json2md" "inputs" "." ];
        rootDir = ./.;
        flakesUpdate_ = flakesUpdate rootDir dirs;
        flakesPushToCachix_ = flakesPushToCachix rootDir dirs;
        update = flakesUpdateAndPushToCachix rootDir dirs;
        codium = mkCodium { extensions = { inherit (extensions) nix; }; };
      in
      {
        devShells = mkDevShellsWithDefault
          {
            buildInputs = [ codium update flakesUpdate_ flakesPushToCachix_ ];
          }
          {
            update = {
              buildInputs = [ update flakesUpdate_ flakesPushToCachix_ ];
              shellHook = ''
                ${update.name}
                nix fmt **/*.nix
              '';
            };
          };
        packages.default = update;
      }) // { inherit (my-inputs) formatter; };
}
