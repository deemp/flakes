{
  inputs = {
    # my-inputs.url = path:./inputs;
    my-inputs.url = github:br4ch1st0chr0n3/flakes?dir=inputs;
    flake-utils.follows = "my-inputs/flake-utils";
    nixpkgs.follows = "my-inputs/nixpkgs";
    # my-codium.url = path:./codium;
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
          mkShellApp
          flakesToggleRelativePaths
          pushToGithub
          ;
        pkgs = nixpkgs.legacyPackages.${system};
        flakesUtils = (mkFlakesUtils [ "source-flake" "codium" "json2md" "inputs" "." ]);

        toggleRelativePaths_ =
          let
            sourceFlake = "source-flake";
            myCodium = "my-codium";
            json2md = "json2md";
            myInputs = "my-inputs";
            toggleConfig = [
              { "codium" = [ sourceFlake ]; }
              { "json2md" = [ sourceFlake ]; }
              { "inputs" = [ sourceFlake myCodium json2md ]; }
              { "." = [ myInputs myCodium ]; }
            ];
          in
          flakesToggleRelativePaths toggleConfig flakesUtils.flakesUpdate;

        pushToGithub_ = pushToGithub toggleRelativePaths_ flakesUtils.flakesUpdate;

        codium = mkCodium {
          extensions = { inherit (extensions) nix misc github; };
          runtimeDependencies = [
            (toList { inherit (shellTools) nix docker; })
            toggleRelativePaths_
            (builtins.attrValues flakesUtils)
            pkgs.pre-commit
            pushToGithub_
          ];
        };

      in
      {
        devShells = mkDevShellsWithDefault
          {
            buildInputs = [ codium (builtins.attrValues flakesUtils) toggleRelativePaths_ pushToGithub_ ];
          }
          {
            enter = { };
          };
        packages.default = flakesUtils.flakesPushToCachix;
      }) // { inherit (my-inputs) formatter; };
}
