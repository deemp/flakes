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

        flakesUtils = (mkFlakesUtils [ "source-flake" "codium" "env2json" "json2md" "inputs" "." ]);

        # toggleRelativePaths_ =
        #   let
        #     myCodium = "my-codium";
        #     myInputs = "my-inputs";
        #     toggleConfig = [
        #       { "." = [ myInputs myCodium ]; }
        #     ];
        #   in
        #   flakesToggleRelativePaths toggleConfig flakesUtils.flakesUpdate;

        # pushToGithub_ = pushToGithub toggleRelativePaths_ flakesUtils.flakesUpdate;

        codium = mkCodium {
          extensions = { inherit (extensions) nix misc github; };
          runtimeDependencies = [
            (toList { inherit (shellTools) nix docker; })
            # pushToGithub_
            # toggleRelativePaths_
            (builtins.attrValues flakesUtils)
          ];
        };

      in
      {
        devShells = mkDevShellsWithDefault
          {
            buildInputs = [
              codium
              (builtins.attrValues flakesUtils)
              # toggleRelativePaths_
              # pushToGithub_
            ];
          }
          {
            enter = { };
          };
        packages = {
          pushToCachix = flakesUtils.flakesPushToCachix;
          updateLocks = flakesUtils.flakesUpdate;
          format = flakesUtils.flakesFormat;
        };
      }) // { inherit (my-inputs) formatter; };
}
