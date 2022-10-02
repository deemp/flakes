{
  inputs = {
    my-inputs.url = github:br4ch1st0chr0n3/flakes?dir=inputs;
    flake-utils.follows = "my-inputs/flake-utils";
    nixpkgs.follows = "my-inputs/nixpkgs";
    my-codium.url = path:./codium;
    # my-codium.url = github:br4ch1st0chr0n3/flakes?dir=codium;
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

        # TODO
        toggleRelativePaths_ =`
          let
            myCodium = "my-codium";
            json2md = "json2md";
            myInputs = "my-inputs";
            env2json = "env2json";
            toggleConfig = [
              { "inputs" = [ myCodium json2md env2json ]; }
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
