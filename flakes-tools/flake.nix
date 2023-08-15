{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs: inputs.flakes.makeFlake {
    inputs = { inherit (inputs.flakes.all) nixpkgs drv-tools; };
    perSystem = { inputs, system }:
      let
        pkgs = inputs.nixpkgs.outputs.legacyPackages.${system};
        inherit (inputs.drv-tools.outputs.lib.${system}) mkShellApp mkShellApps subDirectories;
        inherit (pkgs.lib.lists) flatten;
        inherit (pkgs.lib.strings) concatMapStringsSep;

        # should be flake dirs
        flakesSave = { dirs ? [ ], doPushToCachix ? false }:
          let
            description = "Save and conditionally push to `Cachix` inputs and outputs of flakes in specified directories relative to `CWD`.";
            save = pkgs.writeText "txt${if doPushToCachix then "-cachix" else ""}" (
              concatMapStringsSep "\n" (x: x) (__attrValues (import (__head dirs)).devShells.${system})
            );
          in
          save;

        # all flake tools together
        mkFlakesTools =
          { dirs ? [ ], subDirs ? [ ], root, flakesSaveArgs ? { } }:
          let
            args = {
              dirs = map (x: "${root}/${x}") (flatten (dirs ++ builtins.map (subDirectories root) subDirs));
            };
          in
          {
            pushToCachix = flakesSave (args // flakesSaveArgs // { doPushToCachix = true; });
            saveFlakes = flakesSave (args // flakesSaveArgs);
          };

        packages = mkFlakesTools { dirs = [ "." ]; root = ./.; };
        devShells.default = pkgs.mkShell {
          buildInputs = builtins.filter (pkgs.lib.attrsets.isDerivation) (with packages; [
            pushToCachix
          ]);
        };
      in
      {
        inherit packages devShells;
      };
  };
}
