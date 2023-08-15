{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs: inputs.flakes.makeFlake {
    inputs = { inherit (inputs.flakes.all) nixpkgs drv-tools; };
    perSystem = { inputs, system }:
      let
        pkgs = inputs.nixpkgs.outputs.legacyPackages.${system};
        inherit (inputs.drv-tools.outputs.lib.${system})
          withMan mkShellApp mkShellApps
          getExe framedBrackets singletonIf
          runInEachDir genAttrsId subDirectories
          concatStringsNewline
          ;
        inherit (pkgs.lib.lists) flatten;
        inherit (pkgs.lib.strings) concatMapStringsSep;

        # all flake tools together
        mkFlakesTools =
          { dirs ? [ ], subDirs ? [ ], root }:
          let args.dirs = map (x: "${root}/${x}") (flatten (dirs ++ builtins.map (subDirectories root) subDirs)); in
          {
            save = pkgs.writeText "txt" (
              concatMapStringsSep "\n" (x: x) (__attrValues (import (__head args.dirs)).devShells.${system})
            );
          };

        testFlakesTools = mkFlakesTools { dirs = [ "." ]; root = ./.; };
        devShells.default = pkgs.mkShell {
          buildInputs = [ pkgs.hello ];
        };
      in
      {
        packages = testFlakesTools;
        inherit devShells;
      };
  };
}
