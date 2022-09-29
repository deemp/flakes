{
  inputs = {
    my-inputs.url = path:./inputs;
    flake-utils.follows = "my-inputs/flake-utils";
    my-codium.follows = "my-inputs/my-codium";
  };
  outputs =
    { self
    , my-inputs
    , flake-utils
    , my-codium
    }: flake-utils.lib.eachDefaultSystem (system:
    let
      inherit (my-codium.tools.${system})
        runInEachDir
        pushAllToCachix
        codium
        extensions
        toList
        shellTools
        mkCodium
        mkDevShellsWithEntryPoint
        pushDevShellsToCachix
        pushPackagesToCachix
        ;
      dirs = [ "source" "codium" "json2md" "inputs" "." ];
      updateFlakes = runInEachDir { inherit dirs; name = "update-flakes"; command = "nix flake update"; root = ./.; };
      pushToCachix = runInEachDir { inherit dirs; name = "push-to-cachix"; command = "${pushAllToCachix.name}"; runtimeInputs = [ pushAllToCachix ]; root = ./.; };
    in
    {
      devShells = mkDevShellsWithEntryPoint "update"
        {
          runtimeInputs = [ updateFlakes pushToCachix ];
          text = ''
            ${updateFlakes.name}
            ${pushToCachix.name}
          '';
        }
        {
          buildInputs =
            let
              codium = mkCodium { inherit (extensions) nix markdown github misc; };
              shellTools_ = toList { inherit (shellTools) nix; };
            in
            [ codium shellTools_ updateFlakes pushToCachix];
        }
        { };
      packages.default = updateFlakes;
    });
}
