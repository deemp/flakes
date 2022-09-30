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
        fullUpdate
        updateFlakes
        pushToCachix
        ;
      dirs = [ "source" "codium" "json2md" "inputs" "." ];
      rootDir = ./.;
      updateFlakes_ = updateFlakes rootDir dirs;
      pushToCachix_ = pushToCachix rootDir dirs;
      update = fullUpdate rootDir dirs;
    in
    {
      devShells = mkDevShellsWithEntryPoint
        {
          name = "update";
          runtimeInputs = [ update ];
          text = ''${update.name}'';
        }
        {
          buildInputs =
            let
              codium = mkCodium { inherit (extensions) nix markdown github misc; };
              shellTools_ = toList { inherit (shellTools) nix; };
            in
            [ codium shellTools_ updateFlakes_ pushToCachix_ ];
        }
        { };
      packages.default = updateFlakes_;
    });
}
