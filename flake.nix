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
      inherit (my-codium.tools.${system}) writeShellApp;
      dirs = [ "source" "codium" "json2md" "inputs" "." ];
    in
    {
      packages.default = writeShellApp {
        name = "default";
        text = builtins.concatStringsSep "\n"
          (builtins.map
            (dir: ''
              (cd ${dir} && nix flake update)
            '')
            dirs);
      };
    });
}
