{
  inputs = {
    nixpkgs_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    drv-tools.url = "github:br4ch1st0chr0n3/flakes?dir=drv-tools";
    flake-utils_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    my-devshell_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/devshell";
    my-devshell.follows = "my-devshell_/devshell";
  };
  outputs = { self, nixpkgs, drv-tools, flake-utils, my-devshell, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (drv-tools.packages.${system}) desc;
          devshell = let devshell_ = ((pkgs.extend my-devshell.overlay).devshell); in
            devshell_ // {
              mkShell = configuration: devshell_.mkShell (
                configuration // {
                  packages = [ desc ] ++ configuration.packages;
                  commands = builtins.map
                    (c: {
                      category = "standalone executables";
                      help = "programs listed in `packages` of this devshell";
                      command = "echo ''";
                    } // c)
                    configuration.commands;
                }
              );
            };
        in
        {
          inherit devshell;
          devShells.default = devshell.mkShell {
            packages = [ pkgs.gawk pkgs.hello ];
            commands = [
              {
                name = "awk, hello";
              }
              {
                name = "run-hello";
                category = "scripts";
                help = "this is how to expose a group of scripts";
                command = "hello";
              }
              {
                name = "run-awk-help";
                category = "scripts";
                help = "this is how to expose a group of scripts";
                command = "awk --help";
              }
            ];
          };
        });
}
