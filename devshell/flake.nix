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
          inherit (drv-tools.functions.${system}) framedNewlines;
          devshell = let devshell_ = ((pkgs.extend my-devshell.overlay).devshell); in
            devshell_ // {
              mkShell = configuration: devshell_.mkShell (
                configuration // {
                  packages = [ desc ] ++ (pkgs.lib.lists.flatten configuration.packages);
                  commands = (
                    builtins.map
                      (c:
                        {
                          category = "standalone executables";
                          help = "listed in `packages` of this devshell";
                          command = ''
                            printf "${framedNewlines ''
                              This is a dummy command just to let help text for this entry
                              to be present in this devshell's message
                              ''}"
                          '';
                        } // c // {
                          # append a space to have no name clashes with original executables
                          name = c.name + (if builtins.hasAttr "command" c then "" else " ");
                        })
                      configuration.commands
                  ) ++ [
                    {
                      name = "exit ";
                      category = "general commands";
                      help = "exit this devshell";
                      command = "exit";
                    }
                  ];
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
                name = "awk";
              }
              {
                name = "hello";
              }
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
