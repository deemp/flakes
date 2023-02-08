{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    my-devshell_.url = "github:deemp/flakes?dir=source-flake/devshell";
    my-devshell.follows = "my-devshell_/devshell";
  };
  outputs = { self, nixpkgs, flake-utils, my-devshell, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          # frame a text with newlines
          framedNewlines = framed_ "\n\n" "\n\n";
          framed_ = pref: suff: txt: ''${pref}${txt}${suff}'';

          devshell = let devshell_ = ((pkgs.extend my-devshell.overlay).devshell); in
            devshell_ // {
              mkShell = configuration: devshell_.mkShell (
                configuration // {
                  # bashInteractive - for VSCodium
                  packages = pkgs.lib.lists.flatten ((configuration.packages or [ ]) ++ [ pkgs.bashInteractive ]);
                  commands = (
                    builtins.map
                      (c:
                        {
                          category = "programs";
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
                      (
                        (configuration.commands or [ ])
                        ++ [
                          {
                            name = "exit";
                            category = "general commands";
                            help = "exit this devshell";
                          }
                          {
                            name = "man";
                            category = "general commands";
                            help = "get more info about a command. Example: 'man man'";
                          }
                        ]
                      )
                  );
                }
              );
            };

          mkCommands = category: drvs: map
            (
              x: {
                name = x.pname;
                category = category;
                help = x.meta.description;
              }
            )
            drvs;
          mkShell = devshell.mkShell;
        in
        {
          inherit devshell;
          functions = {
            inherit mkCommands mkShell;
          };
          devShells.default = mkShell {
            packages = [ pkgs.gawk pkgs.hello ];
            bash = {
              extra = ''
                printf "Hello, World!\n"
              '';
            };
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
                help = "commands having the same category";
                command = "hello";
              }
              {
                name = "run-awk-help";
                category = "scripts";
                help = "commands having the same category";
                command = "awk --help";
              }
            ];
          };
        });
}
