{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    devshell_.url = "github:deemp/flakes?dir=source-flake/devshell";
    devshell.follows = "devshell_/devshell";
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};

          # frame a text with newlines
          framedNewlines = framed_ "\n\n" "\n\n";
          framed_ = pref: suff: txt: ''${pref}${txt}${suff}'';

          devshell = let devshell_ = ((pkgs.extend inputs.devshell.overlay).devshell); in
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

          inherit (devshell) mkShell;

          # Case: we have several scripts available in `packages`
          # And we'd like to present them in a `devShell`
          mkRunCommands = category: drvs@{ ... }:
            pkgs.lib.attrsets.mapAttrsToList
              (
                name: value: {
                  name = "nix run .#${name}";
                  inherit category;
                  help = value.meta.description or "dummy description";
                }
              )
              drvs;

          # Case: we have several programs available in a `devShell`
          # And we'd like to present them in that `devShell`
          mkCommands = category: drvs:
            map
              (
                x: {
                  name = x.pname or x.name;
                  help = x.meta.description or "dummy description";
                  inherit category;
                }
              )
              drvs;

          packages = {
            awk = pkgs.gawk;
            myScript = pkgs.writeScript ''printf "hi from my script!"'';
            testHello = pkgs.hello;
          };

          devShells.default = mkShell {
            packages = [ pkgs.gawk pkgs.hello ];
            bash = {
              extra = ''
                printf "Hello, World!\n"
              '';
            };
            commands =
              mkCommands "pkgs" [ pkgs.gawk pkgs.hello ]
              ++
              mkRunCommands "run" { inherit (packages) awk testHello; }
              ++
              [
                {
                  name = "awk, hello";
                }
                {
                  name = "run-hello";
                  category = "scripts";
                  help = "commands having the same category";
                  command = "${pkgs.hello}/bin/hello";
                }
                {
                  name = "run-awk";
                  category = "scripts";
                  help = "commands having the same category";
                  command = "${pkgs.gawk}/bin/awk";
                }
              ]
            ;
          };
        in
        {
          inherit devShells packages devshell;
          functions = { inherit mkCommands mkRunCommands mkShell; };
        });
}
