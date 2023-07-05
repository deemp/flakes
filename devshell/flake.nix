{
  inputs.flakes.url = "github:deemp/flakes";

  outputs =
    inputs:
    let
      inputs_ =
        let flakes = inputs.flakes.flakes; in
        {
          inherit (flakes.source-flake) flake-utils nixpkgs devshell;
          inherit (flakes) drv-tools;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem
          (system:
          let
            pkgs = inputs.nixpkgs.legacyPackages.${system};

            # frame a text with newlines
            inherit (inputs.drv-tools.lib.${system}) framedNewlines framed_;

            devshell = let devshell_ = ((pkgs.extend inputs.devshell.overlays.default).devshell); in
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

            # Case: we have several scripts available in `packages` of a flake
            # And we'd like to present them in a `devShell`
            mkRunCommandsDir = dir: category: drvs@{ ... }:
              pkgs.lib.attrsets.mapAttrsToList
                (
                  name: value: {
                    name = "nix run ${dir}#${name}";
                    inherit category;
                    help = value.meta.description or "dummy description";
                  }
                )
                drvs;

            # Case: we have several scripts available in `packages` of the current flake
            # And we'd like to present them in a `devShell`
            mkRunCommands = mkRunCommandsDir ".";

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
                    # test name collision prevention
                    name = "hello";
                    package = pkgs.hello;
                  }
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
            lib = {
              inherit
                mkCommands
                mkRunCommands
                mkRunCommandsDir
                mkShell
                ;
            };
          });
    in
    outputs;
}
