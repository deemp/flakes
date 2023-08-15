{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs: inputs.flakes.makeFlake {
    inputs = {
      inherit (inputs.flakes.all) nixpkgs drv-tools;
      devshell = inputs.flakes.all.devshell-source;
    };
    perSystem = { inputs, system }:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        inherit (inputs.drv-tools.lib.${system}) framedNewlines framed_;
        inherit (pkgs.lib.lists) flatten imap0;
        inherit (pkgs.lib) setPrio;
        inherit (pkgs.lib.attrsets) mapAttrsToList;

        setPrios = xs: imap0 setPrio (flatten xs);

        devshell = let devshell_ = ((pkgs.extend inputs.devshell.overlays.default).devshell); in
          devshell_ // {
            mkShell = configuration: devshell_.mkShell (
              configuration // {
                # bashInteractive - for VSCodium
                packages = setPrios ((configuration.packages or [ ]) ++ [ pkgs.bashInteractive ]);
                commands =
                  (configuration.commands or [ ])
                  ++ [
                    {
                      name = "exit";
                      category = "general commands";
                      command = "exit $@";
                      help = "exit this devshell";
                    }
                    {
                      name = "man";
                      category = "general commands";
                      command = "man $@";
                      help = "get more info about a command. Example: 'man man'";
                    }
                  ]
                ;
              }
            );
          };

        inherit (devshell) mkShell;

        # Case: list commands to run packages in a given directory
        mkRunCommandsDir = dir: category: drvs@{ ... }:
          mapAttrsToList
            (
              name: drv:
                {
                  name = "nix run ${dir}#${name}";
                  command = " ";
                  inherit category;
                  help = drv.meta.description or "dummy description";
                }
            )
            drvs;

        # Case: list commands to run packages in the current directory
        mkRunCommands = mkRunCommandsDir ".";

        # Case: provide packages in the same category
        mkCommands = category: drvs: map (x: { package = x; inherit category; }) (setPrios drvs);

        mkDefaultCommands = commands: map (x: { command = " "; } // x) commands;

        packages = {
          awk = pkgs.gawk;
          myScript = pkgs.writeScript ''printf "hi from my script!"'';
          testHello = pkgs.hello;
        };

        devShells.default = mkShell {
          bash = {
            extra = ''
              printf "Hello, World!\n"
            '';
          };
          commands =
            mkCommands "pkgs" [ pkgs.gawk pkgs.hello pkgs.nodejs_18 pkgs.nodejs_20 ]
            ++ mkRunCommands "run" { inherit (packages) awk testHello; }
            ++ mkDefaultCommands
              [
                {
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
            mkDefaultCommands
            mkRunCommands
            mkRunCommandsDir
            mkShell
            ;
        };
      };
  };
}
