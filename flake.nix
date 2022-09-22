{
  inputs = {
    my-inputs.url = "github:br4ch1st0chr0n3/flakes?dir=inputs";
    nixpkgs.follows = "my-inputs/nixpkgs";
    flake-utils.follows = "my-inputs/flake-utils";
    my-codium.follows = "my-inputs/my-codium";
    flake-compat.follows = "my-inputs/flake-compat";
    hls.follows = "my-inputs/haskell-language-server";
    gitignore.follows = "my-inputs/gitignore";
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , my-codium
    , flake-compat
    , hls
    , gitignore
    , my-inputs
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      ghcVersion = "902";
      inherit (my-codium.tools.${system})
        writeSettingsJson
        settingsNix
        codium
        toList
        shellTools
        toolsGHC
        ;
      inherit (toolsGHC ghcVersion) hls stack callCabal staticExecutable;

      manager =
        let
          manager-exe = staticExecutable "manager" ./manager;
        in
        pkgs.symlinkJoin {
          name = "manager";
          paths = [ manager-exe ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/manager \
              --set PATH ${
                pkgs.lib.makeBinPath [
                  pkgs.hpack
                ]
              }
          '';
        };

      writeSettings = writeSettingsJson
        {
          inherit (settingsNix) haskell todo-tree files editor gitlens git nix-ide workbench;
        };

      tools = (
        toList {
          inherit (shellTools) nix haskell;
        }) ++ [ stack hls manager ];

      codiumWithSettings = pkgs.mkShell {
        buildInputs = [ writeSettings codium ];
        shellHook = ''
          write-settings-json
          codium .
        '';
      };
    in
    {
      devShells =
        {
          default = pkgs.mkShell {
            name = "dev-tools";
            buildInputs = tools;
            shellHook = ''
              source <(manager --bash-completion-script `which manager`)
            '';
          };

          codium = codiumWithSettings;
        };

      stack-shell = { ghcVersion }:

        pkgs.haskell.lib.buildStackProject {
          name = "nix-managed-stack-shell";

          ghc = pkgs.haskell.compiler.${ghcVersion};

          buildInputs = [
            pkgs.zlib
          ];
        };
    });

  nixConfig = {
    extra-substituters = [
      https://haskell-language-server.cachix.org
      https://nix-community.cachix.org
      https://hydra.iohk.io
      https://br4ch1st0chr0n3.cachix.org
    ];
    extra-trusted-public-keys = [
      haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8=
      nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
      hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
      br4ch1st0chr0n3.cachix.org-1:o1FA93L5vL4LWi+jk2ECFk1L1rDlMoTH21R1FHtSKaU=
    ];
  };
}
