{
  inputs = {
    nixpkgs_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    flakes-tools.url = "github:br4ch1st0chr0n3/flakes?dir=flakes-tools";
    drv-tools.url = "github:br4ch1st0chr0n3/flakes?dir=drv-tools";
    formatter.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/formatter";
    my-codium.url = "github:br4ch1st0chr0n3/flakes?dir=codium";
    my-devshell.url = "github:br4ch1st0chr0n3/flakes?dir=devshell";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , flakes-tools
    , drv-tools
    , my-codium
    , formatter
    , my-devshell
    , ...
    }: flake-utils.lib.eachDefaultSystem
      (system:
      let
        inherit (my-codium.configs.${system})
          extensions
          ;
        inherit (my-codium.functions.${system})
          mkCodium
          writeSettingsJSON
          ;
        inherit (my-codium.configs.${system})
          settingsNix
          ;
        inherit (drv-tools.functions.${system})
          readDirectories
          ;
        inherit (flakes-tools.functions.${system})
          mkFlakesTools
          ;
        pkgs = nixpkgs.legacyPackages.${system};
        devshell = my-devshell.devshell.${system};

        flakesTools = (mkFlakesTools (
          let f = dir: (builtins.map (x: "${dir}/${x}") (readDirectories ./${dir})); in
          [
            (f "source-flake")
            (f "language-tools")
            (f "templates/codium")
            [
              "drv-tools"
              "flakes-tools"
              "env2json"
              "codium"
              "json2md"
              "devshell"
              "manager/nix-utils"
              "manager"
              "."
            ]
          ]
        ));

        codium = mkCodium {
          extensions = { inherit (extensions) nix misc github markdown; };
        };
        writeSettings = writeSettingsJSON settingsNix;
      in
      {
        devShells.default = devshell.mkShell {
          packages = (builtins.attrValues flakesTools) ++ [ codium writeSettings ];
          commands = [
            {
              name = "codium";
              category = "ide";
              help = "VSCodium";
            }
            {
              name = "${writeSettings.name}";
              category = "ide";
              help = "write settings.json for VSCodium";
            }
          ];
        };

        packages = {
          pushToCachix = flakesTools.pushToCachix;
          updateLocks = flakesTools.update;
          format = flakesTools.format;
        };
        inherit (formatter) formatter;
      }) // {
      templates = {
        codium-generic = {
          path = ./templates/codium/generic;
          description = "VSCodium with extensions and executables";
        };
        codium-haskell = {
          path = ./templates/codium/haskell;
          description = "VSCodium with extensions and executables for Haskell";
        };
      };
    };

  nixConfig = {
    extra-trusted-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://hydra.iohk.io"
      "https://br4ch1st0chr0n3.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "br4ch1st0chr0n3.cachix.org-1:o1FA93L5vL4LWi+jk2ECFk1L1rDlMoTH21R1FHtSKaU="
    ];
  };
}
