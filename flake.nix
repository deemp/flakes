{
  inputs = {
    nixpkgs_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    flake-tools.url = "github:br4ch1st0chr0n3/flakes?dir=flake-tools";
    drv-tools.url = "github:br4ch1st0chr0n3/flakes?dir=drv-tools";
    formatter.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/formatter";
    my-codium.url = "github:br4ch1st0chr0n3/flakes?dir=codium";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , flake-tools
    , drv-tools
    , my-codium
    , formatter
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
          mkDevShellsWithDefault
          readDirectories
          mkBin
          ;
        inherit (flake-tools.functions.${system})
          mkFlakesUtils
          ;
        pkgs = nixpkgs.legacyPackages.${system};
        devshell = my-codium.devshell.${system};

        flakesUtils = (mkFlakesUtils (
          let f = dir: (builtins.map (x: "${dir}/${x}") (readDirectories ./${dir})); in
          [
            (f "source-flake")
            (f "language-tools")
            [
              "drv-tools"
              "flake-tools"
              "env2json"
              "codium"
              "codium/template"
              "json2md"
              "devshell"
              "."
            ]
          ]
        ));

        codium = mkCodium {
          extensions = { inherit (extensions) nix misc github fish; };
        };
        writeSettings = writeSettingsJSON settingsNix;
      in
      {
        devShells.default = devshell.mkShell {
          packages = builtins.attrValues flakesUtils;
          commands = [
            {
              name = "runCodium";
              category = "ide";
              help = "start VSCodium in current directory";
              command = "${mkBin codium} .";
            }
            {
              name = "writeSettings";
              category = "ide";
              help = "write settings.json for VSCodium";
              command = "${mkBin writeSettings}";
            }
          ];
        };

        packages = {
          pushToCachix = flakesUtils.flakesPushToCachix;
          updateLocks = flakesUtils.flakesUpdate;
          format = flakesUtils.flakesFormat;
          default = codium;
          inherit writeSettings;
        };
        inherit (formatter) formatter;
      }) // {
      templates = {
        codium = {
          path = ./codium/template;
          description = "codium with extensions and binaries";
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
