{
  inputs = {
    nixpkgs_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs;
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;
    flake-tools.url = github:br4ch1st0chr0n3/flakes?dir=flake-tools;
    drv-tools.url = github:br4ch1st0chr0n3/flakes?dir=drv-tools;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils.follows = "flake-utils_/flake-utils";
    formatter.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/formatter;
    my-codium.url = github:br4ch1st0chr0n3/flakes?dir=codium;
    # my-codium.url = path:./codium;
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
          extensions;
        inherit (my-codium.functions.${system})
          mkCodium;
        inherit (drv-tools.functions.${system})
          toList
          mkDevShellsWithDefault
          readDirectories
          mkShellApp;
        inherit (flake-tools.functions.${system})
          flakesToggleRelativePaths
          mkFlakesUtils
          ;
        pkgs = nixpkgs.legacyPackages.${system};

        flakesUtils = (mkFlakesUtils (
          let f = dir: (builtins.map (x: "${dir}/${x}") (readDirectories ./${dir})); in
          [
            (f "source-flake")
            (f "language-tools")
            [ "drv-tools" "flake-tools" "env2json" "codium" "json2md" "." ]
          ]
        ));

        toggleRelativePaths_ =
          let
            myCodium = "my-codium";
            toggleConfig = [
              { "." = [ myCodium ]; }
            ];
          in
          flakesToggleRelativePaths toggleConfig flakesUtils.flakesUpdate;

        codium = mkCodium {
          extensions = { inherit (extensions) nix misc github fish; };
          runtimeDependencies = [
            pkgs.docker
            pkgs.rnix-lsp
            toggleRelativePaths_
            (builtins.attrValues flakesUtils)
            pkgs.inotify-tools
          ];
        };
      in
      {

        devShells = mkDevShellsWithDefault
          {
            buildInputs = [
              (builtins.attrValues flakesUtils)
              toggleRelativePaths_
              codium
            ];
          }
          {
            enter = { buildInputs = [ pkgs.gawk ]; };
          };
        packages = {
          pushToCachix = flakesUtils.flakesPushToCachix;
          updateLocks = flakesUtils.flakesUpdate;
          format = flakesUtils.flakesFormat;
          default = codium;
        };
        inherit (formatter) formatter;
      });

  nixConfig = {
    extra-trusted-substituters = [
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
