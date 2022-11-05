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
    nix-vscode-marketplace_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nix-vscode-marketplace;
    nix-vscode-marketplace.follows = "nix-vscode-marketplace_/nix-vscode-marketplace";
    python-tools.url = github:br4ch1st0chr0n3/flakes?dir=language-tools/python;
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , flake-tools
    , drv-tools
    , my-codium
    , formatter
    , python-tools
    , nix-vscode-marketplace
    , ...
    }: flake-utils.lib.eachDefaultSystem
      (system:
      let
        inherit (my-codium.configs.${system}) extensions;
        inherit (my-codium.functions.${system}) mkCodium;
        inherit (drv-tools.functions.${system}) mkShellApp;
        inherit (python-tools.functions.${system}) createVenvs;
        inherit (python-tools.snippets.${system}) activateVenv;
        inherit (nix-vscode-marketplace.packages.${system}) vscode open-vsx;
        pkgs = nixpkgs.legacyPackages.${system};

        codium = mkCodium {
          extensions = extensions // { add = { inherit (vscode.mtxr) sqltools; }; };
          runtimeDependencies = [
            (
              builtins.attrValues
                {
                  inherit (pkgs)
                    docker poetry direnv rnix-lsp
                    nixpkgs-fmt fish mysql;
                }
            )
          ];
        };
      in
      {
        devShells.default = pkgs.mkShell {
          shellHook = activateVenv;
        };
        packages = {
          default = codium;
          createVenvs = createVenvs [ "." ];
        };
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
