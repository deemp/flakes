{
  description = "CLI for Hosted Nix binary caches";
  inputs = rec {
    nixpkgs.url = "github:NixOS/nixpkgs/cd8bbdd4fdb15f7758fd3c8df531e9d42bee239d";
    flake-utils.url = "github:numtide/flake-utils";

    # https://discourse.nixos.org/t/recommendations-for-use-of-flakes-input-follows/17413
    # 524ef152342f37ac524e4da1e419d164316c7c8f
    # hls = {
    #   url = "github:haskell/haskell-language-server?rev=907a6e645bf03247887c63690f125d02dbcf9ed8";
    # };
  };

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org/"
      "https://cachix.cachix.org"
      # "https://haskell-language-server.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      # "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
  };


  outputs =
    { self
    , nixpkgs
    , flake-utils
    # , hls
    }:
      with flake-utils;
      lib.eachDefaultSystem (system:
      let
        # https://discourse.nixos.org/t/using-nixpkgs-legacypackages-system-vs-import/17462
        pkgs = nixpkgs.legacyPackages.${system};

        backend = (import ./backend.nix) { inherit system pkgs; };

        ghcVersion = "902";
        hPkgs = pkgs.haskell.packages."ghc${ghcVersion}"; # need to match Stackage LTS version
        # from stack.yaml resolver

        myDevTools =
          with hPkgs; [
            ghc # GHC compiler in the desired version (will be available on PATH)
            ghcid # Continous terminal Haskell compile checker
            fourmolu # Haskell formatter
            ormolu # Haskell formatter
            hlint # Haskell codestyle checker
            hoogle # Lookup Haskell documentation
            implicit-hie # auto generate LSP hie.yaml file from cabal
            haskell-language-server
            retrie # Haskell refactoring tool
          ] ++ [
            stack-wrapped
            # hls.packages.${system}."haskell-language-server-${ghcVersion}"
          ];

        # Wrap Stack to work with our Nix integration. 
        stack-wrapped = pkgs.symlinkJoin {
          # will be available as the usual `stack` in terminal
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
          # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in
      {
        packages = {
          default = backend;
        };

        devShells = {
          default = pkgs.mkShell {
            buildInputs = myDevTools;

            # https://stackoverflow.com/a/63751678
            shellHook = ''
              export LANG="C.UTF-8";
            '';

            # Make external Nix c libraries like zlib known to GHC, like
            # pkgs.haskell.lib.buildStackProject does
            # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
          };
        };
      });
}
