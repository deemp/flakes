{
  inputs = {
    nixpkgs_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs;
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;
    flake-utils.follows = "flake-utils_/flake-utils";
    gitignore_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/gitignore;
    gitignore.follows = "gitignore_/gitignore";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , gitignore
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      shellTools = {
        inherit (pkgs.haskellPackages)
          # auto generate LSP hie.yaml file fm cabal
          implicit-hie
          # Convert package.yaml to *.cabal
          hpack
          # GHCi based bare bones IDE
          ghcid
          ;
        # Haskell-language-server
        inherit (pkgs) haskell-language-server;
      };

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

      # a convenience function for building haskell packages
      # can be used for a project with GHC 9.0.2 as follows:
      # callCabal = callCabalGHC "902";
      # dep = callCabal "dependency-name" ./dependency-path { };
      # my-package = callCabal "my-package-name" ./my-package-path { inherit dependency; };
      callCabalGHC = ghcVersion: name: path: args:
        let
          inherit (pkgs.haskell.packages."ghc${ghcVersion}") callCabal2nix;
          inherit (gitignore.lib) gitignoreSource;
        in
        callCabal2nix name (gitignoreSource path) args;


      # actually build an executable
      inherit (pkgs.haskell.lib) justStaticExecutables;

      # build an executable without local dependencies (notice empty args)
      staticExecutableGHC = ghcVersion: name: path:
        let
          inherit (pkgs.haskell.packages."ghc${ghcVersion}") callCabal2nix;
          inherit (gitignore.lib) gitignoreSource;
        in
        justStaticExecutables
          (callCabal2nix name (gitignoreSource path) { });

      # stack and ghc of a specific version
      # they should come together so that stack doesn't use the system ghc
      stackGHC = ghcVersion: [
        stack-wrapped
        pkgs.haskell.compiler."ghc${ghcVersion}"
      ];

      # see the possible values for ghcVersion here
      # https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html#how-to-install-haskell-language-server
      hlsGHC = ghcVersion: pkgs.haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; };

      # tools for a specific GHC version
      toolsGHC = ghcVersion: {
        hls = hlsGHC ghcVersion;
        # see what you need to pass to your shell for GHC
        # https://docs.haskellstack.org/en/stable/nix_integration/#supporting-both-nix-and-non-nix-developers
        stack = stackGHC ghcVersion;
        callCabal = callCabalGHC ghcVersion;
        staticExecutable = staticExecutableGHC ghcVersion;
      };
    in
    {
      packages = {
        inherit stack-wrapped;
      };
      toolSets = {
        inherit shellTools;
      };
      functions = {
        inherit
          toolsGHC
          ;
      };
    }
    );
}
