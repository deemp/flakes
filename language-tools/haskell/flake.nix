{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    gitignore_.url = "github:deemp/flakes?dir=source-flake/gitignore";
    gitignore.follows = "gitignore_/gitignore";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , gitignore
    , drv-tools
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (drv-tools.functions.${system}) withAttrs concatMapStringsNewline framedBrackets;

      # build tool with GHC of a specific version and other runtime deps available to this tool on PATH
      buildToolWithDependenciesGHC = name: drv: flags: ghcVersion: runtimeDependencies:
        assert builtins.isString ghcVersion;
        assert builtins.isList runtimeDependencies;
        let
          deps = pkgs.lib.lists.flatten [
            pkgs.haskell.compiler."ghc${ghcVersion}"
            runtimeDependencies
          ];
          flags_ = concatMapStringsNewline (x: x + " \\") flags;
        in
        withAttrs
          (pkgs.runCommand name
            { buildInputs = [ pkgs.makeBinaryWrapper ]; }
            ''
              mkdir $out
              ln -s ${drv}/* $out
              rm $out/bin
              mkdir $out/bin
              
              makeWrapper ${drv}/bin/${name} $out/bin/${name} \
                --add-flags "\
                  ${flags_}
                " \
                --prefix PATH : ${pkgs.lib.makeBinPath deps}
            ''
          )
          {
            pname = name;
            meta.description = drv.meta.description;
          };

      # --system-ghc    # Use the existing GHC on PATH (will come from a Nix file)
      # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
      stackWithDependenciesGHC = buildToolWithDependenciesGHC "stack" pkgs.stack [ "--system-ghc" "--no-install-ghc" ];
      cabalWithDependenciesGHC = buildToolWithDependenciesGHC "cabal" pkgs.cabal-install [ "--enable-nix" ];

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
          exe = justStaticExecutables
            (callCabal2nix name (gitignoreSource path) { });
        in
        withAttrs exe { pname = name; };

      # see the possible values for ghcVersion here
      # https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html#how-to-install-haskell-language-server
      hlsGHC = ghcVersion: pkgs.haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; };

      # tools for a specific GHC version
      # see what you need to pass to your shell for GHC
      # https://docs.haskellstack.org/en/stable/nix_integration/#supporting-both-nix-and-non-nix-developers
      toolsGHC = ghcVersion: {
        hls = hlsGHC ghcVersion;
        stack = stackWithDependenciesGHC ghcVersion [ ];
        cabal = cabalWithDependenciesGHC ghcVersion [ ];
        ghc = pkgs.haskell.compiler."ghc${ghcVersion}";
        callCabal = callCabalGHC ghcVersion;
        staticExecutable = staticExecutableGHC ghcVersion;
        inherit justStaticExecutables;
        stackWithDependencies = stackWithDependenciesGHC ghcVersion;
        cabalWithDependencies = cabalWithDependenciesGHC ghcVersion;
        inherit (pkgs.haskellPackages) implicit-hie ghcid;
      };

      stack =
        let inherit (toolsGHC "90") stackWithDependencies implicit-hie; in
        stackWithDependencies [ implicit-hie ];

      cabal =
        let inherit (toolsGHC "90") cabalWithDependencies implicit-hie; in
        cabalWithDependencies [ implicit-hie ];
    in
    {
      functions = {
        inherit
          toolsGHC
          ;
      };

      test = {
        inherit stack cabal;
      };

      # test stack has `hello` on PATH
      devShells.default = pkgs.mkShell {
        shellHook = ''
          cat <<EOT > Ex.hs
          {- cabal:
          build-depends: base
                      , process
          -}

          import System.Process
          main = putStrLn =<< readProcess "gen-hie" ["--help"] ""
          EOT
          
          printf "${framedBrackets "stack runs"}"
          stack runghc -- Ex
          
          printf "${framedBrackets "cabal runs"}"
          cabal run Ex.hs
          
          rm Ex.*
        '';
        buildInputs = [ stack cabal ];
      };
    });
}
