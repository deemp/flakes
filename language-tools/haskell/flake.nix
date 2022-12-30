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

      # GHC of a specific version
      ghcGHC = ghcVersion: pkgs.haskell.compiler."ghc${ghcVersion}";

      # build tool with GHC of a specific version available on PATH
      buildToolWithFlagsGHC = name: drv: flags: ghcVersion:
        assert builtins.isString name && builtins.isString ghcVersion
          && builtins.isAttrs drv && builtins.isList flags;
        let
          deps = [
            (ghcGHC ghcVersion)
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
            inherit (drv) meta;
          };

      # --system-ghc    # Use the existing GHC on PATH (will come from a Nix file)
      # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
      stackWithFlagsGHC = buildToolWithFlagsGHC "stack" pkgs.stack [ "--system-ghc" "--no-install-ghc" ];
      # --enable-nix - allow use a shell.nix if present
      cabalWithFlagsGHC = buildToolWithFlagsGHC "cabal" pkgs.cabal-install [ "--enable-nix" ];

      haskellPackagesGHCOverride = ghcVersion: override: pkgs.haskell.packages."ghc${ghcVersion}".override override;

      # a convenience function for building haskell packages
      # can be used for a project with GHC 9.0.2 as follows:
      # callCabal = callCabalGHCOverride "902" { };
      # dep = callCabal "dependency-name" ./dependency-path { };
      # my-package = callCabal "my-package-name" ./my-package-path { inherit dependency; };
      callCabalGHCOverride = ghcVersion: override: name: path: args:
        let
          inherit (haskellPackagesGHCOverride ghcVersion override) callCabal2nix;
          inherit (gitignore.lib) gitignoreSource;
        in
        callCabal2nix name (gitignoreSource path) args;

      # actually build an executable
      inherit (pkgs.haskell.lib) justStaticExecutables;

      # build an executable without local dependencies (notice empty args)
      staticExecutableGHCOverride = ghcVersion: override: name: path:
        let
          exe = justStaticExecutables
            (callCabalGHCOverride ghcVersion override name path { });
        in
        withAttrs exe { pname = name; };

      # see the possible values for ghcVersion here
      # https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html#how-to-install-haskell-language-server
      hlsGHC = ghcVersion: pkgs.haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; };

      # tools for a specific GHC version and overriden haskell packages for this GHC
      # see what you need to pass to your shell for GHC
      # https://docs.haskellstack.org/en/stable/nix_integration/#supporting-both-nix-and-non-nix-developers
      toolsGHCOverride = ghcVersion: override: {
        hls = hlsGHC ghcVersion;
        stack = stackWithFlagsGHC ghcVersion;
        cabal = cabalWithFlagsGHC ghcVersion;
        ghc = ghcGHC ghcVersion;
        callCabal = callCabalGHCOverride ghcVersion override;
        staticExecutable = staticExecutableGHCOverride ghcVersion override;
        haskellPackages = haskellPackagesGHCOverride ghcVersion override;
        inherit justStaticExecutables;
        inherit (pkgs.haskellPackages) implicit-hie ghcid;
        inherit (pkgs) hpack;
      };
      
      toolsGHC = ghcVersion: toolsGHCOverride ghcVersion { };

      ghcVersion_ = "90";
      inherit (toolsGHC ghcVersion_) stack cabal implicit-hie;
    in
    {
      functions = {
        inherit
          toolsGHCOverride
          toolsGHC
          ;
      };

      test = {
        inherit stack cabal;
      };

      # test stack has `hello` on PATH
      devShells.default = pkgs.haskell.packages."ghc${ghcVersion_}".shellFor {
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
        packages = ps: [ ];
        buildInputs = [
          stack
          cabal
          implicit-hie
        ];
      };
    });
}
