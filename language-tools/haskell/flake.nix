{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , drv-tools
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (builtins) map concatLists attrValues;

      inherit (drv-tools.functions.${system}) withAttrs concatMapStringsNewline framedBrackets;

      # GHC of a specific version
      # With haskell packages that are dependencies of the given packages
      ghcGHC = ghcVersion: override: packages:
        (haskellPackagesGHCOverride ghcVersion override).ghcWithPackages
          (ps: haskellDepsPackages (packages ps));

      # build tool with GHC of a specific version available on PATH
      buildToolWithFlagsGHC = name: drv: flags: ghcVersion: override: packages:
        assert builtins.isString name && builtins.isString ghcVersion
          && builtins.isAttrs drv && builtins.isList flags;
        let
          deps = [
            (ghcGHC ghcVersion override packages)
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

      # get packages-deps for a Haskell package
      haskellDeps = drv: concatLists (attrValues drv.getCabalDeps);

      # get packages-deps for a list of Haskell packages
      haskellDepsPackages = packages: concatLists (map haskellDeps packages);

      # --system-ghc    # Use the existing GHC on PATH (will come from a Nix file)
      # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
      stackWithFlagsGHCOverride = buildToolWithFlagsGHC "stack" pkgs.stack [ "--system-ghc" "--no-install-ghc" ];
      # --enable-nix - allow use a shell.nix if present
      cabalWithFlagsGHCOverride = buildToolWithFlagsGHC "cabal" pkgs.cabal-install [ "--enable-nix" ];

      haskellPackagesGHCOverride = ghcVersion: override: pkgs.haskell.packages."ghc${ghcVersion}".override override;

      # actually build an executable
      inherit (pkgs.haskell.lib) justStaticExecutables;

      # build an executable without local dependencies (notice empty args)
      staticExecutableGHCOverride = ghcVersion: override: name: path:
        let
          inherit (haskellPackagesGHCOverride ghcVersion override) callCabal2nix;
          exe = justStaticExecutables (callCabal2nix name path { });
        in
        withAttrs exe { pname = name; };

      # see the possible values for ghcVersion here
      # https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html#how-to-install-haskell-language-server
      hlsGHC = ghcVersion: override: (haskellPackagesGHCOverride ghcVersion override).override { supportedGhcVersions = [ ghcVersion ]; };

      # tools for a specific GHC version and overriden haskell packages for this GHC
      # see what you need to pass to your shell for GHC
      # https://docs.haskellstack.org/en/stable/nix_integration/#supporting-both-nix-and-non-nix-developers
      toolsGHCOverridePackages = ghcVersion: override: packages:
        let
          haskellPackages = haskellPackagesGHCOverride ghcVersion override;
        in
        {
          hls = hlsGHC ghcVersion override;
          stack = stackWithFlagsGHCOverride ghcVersion override packages;
          cabal = cabalWithFlagsGHCOverride ghcVersion override packages;
          ghc = ghcGHC ghcVersion override packages;
          staticExecutable = staticExecutableGHCOverride ghcVersion override;
          inherit (haskellPackages)
            implicit-hie ghcid hpack
            callCabal2nix ghcWithPackages;
          inherit haskellPackages;
          inherit haskellDeps haskellDepsPackages justStaticExecutables;
        };

      toolsGHC = ghcVersion: toolsGHCOverridePackages ghcVersion { } (_: [ ]);

      ghcVersion_ = "90";
      inherit (toolsGHC ghcVersion_) stack cabal implicit-hie;
    in
    {
      functions = {
        inherit
          toolsGHCOverridePackages
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
