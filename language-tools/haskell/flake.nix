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

      inherit (drv-tools.functions.${system})
        withAttrs concatMapStringsNewline framedBrackets
        genAttrsId;

      # GHC of a specific version
      # With haskell packages that are dependencies of the given packages
      ghcGHC = ghcVersion: override: packages:
        (haskellPackagesGHCOverride ghcVersion override).ghcWithPackages
          (ps: haskellDepsPackages (packages ps));

      # build tool with GHC of a specific version available on PATH
      buildToolWithFlagsGHC = name: drv: flags: ghcVersion: override: packages: deps_:
        assert builtins.isString name && builtins.isString ghcVersion
          && builtins.isAttrs drv && builtins.isList flags;
        let
          deps = [
            (ghcGHC ghcVersion override packages)
          ] ++ deps_;
          flags_ = concatMapStringsNewline (x: x + " \\") flags;
        in
        withAttrs
          (
            pkgs.runCommand name
              { buildInputs = [ pkgs.makeBinaryWrapper ]; }
              ''
                mkdir $out
                ln -s ${drv}/* $out
                rm $out/bin
                mkdir $out/bin

                makeWrapper ${drv}/bin/${name} $out/bin/${name} \
                  --add-flags "\
                    ${flags_}
                  " ${addDeps deps}
              ''
          )
          {
            pname = name;
            inherit (drv) meta;
          };

      # get packages-deps for a Haskell package
      haskellDeps = drv: concatLists (attrValues drv.getCabalDeps);

      # get packages-deps for a list of Haskell packages
      # filter out input packages
      haskellDepsPackages = packages:
        pkgs.lib.lists.subtractLists
          packages
          (concatLists (map (package: concatLists (attrValues package.getCabalDeps)) packages));

      # --system-ghc    # Use the existing GHC on PATH (will come from a Nix file)
      # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
      stackWithFlagsGHCOverride = buildToolWithFlagsGHC "stack" pkgs.stack [ "--system-ghc" "--no-install-ghc" ];
      # --enable-nix - allow use a shell.nix if present
      cabalWithFlagsGHCOverride = buildToolWithFlagsGHC "cabal" pkgs.cabal-install [ "--enable-nix" ];

      haskellPackagesGHCOverride = ghcVersion: override: pkgs.haskell.packages."ghc${ghcVersion}".override override;

      addDeps = deps: if deps != [ ] then "--prefix PATH : ${pkgs.lib.makeBinPath deps}" else "";

      # build an executable without local dependencies (notice empty args)
      justStaticExecutableGHCOverrideDeps = ghcVersion: override: deps: name: package:
        let
          exe = pkgs.haskell.lib.justStaticExecutables package;
        in
        withAttrs
          (pkgs.runCommand exe
            { buildInputs = [ pkgs.makeBinaryWrapper ]; }
            ''
              mkdir $out
              ln -s ${exe}/* $out

              rm $out/share
              mkdir $out/share
              cp -r ${exe}/share $out/share
                        
              rm $out/bin
              mkdir $out/bin

              makeWrapper ${exe}/bin/${package.pname} $out/bin/${name} ${addDeps deps}
            ''
          )
          { pname = name; }
      ;

      # see the possible values for ghcVersion here
      # https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html#how-to-install-haskell-language-server
      hlsGHC = ghcVersion: override: (haskellPackagesGHCOverride ghcVersion override).haskell-language-server;

      # tools for a specific GHC version and overriden haskell packages for this GHC
      # see what you need to pass to your shell for GHC
      # https://docs.haskellstack.org/en/stable/nix_integration/#supporting-both-nix-and-non-nix-developers
      haskellTools = ghcVersion: override: packages: deps:
        let
          haskellPackages = haskellPackagesGHCOverride ghcVersion override;
        in
        {
          hls = hlsGHC ghcVersion override;
          stack = stackWithFlagsGHCOverride ghcVersion override packages deps;
          cabal = cabalWithFlagsGHCOverride ghcVersion override packages deps;
          ghc = ghcGHC ghcVersion override packages;
          justStaticExecutable = justStaticExecutableGHCOverrideDeps ghcVersion override deps;
          inherit (haskellPackages)
            implicit-hie ghcid hpack
            callCabal2nix ghcWithPackages;
          inherit haskellPackages;
          inherit haskellDeps haskellDepsPackages;
        };

      haskellTools_ = ghcVersion: haskellTools ghcVersion
        {
          overrides = self: super: {
            haskell = pkgs.haskell.lib.overrideCabal
              (super.callCabal2nix "haskell" ./. {
                # you can put other packages here like
                # lzma = super.lzma;
              })
              (_: {
                librarySystemDepends = [ pkgs.zlib ];
              });
          };
        }
        (ps: [ ps.haskell ])
        [ pkgs.hello ]
      ;

      ghcVersion_ = "92";

      tools =
        let hp = haskellTools_ ghcVersion_; in
        {
          inherit (haskellTools_ ghcVersion_)
            cabal stack hls ghc implicit-hie ghcid hpack;
          hello-world = hp.justStaticExecutable "hello-world" hp.haskellPackages.haskell;
        };

      # deps that can be overriden in a package
      deps_ = (haskellTools_ ghcVersion_).haskellPackages.haskell.getCabalDeps;
    in
    {
      functions = {
        inherit haskellTools;
      };

      # test stack has `hello` on PATH
      devShells =
        {
          default = pkgs.mkShell {
            buildInputs = attrValues tools;
            shellHook = ''
              printf "\n--- cabal runs---\n"
              cabal run

              printf "\n--- exe runs ---\n"
              ${tools.hello-world}/bin/${tools.hello-world.pname}
            '';
          };
        };
      inherit deps_;
    });
}
