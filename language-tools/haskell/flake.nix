{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (builtins) map concatLists attrValues;
      inherit (pkgs.lib.lists) genAttrs;
      inherit (pkgs.lib.strings) concatMapStringsSep;
      inherit (pkgs.lib.attrsets) recursiveUpdate;
      concatMapStringsNewline = concatMapStringsSep "\n";
      genAttrsId = list: genAttrs list (x: x);
      withAttrs = recursiveUpdate;

      # GHC of a specific version
      # With haskell packages that are dependencies of the given packages
      ghcGHC = ghcVersion: override: packages:
        ((haskellPackagesGHCOverride ghcVersion override).ghcWithPackages
          (ps: haskellDepsPackages (packages ps))) // { pname = "ghc${ghcVersion}"; };

      # build tool with GHC of a specific version available on PATH
      buildToolWithFlagsGHC = name: drv: flags: ghcVersion: override: packages: runtimeDependencies:
        assert builtins.isString name && builtins.isString ghcVersion
          && builtins.isAttrs drv && builtins.isList flags;
        let
          deps = [
            (ghcGHC ghcVersion override packages)
          ] ++ runtimeDependencies;
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

      # get deps for a Haskell package
      haskellDeps = drv: concatLists (attrValues drv.getCabalDeps);

      # get deps for a list of Haskell packages
      # Example. We develop packages A and B, where B is in deps of A.
      # We don't want B in the final list of deps 
      # because `ghcWithPackages` may fail due to errors in B.
      # That's why, we remove the supplied packages (A and B in this example).
      # from the final list of deps
      haskellDepsPackages = packages:
        pkgs.lib.lists.subtractLists
          packages
          (concatLists (map (package: concatLists (attrValues package.getCabalDeps)) packages));

      # --system-ghc    # Use the existing GHC on PATH (will come from a Nix file)
      # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
      stackWithFlagsGHCOverride = buildToolWithFlagsGHC "stack" pkgs.stack [ "--system-ghc" "--no-install-ghc" ];
      # --enable-nix - allow use a shell.nix if present
      cabalWithFlagsGHCOverride = buildToolWithFlagsGHC "cabal" pkgs.cabal-install [ "--enable-nix" ];

      haskellPackagesGHCOverride = ghcVersion: override: (haskellPackagesGHC ghcVersion).override override;

      haskellPackagesGHC = ghcVersion: pkgs.haskell.packages."ghc${ghcVersion}";

      addDeps = deps: if deps != [ ] then "--prefix PATH : ${pkgs.lib.makeBinPath deps}" else "";

      # build an executable without local dependencies (notice empty args)
      justStaticExecutableGHCOverrideDeps = deps: name: package:
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
      hlsGHC = ghcVersion: (haskellPackagesGHC ghcVersion).haskell-language-server;

      # tools for a specific GHC version and overriden haskell packages for this GHC
      # see what you need to pass to your shell for GHC
      # https://docs.haskellstack.org/en/stable/nix_integration/#supporting-both-nix-and-non-nix-developers
      toolsGHC =
        {
          # GHC version as it's set in nixpkgs
          # `925` is for `pkgs.haskell.packages.ghc925`
          version ? "925"
        , # override for haskell packages. See https://nixos.wiki/wiki/Haskell#Overrides
          override ? { }
        , # a function from an attrset Haskell packages to a list of packages that you develop
          # packages that you develop should be provided in the override
          packages ? (_: [ ])
        , # what should be available on PATH to a Haskell app at runtime
          runtimeDependencies ? [ ]
        }:
        {
          hls = hlsGHC version;
          stack = stackWithFlagsGHCOverride version override packages runtimeDependencies;
          cabal = cabalWithFlagsGHCOverride version override packages runtimeDependencies;
          ghc = ghcGHC version override packages;
          justStaticExecutable = justStaticExecutableGHCOverrideDeps runtimeDependencies;
          inherit (haskellPackagesGHC version) implicit-hie ghcid hpack callCabal2nix;
          haskellPackages = haskellPackagesGHCOverride version override;
          inherit haskellDeps haskellDepsPackages;
        };
    in
    {
      functions = {
        inherit toolsGHC;
      };
    }
    # Tests
    // (
      let
        toolsGHC_ = version: toolsGHC {
          inherit version;
          override = {
            overrides = self: super: {
              haskell = pkgs.haskell.lib.overrideCabal
                (super.callCabal2nix "haskell" ./. {
                  # here can be the local packages that 
                  # `haskell` package depends on
                })
                (x: {
                  # see what can be overriden - https://github.com/NixOS/nixpkgs/blob/0ba44a03f620806a2558a699dba143e6cf9858db/pkgs/development/haskell-modules/generic-builder.nix#L13
                  # include the previous deps
                  librarySystemDepends = [ pkgs.zlib ] ++ (x.librarySystemDepends or [ ]);
                });
            };
          };
          packages = (ps: [ ps.haskell ]);
          runtimeDependencies = [ pkgs.hello ];
        };

        ghcVersion_ = "925";

        tools =
          let hp = toolsGHC_ ghcVersion_; in
          {
            inherit (toolsGHC_ ghcVersion_)
              cabal stack hls ghc implicit-hie ghcid hpack;
            hello-world = hp.justStaticExecutable "hello-world" hp.haskellPackages.haskell;
          };

        # deps that can be overriden in a package
        deps_ = (toolsGHC_ ghcVersion_).haskellPackages.haskell.getCabalDeps;
      in
      {
        # test stack has `hello` on PATH
        devShells =
          {
            default = pkgs.mkShell {
              buildInputs = attrValues tools;
              shellHook = ''
                printf "\n--- cabal runs ---\n"
                cabal run

                printf "\n--- Haskell executable runs ---\n"
                ${tools.hello-world}/bin/${tools.hello-world.pname}
              '';
            };
          };
        inherit deps_ tools;
      }
    )
    );
}
