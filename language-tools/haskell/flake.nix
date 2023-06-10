{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (builtins) map concatLists attrValues;
      inherit (pkgs.lib.lists) genAttrs;
      inherit (pkgs.lib.strings) concatMapStringsSep;
      inherit (pkgs.lib.attrsets) recursiveUpdate;
      concatMapStringsNewline = concatMapStringsSep "\n";
      genAttrsId = list: genAttrs list (x: x);
      withAttrs = recursiveUpdate;

      # `GHC` of a specific version
      # With haskell packages that are dependencies of the given packages
      ghcGHC = ghcVersion: override: packages:
        ((haskellPackagesGHCOverride ghcVersion override).ghcWithPackages
          (ps: getHaskellPackagesDeps (packages ps))) // { pname = "ghc"; };

      # build tool with `GHC` of a specific version available on PATH
      buildToolWithFlagsGHC = { name, drv, flags, ghcVersion, override, packages, runtimeDependencies }:
        assert builtins.isString name && builtins.isString ghcVersion
          && builtins.isAttrs drv && builtins.isList flags;
        let
          runtimeDependencies_ = [
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
                cp -rs --no-preserve=mode,ownership ${drv}/* $out
                rm -rf $out/bin
                mkdir $out/bin

                makeWrapper ${drv}/bin/${name} $out/bin/${name} \
                  --add-flags "\
                    ${flags_}
                  " ${addBinDeps runtimeDependencies_}
              ''
          )
          {
            pname = name;
            inherit (drv) meta;
          };

      # get deps for a Haskell package
      getHaskellPackageDeps = drv: concatLists (attrValues drv.getCabalDeps);

      # get deps for a list of Haskell packages
      # Example. We develop packages A and B, where B is in deps of A.
      # We don't want B in the final list of deps 
      # because `ghcWithPackages` may fail due to errors in B.
      # That's why, we remove the supplied packages (A and B in this example).
      # from the final list of deps
      getHaskellPackagesDeps = packages:
        pkgs.lib.lists.subtractLists
          packages
          (concatLists (map (package: concatLists (attrValues package.getCabalDeps)) packages));

      # --system-ghc    # Use the existing `GHC` on `PATH` (will come from a Nix file)
      # --no-install-ghc  # Don't try to install `GHC` if no matching `GHC` found on `PATH`
      stackWithFlagsGHCOverride = args@{ ghcVersion, override, packages, runtimeDependencies }:
        buildToolWithFlagsGHC ({ name = "stack"; drv = pkgs.stack; flags = [ "--system-ghc" "--no-install-ghc" ]; } // args);
      # --enable-nix - allow use a shell.nix if present
      cabalWithFlagsGHCOverride = args@{ ghcVersion, override, packages, runtimeDependencies }:
        buildToolWithFlagsGHC ({ name = "cabal"; drv = pkgs.cabal-install; flags = [ "--enable-nix" ]; } // args);

      haskellPackagesGHCOverride = ghcVersion: override: (haskellPackagesGHC ghcVersion).override override;

      haskellPackagesGHC = ghcVersion: pkgs.haskell.packages."ghc${ghcVersion}";

      addBinDeps = deps: if deps != [ ] then "--prefix PATH : ${pkgs.lib.makeBinPath deps}" else "";

      # Build an executable from a Haskell package
      # Provide it with given runtime dependencies
      justStaticExecutable =
        {
          # package that contains an executable
          package
        , # name of the executable
          executableName ? package.pname
        , # new name of the executable
          binaryName ? executableName
        , # runtime dependencies of the executable
          # that should be available to it on `PATH`
          runtimeDependencies ? [ ]
        , # derivation description
          description ? "no description provided :("
        }:
        let exe = pkgs.haskell.lib.justStaticExecutables package; in
        withAttrs
          (pkgs.runCommand binaryName
            { buildInputs = [ pkgs.makeBinaryWrapper ]; }
            ''
              mkdir $out
              cp -rs --no-preserve=mode,ownership ${exe}/* $out/
              rm -rf $out/bin
              mkdir $out/bin
              makeWrapper ${exe}/bin/${executableName} \
                $out/bin/${binaryName} \
                ${addBinDeps runtimeDependencies}
            ''
          )
          {
            pname = binaryName;
            name = "${binaryName}-${package.version}";
            meta = { inherit description; };
          }
      ;

      # Tools for a specific `GHC` version and overriden haskell packages for this `GHC`
      toolsGHC =
        {
          # `GHC` version as it's set in nixpkgs
          # `version` corresponds to `pkgs.haskell.packages.${version}`
          version ? "945"
        , # override for haskell packages. See https://nixos.wiki/wiki/Haskell#Overrides
          override ? { }
        , # a function from an attrset Haskell packages to a list of packages that you develop
          # packages that you develop should be provided in the override
          packages ? (_: [ ])
        , # what should be available on PATH
          # this lists lists necessary non-Haskell executables for all Haskell packages
          # These non-Haskell packages will be available to a build tool
          runtimeDependencies ? [ ]
        }:
        {
          hls = (haskellPackagesGHC version).haskell-language-server;
          # https://docs.haskellstack.org/en/stable/nix_integration/#supporting-both-nix-and-non-nix-developers
          stack = stackWithFlagsGHCOverride {
            ghcVersion = version;
            inherit override packages runtimeDependencies;
          };
          cabal = cabalWithFlagsGHCOverride {
            ghcVersion = version;
            inherit override packages runtimeDependencies;
          };
          ghc = ghcGHC version override packages;
          
          inherit (haskellPackagesGHC version) callCabal2nix;
          implicit-hie = pkgs.haskell.packages.ghc945.implicit-hie_0_1_4_0;
          inherit (pkgs) ghcid hpack;

          haskellPackages = haskellPackagesGHCOverride version override;
          inherit justStaticExecutable getHaskellPackageDeps getHaskellPackagesDeps;
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
        packageName = "hello-world";
        toolsGHC_ = version: toolsGHC {
          inherit version;
          # docs on overrides - https://nixos.org/manual/nixpkgs/unstable/#haskell-overriding-haskell-packages
          override = {
            overrides = self: super: {
              ${packageName} = pkgs.haskell.lib.overrideCabal
                (super.callCabal2nix packageName ./. {
                  # here can be the local packages that our package depends on
                })
                (x: {
                  # see what can be overriden - https://github.com/NixOS/nixpkgs/blob/0ba44a03f620806a2558a699dba143e6cf9858db/pkgs/development/haskell-modules/generic-builder.nix#L13
                  # deps explanation - https://nixos.org/manual/nixpkgs/unstable/#haskell-derivation-deps

                  # The dependencies of the library
                  # Newer deps listed before the existing deps override the existing ones
                  libraryToolDepends = [ pkgs.zlib ] ++ (x.libraryToolDepends or [ ]);
                });
            };
          };
          # Packages that we develop
          packages = (ps: [ ps.${packageName} ]);
          # What our packages need at runtime
          # We have a single `cabal` and puth these deps on its `PATH`
          runtimeDependencies = [ pkgs.hello ];
        };

        ghcVersion_ = "945";
        executableName = "hello-world";
        binaryName = "hello";
        test =
          let
            tools = toolsGHC_ ghcVersion_;
            hello-world = tools.justStaticExecutable {
              package = tools.haskellPackages.${packageName};
              inherit binaryName;
              runtimeDependencies = [ pkgs.hello ];
              description = "A Haskell `hello-world` script";
            };
          in
          {
            inherit (tools) cabal stack hls ghc implicit-hie ghcid hpack haskellPackages;
            inherit hello-world;
            deps = "${packageName}".getCabalDeps;
          };
      in
      {
        packages.default = test.${packageName};
        # test cabal has `hello` on PATH
        devShells = {
          default = pkgs.mkShell {
            shellHook = ''
              printf "\n--- cabal runs ---\n"
              ${test.cabal}/bin/cabal run ${executableName}

              printf "\n--- stack runs ---\n"
              ${test.stack}/bin/stack run ${executableName}

              printf "\n--- Haskell executable runs ---\n"
              ${test.${packageName}}/bin/${binaryName}
            '';
            buildInputs = __attrValues {
              inherit (test) cabal stack hls ghc implicit-hie ghcid hpack;
            };
          };
        };
        inherit test;
      }
    )
    );
}
