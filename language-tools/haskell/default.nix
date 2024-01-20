{ system ? builtins.currentSystem
, pkgs ? (import ../..).inputs.nixpkgs.legacyPackages.${system}
, drv-tools ? (import ../../drv-tools { inherit system pkgs; })
}:
let
  lib = drv-tools.lib.mergeAttrsRecursive [ builtins pkgs.lib drv-tools.lib ];

  # `GHC` of a specific version
  # With haskell packages that are dependencies of the given packages
  ghcGHC = ghcVersion: override: packages:
    lib.withMeta
      ((haskellPackagesGHCOverride ghcVersion override).ghcWithPackages
        (ps: getHaskellPackagesDeps (packages ps)) // { pname = "ghc"; })
      (_: { mainProgram = "ghc"; });

  # build tool with `GHC` of a specific version available on PATH
  buildToolWithFlagsGHC = { name, drv, flags, ghcVersion, override, packages, runtimeDependencies }:
    assert builtins.isString name && builtins.isString ghcVersion
      && builtins.isAttrs drv && builtins.isList flags;
    let
      runtimeDependencies_ = [
        (ghcGHC ghcVersion override packages)
      ] ++ runtimeDependencies;
      flags_ = lib.concatMapStringsNewline (x: x + " \\") flags;
    in
    pkgs.stdenv.mkDerivation {
      pname = drv.pname or null;
      name = drv.name or null;
      version = drv.version or null;

      phases = [ "installPhase" ];
      buildInputs = [ pkgs.makeBinaryWrapper ];
      installPhase = ''
        runHook preInstall

        mkdir $out
        cp -rs --no-preserve=mode,ownership ${drv}/* $out
        rm -rf $out/bin
        mkdir $out/bin

        makeWrapper ${drv}/bin/${name} $out/bin/${name} \
          --add-flags "\
               ${flags_}
             " ${addBinDeps runtimeDependencies_}

        runHook postInstall
      '';

      meta = drv.meta // { mainProgram = name; };
    };

  # get deps for a Haskell package
  getHaskellPackageDeps = drv: lib.concatLists (lib.attrValues drv.getCabalDeps);

  # get deps for a list of Haskell packages
  # Example. We develop packages A and B, where B is in deps of A.
  # We don't want B in the final list of deps 
  # because `ghcWithPackages` may fail due to errors in B.
  # That's why, we remove the supplied packages (A and B in this example).
  # from the final list of deps
  getHaskellPackagesDeps = packages:
    pkgs.lib.lists.subtractLists
      packages
      (lib.concatLists (map (package: lib.concatLists (lib.attrValues package.getCabalDeps)) packages));

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
      executableName ? builtins.baseNameOf (lib.getExe package)
    , # new name of the executable
      binaryName ? executableName
    , # runtime dependencies of the executable
      # that should be available to it on `PATH`
      runtimeDependencies ? [ ]
    , # derivation description
      description ? "no description provided :("
    }:
    let exe = pkgs.haskell.lib.justStaticExecutables package; in
    pkgs.stdenv.mkDerivation {
      pname = package.pname or null;
      name = package.name or null;
      version = package.version or null;

      phases = [ "installPhase" ];
      buildInputs = [ pkgs.makeBinaryWrapper ];
      installPhase = ''
        runHook preInstall

        mkdir $out
          cp -rs --no-preserve=mode,ownership ${exe}/* $out/
          rm -rf $out/bin
          mkdir $out/bin
          makeWrapper ${exe}/bin/${executableName} $out/bin/${binaryName} \
            ${addBinDeps runtimeDependencies}

        runHook postInstall
      '';

      meta = package.meta // { mainProgram = binaryName; inherit description; };
    };

  # Tools for a specific `GHC` version and overriden haskell packages for this `GHC`
  toolsGHC =
    {
      # `GHC` version as it's set in nixpkgs
      # `version` corresponds to `pkgs.haskell.packages."ghc${version}"`
      version ? "928"
    , # override for haskell packages. See https://nixos.wiki/wiki/Haskell#Overrides, https://nixos.org/manual/nixpkgs/unstable/#haskell
      override ? { }
    , # a function from an attrset of Haskell packages to a list of packages that you develop
      # packages that you develop should be provided in the override
      packages ? (_: [ ])
    , # Programs that will be available to a build tool at development time
      # and to a Haskell program at runtime
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
      implicit-hie = pkgs.haskellPackages.implicit-hie_0_1_4_0;
      fourmolu = pkgs.haskellPackages.fourmolu;
      hpack = pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.hpack_0_36_0) (x: {
        libraryHaskellDepends = [ pkgs.haskellPackages.http-client-tls_0_3_6_3 ] ++ (x.libraryHaskellDepends or [ ]);
      });
      inherit (pkgs) ghcid;

      haskellPackages = haskellPackagesGHCOverride version override;
      inherit justStaticExecutable getHaskellPackageDeps getHaskellPackagesDeps;
    };
in
{
  lib = {
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

    ghcVersion_ = "948";
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
        inherit (tools) cabal stack hls ghc implicit-hie ghcid hpack haskellPackages fourmolu;
        inherit hello-world;
        deps = "${packageName}".getCabalDeps;
      };
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
        buildInputs = lib.attrValues {
          inherit (test) cabal stack hls ghc implicit-hie ghcid hpack fourmolu;
        };
      };
    };
  in
  {
    inherit packages devShells test;
  }
)
