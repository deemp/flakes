{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    codium.url = "github:deemp/flakes?dir=codium";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    devshell.url = "github:deemp/flakes?dir=devshell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    workflows.url = "github:deemp/flakes?dir=workflows";
    lima_.url = "github:deemp/flakes?dir=source-flake/lima";
    lima.follows = "lima_/lima";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      # We're going to make some dev tools for our Haskell package
      # The NixOS wiki has more info - https://nixos.wiki/wiki/Haskell

      # --- Imports ---

      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (inputs.codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (inputs.codium.configs.${system}) extensions settingsNix;
      inherit (inputs.flakes-tools.functions.${system}) mkFlakesTools;
      inherit (inputs.devshell.functions.${system}) mkCommands mkRunCommands mkShell;
      inherit (inputs.haskell-tools.functions.${system}) toolsGHC;
      inherit (inputs.workflows.functions.${system}) writeWorkflow;
      inherit (inputs.workflows.configs.${system}) nixCI;
      inherit (inputs) lima;

      # --- Parameters ---

      # The desired GHC version
      ghcVersion = "925";

      # The name of a package
      packageName = "nix-managed";

      # The libraries that the package needs during a build
      packageLibraryDependencies = [ pkgs.lzma ];

      # The packages that provide the binaries that our package uses at runtime
      packageRuntimeDependencies = [ pkgs.hello ];

      # --- Override ---

      # We need to prepare an attrset of Haskell packages and include our packages into it,
      # so we define an override - https://nixos.wiki/wiki/Haskell#Overrides.
      # We'll supply the necessary dependencies to our packages.
      # Sometimes, we need to fix the broken packages - https://gutier.io/post/development-fixing-broken-haskell-packages-nixpkgs/.
      # For doing that, we use several helper functions.
      # Overriding the packages may trigger multiple rebuilds,
      # so we override as few packages as possible.

      inherit (pkgs.haskell.lib)
        # doJailbreak - remove package bounds from build-depends of a package
        doJailbreak
        # dontCheck - skip tests
        dontCheck
        # override deps of a package
        overrideCabal
        ;

      # Here's our override
      # Haskell overrides are described here: https://nixos.org/manual/nixpkgs/unstable/#haskell
      override = {
        overrides = self: super: {
          lzma = dontCheck (doJailbreak super.lzma);
          "${packageName}" = overrideCabal
            (super.callCabal2nix packageName ./. { })
            (x: {
              # See what can be overriden - https://github.com/NixOS/nixpkgs/blob/0ba44a03f620806a2558a699dba143e6cf9858db/pkgs/development/haskell-modules/generic-builder.nix#L13
              # And the explanation of the deps - https://nixos.org/manual/nixpkgs/unstable/#haskell-derivation-deps

              # Dependencies of the our package library
              # New deps go before the existing deps and override them
              librarySystemDepends = packageLibraryDependencies ++ (x.librarySystemDepends or [ ]);

              # Dependencies of our package executables
              executableSystemDepends = packageLibraryDependencies ++ (x.executableSystemDepends or [ ]);

              # Here's how we can add a package built from sources
              # Later, we may use this package in `.cabal` in a test-suite
              # We should use `cabal v1-*` commands with it - https://github.com/NixOS/nixpkgs/issues/130556#issuecomment-1114239002
              # `lima` lets you write a `README.hs` and convert it to `README.md` - https://hackage.haskell.org/package/lima
              # Uncomment `lima` to use it
              testHaskellDepends = [
                # (super.callCabal2nix "lima" lima.outPath { })
              ] ++ (x.testHaskellDepends or [ ]);
            });
        };
      };

      # --- Haskell tools ---

      # We call a helper function that will give us tools for Haskell
      inherit (toolsGHC {
        version = ghcVersion;
        inherit override;
        runtimeDependencies = packageRuntimeDependencies;
        # If we work on multiple packages, we need to supply all of them.
        # Suppose we develop packages A and B, where B is in deps of A.
        # GHC will be given dependencies of both A and B.
        # However, we don't want B to be in the list of deps of GHC
        # because build of GHC may fail due to errors in B.
        packages = (ps: [ ps.${packageName} ]);
      })
        hls cabal implicit-hie justStaticExecutable
        ghcid callCabal2nix haskellPackages hpack ghc;

      # --- Tools ---

      # We list the tools that we'd like to use
      tools = [
        ghcid
        hpack
        implicit-hie
        cabal
        hls
        # `cabal` already has a `ghc` on its `PATH`,
        # so you may remove `ghc` from this list
        # Then, you can access `ghc` via `cabal repl` -> `ghci>:! ghc`
        ghc
      ];

      # --- Packages ---

      packages = {
        # --- Haskell package ---

        # This is a static executable with given runtime dependencies.
        # In this case, its name is the same as the package name.
        default = justStaticExecutable {
          package = haskellPackages.${packageName};
          runtimeDependencies = packageRuntimeDependencies;
          description = "A Haskell `hello-world` script";
        };

        # --- IDE ---

        # This part can be removed if you don't use `VSCodium`
        # We compose `VSCodium` with dev tools and `HLS`
        # This is to let `VSCodium` run on its own, outside of a devshell
        codium = mkCodium {
          extensions = { inherit (extensions) nix haskell misc github markdown; };
          runtimeDependencies = tools;
        };

        # a script to write `.vscode/settings.json`
        writeSettings = writeSettingsJSON {
          inherit (settingsNix) haskell todo-tree files editor gitlens
            git nix-ide workbench markdown-all-in-one markdown-language-features;
        };

        # --- Flakes ---

        # Scripts that can be used in CI
        inherit (mkFlakesTools [ "." ]) updateLocks pushToCachix;

        # --- GH Actions

        # A script to write GitHub Actions workflow file into `.github/ci.yaml`
        writeWorkflows = writeWorkflow "ci" nixCI;
      };

      # --- Devshells ---

      devShells = {
        default = mkShell {
          packages = tools;
          # sometimes necessary for programs that work with files
          bash.extra = "export LANG=C.utf8";
          commands =
            mkCommands "tools" tools
            ++ mkCommands "packages" [ packages.default ]
            ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; }
            ++ mkRunCommands "infra" { inherit (packages) writeWorkflows updateLocks pushToCachix; }
            ++
            [
              {
                name = "cabal v1-run";
                category = "scripts";
                help = "Run app via cabal";
              }
            ];
        };
      };
    in
    {
      inherit packages devShells;

      ghcVersions = pkgs.lib.attrsets.mapAttrsToList (name: _: pkgs.lib.strings.removePrefix "ghc" name) pkgs.haskell.compiler;
    });

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
