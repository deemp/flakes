{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    codium.url = "github:deemp/flakes?dir=codium";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    devshell.url = "github:deemp/flakes?dir=devshell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    workflows.url = "github:deemp/flakes?dir=workflows";
    lima.url = "github:deemp/flakes?dir=lima";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      # We're going to make some dev tools for our Haskell package
      # The NixOS wiki has more info - https://nixos.wiki/wiki/Haskell

      # First, we import stuff
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (inputs.codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (inputs.codium.configs.${system}) extensions settingsNix;
      inherit (inputs.flakes-tools.functions.${system}) mkFlakesTools;
      inherit (inputs.devshell.functions.${system}) mkCommands mkShell;
      inherit (inputs.haskell-tools.functions.${system}) toolsGHC;
      inherit (inputs.workflows.functions.${system}) writeWorkflow;
      inherit (inputs.workflows.configs.${system}) nixCI;
      inherit (inputs) lima;

      # Next, we set the desired GHC version
      ghcVersion_ = "925";

      # and the name of the package
      myPackageName = "nix-managed";

      # Then, we list separately the libraries that our package needs
      myPackageDepsLib = [ pkgs.lzma ];

      # And the binaries. 
      # In our case, the Haskell app will call the `hello` command
      myPackageDepsBin = [ pkgs.hello ];

      # --- shells ---

      # First of all, we need to prepare an attrset of Haskell packages
      # and include our packages into it
      # So, we define an override - https://nixos.wiki/wiki/Haskell#Overrides
      # This is to supply the necessary libraries and executables to our packages
      # Sometimes, we need to fix the broken packages - https://gutier.io/post/development-fixing-broken-haskell-packages-nixpkgs/
      # That's why, we use several helper functions
      # Overriding the packages may trigger multiple rebuilds
      # So, we override as few packages as possible
      # If some package doesn't build, we can make a PR with a fix

      inherit (pkgs.haskell.lib)
        # doJailbreak - remove package bounds from build-depends of a package
        doJailbreak
        # dontCheck - skip tests
        dontCheck
        # override deps of a package
        overrideCabal
        ;

      # Here's our override
      # We should use `cabal v1-*` commands with it - https://github.com/NixOS/nixpkgs/issues/130556#issuecomment-1114239002
      override = {
        overrides = self: super: {
          lzma = dontCheck (doJailbreak super.lzma);
          # see what can be overriden - https://github.com/NixOS/nixpkgs/blob/0ba44a03f620806a2558a699dba143e6cf9858db/pkgs/development/haskell-modules/generic-builder.nix#L13
          myPackage = overrideCabal
            (super.callCabal2nix myPackageName ./. { })
            (x: {
              # we can combine the existing deps and new deps
              # we should write the new deps before the existing deps to override them
              # these deps will be in haskellPackages.myPackage.getCabalDeps.librarySystemDepends
              librarySystemDepends = myPackageDepsLib ++ (x.librarySystemDepends or [ ]);
              # these executables will be available to our package at runtime
              # we may skip the old deps if we'd like to
              executableSystemDepends = myPackageDepsBin;
              # here's how we can add a package built from sources
              # then, we may use this package in .cabal in a test-suite
              # (uncomment to try)
              testHaskellDepends = [
                # (super.callCabal2nix "lima" "${lima.outPath}/lima" { })
              ] ++ x.testHaskellDepends;
            });
        };
      };


      # We supply it to a helper function that will give us Haskell tools 
      # for a given compiler version, override, packages we're going to develop, 
      # and apps' runtime dependencies

      inherit (toolsGHC {
        version = ghcVersion_;
        inherit override;
        # If we work on multiple packages, we need to supply all of them.
        # Suppose we develop packages A and B, where B is in deps of A.
        # GHC will be given dependencies of both A and B.
        # However, we don't want B to be in the list of deps of GHC
        # because build of GHC may fail due to errors in B.
        packages = (ps: [ ps.myPackage ]);
        runtimeDependencies = myPackageDepsBin;
      })
        hls cabal implicit-hie justStaticExecutable
        ghcid callCabal2nix haskellPackages hpack ghc;

      codiumTools = [
        ghcid
        hpack
        implicit-hie
        cabal
        hls
        # `cabal` already has a ghc on its PATH,
        # so you may remove `ghc` from this list
        ghc
      ];

      # And compose VSCodium with dev tools and HLS
      # This is to let VSCodium run on its own, outside of a devshell
      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = codiumTools;
      };

      # a script to write .vscode/settings.json
      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one markdown-language-features;
      };

      # These tools will be available in a devshell
      tools = codiumTools;

      # --- flakes tools ---
      # Also, we provide scripts that can be used in CI
      flakesTools = mkFlakesTools [ "." ];

      # and a script to write GitHub Actions workflow file into `.github/ci.yaml`
      writeWorkflows = writeWorkflow "ci" nixCI;
    in
    {
      packages = {
        inherit (flakesTools)
          updateLocks
          pushToCachix;
        inherit
          writeSettings
          writeWorkflows
          codium;
      };

      devShells = {
        default = mkShell {
          packages = tools;
          # sometimes necessary for programs that work with files
          bash.extra = "export LANG=C.utf8";
          commands = mkCommands "tools" tools ++ [
            {
              name = "nix run .#codium .";
              category = "ide";
              help = "Run " + codium.meta.description + " in the current directory";
            }
            {
              name = "nix run .#writeSettings";
              category = "ide";
              help = writeSettings.meta.description;
            }
            {
              name = "nix run .#writeWorkflows";
              category = "infra";
              help = writeWorkflows.meta.description;
            }
            {
              name = "nix run .#updateLocks";
              category = "infra";
              help = flakesTools.updateLocks.meta.description;
            }
            {
              name = "nix run .#pushToCachix";
              category = "infra";
              help = flakesTools.pushToCachix.meta.description;
            }
          ];
        };
      };
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
