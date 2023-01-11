{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = "github:deemp/flakes?dir=codium";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    devshell.url = "github:deemp/flakes?dir=devshell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    workflows.url = "github:deemp/flakes?dir=workflows";
    # necessary for stack-nix integration
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs =
    { self
    , flake-utils
    , flakes-tools
    , nixpkgs
    , my-codium
    , drv-tools
    , haskell-tools
    , devshell
    , workflows
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      # We're going to make some dev tools for our Haskell package
      # See NixOS wiki for more info - https://nixos.wiki/wiki/Haskell

      # First, we import stuff
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (drv-tools.functions.${system}) mkBin withAttrs withMan withDescription mkShellApp;
      inherit (drv-tools.configs.${system}) man;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      inherit (devshell.functions.${system}) mkCommands mkShell;
      inherit (haskell-tools.functions.${system}) haskellTools;
      inherit (workflows.functions.${system}) writeWorkflow;
      inherit (workflows.configs.${system}) nixCI;

      # Next, set the desired GHC version
      ghcVersion_ = "92";

      # and the name of the package
      myPackageName = "nix-managed";

      # Then, we list separately the libraries that our package needs
      myPackageDepsLib = [ pkgs.lzma ];

      # And the binaries. 
      # In our case, the Haskell app will call the `hello` command
      myPackageDepsBin = [ pkgs.hello ];

      # --- shells ---

      # First of all, we need to prepare the haskellPackages attrset
      # So, we define the overrides - https://nixos.wiki/wiki/Haskell#Overrides
      # This is to supply the necessary libraries and executables to our packages
      # Sometimes, we need to fix the broken packages - https://gutier.io/post/development-fixing-broken-haskell-packages-nixpkgs/
      # That's why, inherit several helper functions
      # Note that overriding the packages from haskellPackages will require their rebuilds
      # So, override as few packages as possible and consider making a PR when haskellPackages.somePackage doesn't build

      inherit (pkgs.haskell.lib)
        # doJailbreak - remove package bounds from build-depends of a package
        doJailbreak
        # dontCheck - skip tests
        dontCheck
        # override deps of a package
        # see what can be overriden - https://github.com/NixOS/nixpkgs/blob/0ba44a03f620806a2558a699dba143e6cf9858db/pkgs/development/haskell-modules/generic-builder.nix#L13
        overrideCabal
        ;

      # Here's our override
      override = {
        overrides = self: super: {
          lzma = dontCheck (doJailbreak super.lzma);
          myPackage = pkgs.haskell.lib.overrideCabal
            (super.callCabal2nix myPackageName ./. { })
            (_: {
              # these deps will be in haskellPackages.myPackage.getCabalDeps.librarySystemDepends
              librarySystemDepends = myPackageDepsLib;
              executableSystemDepends = myPackageDepsBin;
            });
        };
      };

      # We supply it to a helper function that will give us haskell tools for given 
      # compiler version, override, packages we're going to develop, and their binary runtime dependencies

      # Our devShells should only be aware of the dev dependencies of the Haskell packages that we're going to develop
      # So, we need to supply all Haskell packages that we'd like to develop so that they're excluded from the dev dependencies
      # More specifically, if we're developing Haskell packages A and B and A depends on B, we need to supply both A and B
      # This will prevent nix from building B as a dev dependency of A

      inherit (haskellTools ghcVersion_ override (ps: [ ps.myPackage ]) myPackageDepsBin)
        stack hls cabal implicit-hie justStaticExecutable
        ghcid callCabal2nix haskellPackages hpack;


      # wrap a command output
      framed = command: ''
        printf "====\n"
        ${command}
        printf "====\n"
      '';

      # --- shellFor ---
      # Now, we can use shellFor with overriden haskellPackages
      # We'll use an ordinary cabal-install as we want to take it from Nix cache
      # In this shell, we'll get some Haskell packages from cache
      # Some other we'll be able to build incrementally via cabal-install
      # Incremental build means that only necessary rebuilds will be made on changes in a package
      cabalShellFor =
        haskellPackages.shellFor {
          packages = ps: [ ps.myPackage ];
          nativeBuildInputs = myPackageDepsBin ++ [ pkgs.cabal-install ];
          withHoogle = true;
          # get other tools with names
          shellHook = framed "cabal run";
        };

      # The disadvantage of such a shellFor is that we have to run this shell before we can start development
      # If we also would like to start another shell with another toolset (like `devshell` - https://github.com/numtide/devshell)
      # we'll have to use shellHook = 'nix develop .#anotherShell', and that's not cool

      # --- smart cabal ---
      # To overcome the disadvantage of shellFor, we inherited `cabal` from `haskellTools` above
      # this is cabal-install that's aware of `GHC` and runtime deps of our Haskell app
      # moreover, that `GHC` is aware of the dev dependencies of our Haskell app
      # So, instead of a shell like in the case of `shellFor`, we now get a single `cabal` executable
      # This is the best dev way, IMO

      cabalShell = mkShell {
        packages = [ cabal ];
        bash.extra = framed "cabal run";
        commands = mkCommands "tools" [ cabal ];
      };

      # --- build an executable ---
      # We can take one step further and make our app builds reproducible
      # We'll take the haskellPackages.myPackage and turn it into an executable
      # At this moment, we can set a name of our executable

      myPackageExeName = "my-package";

      # We'll also add a description and a man page
      myPackageExe =
        withMan
          (withDescription
            (justStaticExecutable myPackageExeName haskellPackages.myPackage)
            "Demo Nix-packaged `Haskell` program "
          )
          (
            x: ''
              ${man.DESCRIPTION}
              ${x.meta.description}
            ''
          )
      ;
      # A disadvantage of this approach is that the package and all its local dependencies
      # will be rebuilt even if only that package changes
      # So, the builds aren't incremental

      # In this shell, we'll run the obtained executable
      nixPackagedShell = mkShell
        {
          packages = [ myPackageExe ];
          bash.extra = framed (mkBin myPackageExe);
          commands = mkCommands "tools" [ myPackageExe ];
        };


      # --- docker image ---
      # What if we'd like to share our Haskell app?
      # In this case, we can easily make a Docker image with it
      # We'll take an executable from the previous step and put it into an image
      # At this moment, we can set a name and a tag of our image

      myPackageImageName = "my-package-image";
      myPackageImageTag = "latest";

      myPackageImage = pkgs.dockerTools.buildLayeredImage {
        name = myPackageImageName;
        tag = myPackageImageTag;
        config.Entrypoint = [ "bash" "-c" myPackageExe.name ];
        contents = [ pkgs.bash myPackageExe ];
      };

      # The image is ready. We can run it in a devshell
      dockerShell = mkShell {
        packages = [ pkgs.docker ];
        bash.extra = ''
          docker load < ${myPackageImage}
          ${framed "docker run -it ${myPackageImageName}:${myPackageImageTag}"}
        '';
        commands = mkCommands "tools" [ pkgs.docker ];
      };


      # --- stack ---
      # One more way to run our app is to use Stack - Nix integration - https://docs.haskellstack.org/en/stable/nix_integration/#external-c-libraries-through-a-shellnix-file
      # First, we need to set in `stack.yaml`:
      # nix: 
      #   enable: true
      #   shell-file: stack.nix
      # Next, we need to say in `stack.nix` that it should take from this flakes the output called `stack-shell`
      # For `stack.nix` to be able to do that, we have to include `flake-compat` into inputs of this flake
      # Of course, we can select a name that differs from `stack-shell`
      # We just have to make sure that we use the same name in this flake's outputs and in `stack.nix`
      stack-shell = { ghcVersion }:
        pkgs.haskell.lib.buildStackProject {
          name = "stack-shell";

          ghc = pkgs.haskell.compiler.${ghcVersion};

          buildInputs = myPackageDepsLib ++ myPackageDepsBin;
        };

      # When everything is set up, we can run stack in a shell
      stackShell = mkShell {
        packages = [ pkgs.stack ];
        bash.extra = framed "stack run";
        commands = mkCommands "tools" [ pkgs.stack ];
      };
      # The disadvantage of this way is that we depend on stack's resolver, 
      # while smart cabal depends just on GHC and the packages from haskellPackages

      # --- dev tools ---
      # There are other tools that we can provide
      # Some Haskell tools and VSCodium with extensions and settings

      codiumTools = [
        ghcid
        hpack
        implicit-hie
        cabal
        hls
      ];

      # And compose VSCodium with dev tools and HLS
      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = codiumTools;
      };

      # a script to write .vscode/settings.json
      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one markdown-language-features;
      };

      tools = codiumTools ++ [ codium ];

      defaultShell = mkShell {
        packages = tools;
        commands = mkCommands "tools" tools;
      };

      # --- flakes tools ---
      # Also, we provide scripts that can be used in CI
      flakesTools = mkFlakesTools [ "." ];

      # write .github/ci.yaml to get a GitHub Actions workflow file
      writeWorkflows = writeWorkflow "ci" nixCI;
    in
    {
      packages = {
        inherit (flakesTools)
          updateLocks
          pushToCachix;
        inherit
          writeSettings
          writeWorkflows;
      };

      devShells = {
        default = defaultShell;

        nixPackaged = nixPackagedShell;

        cabal = cabalShell;

        cabalShellFor = cabalShellFor;

        docker = dockerShell;

        stack = stackShell;
      };

      inherit stack-shell;
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
