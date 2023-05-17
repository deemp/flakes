{
  inputs = {
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};

      packageName = "nix-managed";
      ghcVersion = "ghc927";
      
      # select a Haskell package set for a specified GHC version
      hpkgs = pkgs.haskell.packages.${ghcVersion};
      
      # Provide overrides
      # https://nixos.wiki/wiki/Haskell#Overrides
      # An override should include a local package into the Haskell package set
      override = { overrides = self: super: { "${packageName}" = self.callCabal2nix packageName ./. { }; }; };
      hpkgs_ = hpkgs.override override;
      
      # Get all dependencies of local Haskell packages excluding these local packages
      # This approach is useful for cases when a local package A depends on a local package B
      # In this case, package B won't be built by Nix as a dependency of A
      getHaskellPackagesDeps = someHaskellPackages: let l = pkgs.lib.lists; in (l.subtractLists someHaskellPackages (l.concatLists (map (package: l.concatLists (__attrValues package.getCabalDeps)) someHaskellPackages)));
      
      # build a GHC with the dependencies of local Haskell packages
      ghcForPackages = localHaskellPackageNames: hpkgs_.ghcWithPackages (ps: (getHaskellPackagesDeps (map (x: ps.${x}) localHaskellPackageNames) ++ [ ps.haskell-language-server ])); # Why provide HLS here - https://github.com/NixOS/nixpkgs/issues/225895#issuecomment-1509991742
      
      # GHC with dependencies of local Haskell packages
      ghc = ghcForPackages [ packageName ];

      # tools that should be available in a development shell
      tools = [
        pkgs.cabal-install
        pkgs.hpack
        # haskell-language-server is already available as a GHC package
        ghc
      ];

      # default development shell
      devShells.default = pkgs.mkShell {
        shellHook = "export LANG=C.utf8";
        buildInputs = tools;
      };

      # default package is the local package
      packages = {
        default = hpkgs_.${packageName};
        default-static = hpkgs_.lib.justStaticExecutables hpkgs_.${packageName};
      };
    in
    {
      inherit packages devShells;
    });
}
