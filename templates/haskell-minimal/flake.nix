{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs: inputs.flakes.makeFlake {
    inputs = { inherit (inputs.flakes.all) nixpkgs devshell; };
    perSystem = { inputs, system }:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;

        packageName = "package";
        packageName1 = "${packageName}1";
        packageName2 = "${packageName}2";
        ghcVersion = "928";

        # select a Haskell package set for a specified GHC version
        hpkgs = pkgs.haskell.packages."ghc${ghcVersion}";

        # Provide overrides
        # https://nixos.wiki/wiki/Haskell#Overrides
        # An override should include a local package into the Haskell package set
        # When composing an override, once can override multiple package attributes
        # https://nixos.org/manual/nixpkgs/stable/#haskell-mkderivation
        override = {
          overrides = self: super: {
            "${packageName1}" = pkgs.haskell.lib.overrideCabal (self.callCabal2nix packageName1 ./${packageName1} { }) (x: {
              libraryToolDepends = [ pkgs.zlib ] ++ (x.libraryToolDepends or [ ]);
            });
            "${packageName2}" = self.callCabal2nix packageName2 ./${packageName2} { "${packageName1}" = self.${packageName1}; };
          };
        };

        hpkgs_ = hpkgs.override override;

        # https://nixos.wiki/wiki/Haskell#Using_shellFor_.28multiple_packages.29
        devShells.shellFor = hpkgs_.shellFor {
          packages = ps: [ ps.${packageName1} ps.${packageName2} ];
          withHoogle = true;
        };

        devShells.default = mkShell {
          bash.extra = ''
            cabal run package2
          '';
          packagesFrom = [ devShells.shellFor ];
          commands = mkCommands "tools" [ pkgs.cabal-install hpkgs.haskell-language-server ];
        };
      in
      {
        inherit devShells;
      };
  };
}
