{
  inputs = {
    flakes = {
      url = "github:deemp/flakes";
    };
  };

  outputs =
    inputsTop:
    let
      inputs_ =
        let flakes = inputsTop.flakes.flakes; in
        {
          inherit (flakes.source-flake) flake-utils nixpkgs;
          inherit (flakes) devshell;
        };

      outputs = flake { } // {
        inherit flake;
        inputs = inputs_;
      };

      flake =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;

          packageName = "package";
          packageName1 = "${packageName}1";
          packageName2 = "${packageName}2";
          ghcVersion = "ghc928";

          # select a Haskell package set for a specified GHC version
          hpkgs = pkgs.haskell.packages.${ghcVersion};

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
            commands = [
              (let cabal = pkgs.cabal-install; in { name = cabal.pname; help = cabal.meta.description; package = cabal; })
              (let hls = hpkgs.haskell-language-server; in { name = hls.pname; help = hls.meta.description; package = hls; })
            ];
          };
        in
        {
          inherit devShells;
        });
    in
    outputs;
}
