{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/767542707d394ff15ac1981e903e005ba69528b5";
    flake-utils.url = "github:numtide/flake-utils/c0e246b9b83f637f4681389ecabcb2681b4f3af0";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix/a20de23b925fd8264fd7fad6454652e142fd7f73";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    my-codium = {
      url = "github:br4ch1st0chr0n3/flakes?dir=codium";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix/5926981701ac781f08b02e31e4705e46b799299d";
      flake = false;
    };
    dream2nix = {
      url = "github:nix-community/dream2nix/0f3b6c5dd1630d601ae6f456421b4dfed178f260";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-vscode-marketplace = {
      url = "github:AmeerTaweel/nix-vscode-marketplace/6378ef43e48faa8e7c6432c69a4cce471305783a";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    vscodium-extensions = {
      url = "github:br4ch1st0chr0n3/vscodium-extensions";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    haskell-language-server = {
      url = "github:haskell/haskell-language-server/ff4f29f859244f6e828ecf74fab6e8a6b745c020";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat/b4a34015c698c7793d592d66adbab377907a2be8";
      flake = false;
    };
    poetry2nix = {
      url = "github:nix-community/poetry2nix/4f8d61cd936f853242a4ce1fd476f5488c288c26";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        # dummy devshell for caching
        devShells.default = pkgs.mkShell { };
      }
    );
}
