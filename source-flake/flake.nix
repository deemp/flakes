{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/ec322bf9e598a510995e7540f17af57ee0c8d5b9";
    cachix.url = "github:cachix/cachix";
    devshell-source.url = "github:numtide/devshell";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    lima.url = "github:deemp/lima";
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dream2nix.url = "github:nix-community/dream2nix";
    # TODO use purescript from nixpkgs
    nixpkgs-purescript.url = "github:deemp/nixpkgs/purescript";
    terrafix.url = "github:deemp/terrafix";
  };

  outputs = inputs: (inputs // (inputs.flake-utils.lib.eachDefaultSystem (system:
    {
      formatter = inputs.nixpkgs.legacyPackages.${system}.nixpkgs-fmt;
    }
  )));
}
