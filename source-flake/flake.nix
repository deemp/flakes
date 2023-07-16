{
  inputs = {
    cachix.url = "github:cachix/cachix/f6fe5df6d96e45359a1e5c448272cfc7e0566da0";
    devshell.url = "github:numtide/devshell/2cf83bb31720fcc29a999aee28d6da101173e66a";
    flake-compat = {
      url = "github:edolstra/flake-compat/b4a34015c698c7793d592d66adbab377907a2be8";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    lima.url = "github:deemp/lima";
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions/295dcce52610c93100c4098714eb06032249bd9d";
    nixpkgs.url = "github:NixOS/nixpkgs/ec322bf9e598a510995e7540f17af57ee0c8d5b9";
    poetry2nix = {
      url = "github:nix-community/poetry2nix/4f8d61cd936f853242a4ce1fd476f5488c288c26";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    vscode-extensions.url = "github:nix-community/nix-vscode-extensions/219877e9da73d5d0ee833cc2cb4b1ea26e76e1df";
    vscode-extensions-extra.url = "github:nix-community/nix-vscode-extensions/288ddbb70b5befac24602bfe7e5d9fe09dfae8d0";

    nixpkgs-purescript.url = "github:deemp/nixpkgs/purescript";
  };

  outputs = inputs: (inputs // (inputs.flake-utils.lib.eachDefaultSystem (system:
    {
      formatter = inputs.nixpkgs.legacyPackages.${system}.nixpkgs-fmt;
    }
  )));
}
