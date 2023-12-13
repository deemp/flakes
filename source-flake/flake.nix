{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/e97b3e4186bcadf0ef1b6be22b8558eab1cdeb5d";
    devshell-source = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "flake-utils/systems";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    lima.url = "github:deemp/lima";
    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions/7cac6db467006533f4c151f0473498470f2e0bfd";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.systems.follows = "flake-utils/systems";
    };
    dream2nix = {
      url = "github:nix-community/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # TODO use purescript from nixpkgs
    nixpkgs-purescript.url = "github:deemp/nixpkgs/purescript";
    terrafix.url = "github:deemp/terrafix";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    nix-filter.url = "github:numtide/nix-filter";

    slimlock.url = "github:thomashoneyman/slimlock";
    slimlock.inputs.nixpkgs.follows = "nixpkgs";

    # TODO use the official repo when https://github.com/nlewo/nix2container/pull/99 is merged
    nix2container = {
      url = "github:deemp/nix2container";
      inputs.flake-utils.follows = "flake-utils";
    };
    nix2container.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs: (inputs // (inputs.flake-utils.lib.eachDefaultSystem (system:
    {
      formatter = inputs.nixpkgs.legacyPackages.${system}.nixpkgs-fmt;
    }
  )));
}
