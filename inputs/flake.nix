{
  inputs = {
    source.url = path:../source;
    nixpkgs.follows = "source/nixpkgs";
    flake-utils.follows = "source/flake-utils";
    gitignore.follows = "source/gitignore";
    easy-purescript-nix.follows = "source/easy-purescript-nix";
    dream2nix.follows = "source/dream2nix";
    nix-vscode-marketplace.follows = "source/nix-vscode-marketplace";
    vscodium-extensions.follows = "source/vscodium-extensions";
    haskell-language-server.follows = "source/haskell-language-server";
    flake-compat.follows = "source/flake-compat";
    poetry2nix.follows = "source/poetry2nix";
    my-codium.url = path:../codium;
    json2md.url = path:../json2md;
  };
  outputs = inputs: { };
}
