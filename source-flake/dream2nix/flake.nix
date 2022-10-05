{
  inputs.dream2nix = {
    url = "github:nix-community/dream2nix/0f3b6c5dd1630d601ae6f456421b4dfed178f260";
    inputs.nixpkgs.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs";
  };
  outputs = x: { };
}
