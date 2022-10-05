{
  inputs.nix-vscode-marketplace = {
    url = "github:br4ch1st0chr0n3/vscodium-extensions/fix-ci/";
    inputs.nixpkgs.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs";
  };
  outputs = x: { };
}
