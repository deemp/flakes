{
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix/a20de23b925fd8264fd7fad6454652e142fd7f73";
    inputs.nixpkgs.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs";
  };
  outputs = x: { };
}
