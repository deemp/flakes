{
  description = "A flake for building Hello World";

  # pin nixpkgs
  inputs.nixpkgs.url = github:NixOS/nixpkgs/a8a557e233c30b60225fa5af41cc56f8409e4afb;

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
    in
    {
      packages.x86_64-linux.default =
        pkgs.stdenv.mkDerivation {
          buildInputs = [ pkgs.gcc ];
          name = "hello";
          src = ./.;
          buildPhase = "${pkgs.gcc}/bin/gcc -o hello hello.c";
          installPhase = "mkdir -p $out/bin; install -t $out/bin hello";
        };
    };
}
