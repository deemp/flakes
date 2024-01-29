let
  lib = import
    (
      let
        l = (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes.nixpkgs_2.locked;
        tar = fetchTarball {
          url = "https://github.com/nixos/nixpkgs/archive/${l.rev}.tar.gz";
          sha256 = l.narHash;
        };
      in
      "${tar}/lib"
    );
in
{
  # original inputs
  inputs
, # systems to generate outputs for
  systems ? [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ]
, # outputs per system
  perSystem ? _: { }
, # other outputs
  other ? { }
}:
lib.pipe systems [
  (map (sys: builtins.mapAttrs (_: value: { ${sys} = value; }) (perSystem sys)))
  (builtins.foldl' lib.recursiveUpdate other)
]
