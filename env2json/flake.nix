{
  inputs = {
    # source-flake.url = path:../source-flake;
    source-flake.url = github:br4ch1st0chr0n3/flakes?dir=source-flake;
    nixpkgs.follows = "source-flake/nixpkgs";
    flake-utils.follows = "source-flake/flake-utils";
  };
  outputs =
    { self
    , source-flake
    , nixpkgs
    , flake-utils
    }: (flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      script = ./env-to-json.py;
      mkJSON = dotenvPath: pkgs.stdenv.mkDerivation {
        name = "make-json";
        buildInputs = [ pkgs.python310 ];
        unpackPhase = "true";
        installPhase = ''
          mkdir -p $out
          python ${script} ${dotenvPath} > $out/.json
        '';
      };
      envToJSONConverter =
        let
          name = "env-to-json-converter";
        in
        pkgs.stdenv.mkDerivation {
          inherit name;
          buildInputs = [ pkgs.python310 ];
          unpackPhase = "true";
          installPhase = ''
            mkdir -p $out/bin
            cp ${script} $out/bin/${name}
            chmod +x $out/bin/${name}
          '';
        };
    in
    {
      packages = {
        mkJSONDemo = mkJSON ./app.env;
        default = envToJSONConverter;
      };
      tools = {
        inherit mkJSON;
      };
    })) // { inherit (source-flake) formatter; };
}
