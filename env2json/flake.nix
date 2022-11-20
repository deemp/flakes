{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , ...
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
        default = envToJSONConverter;
      };
      functions = {
        inherit mkJSON;
      };
      tests = {
        mkJSONDemo = mkJSON ./app.env;
      };
    }));
}
