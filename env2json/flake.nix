{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs: inputs.flakes.makeFlake {
    inputs = { inherit (inputs.flakes.all) nixpkgs; };
    perSystem = { inputs, system }:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
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
        packages = {
          default = envToJSONConverter;
        };
        lib = {
          inherit mkJSON;
        };
        tests = {
          mkJSONDemo = mkJSON ./app.env;
        };
      in
      {
        inherit packages lib tests;
      };
  };
}
