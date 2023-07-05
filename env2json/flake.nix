{
  inputs = {
    flakes = {
      url = "github:deemp/flakes";
    };
  };

  outputs =
    inputsTop:
    let
      inputs_ =
        let flakes = inputsTop.flakes.flakes; in
        {
          inherit (flakes.source-flake) flake-utils nixpkgs;
        };

      outputs = flake { } // {
        inherit flake;
        inputs = inputs_;
      };

      flake =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
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
        });
    in
    outputs;
}
