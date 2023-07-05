{
  inputs.flakes.url = "github:deemp/flakes";

  outputs =
    inputs:
    let
      inputs_ =
        let flakes = inputs.flakes.flakes; in
        {
          inherit (flakes.source-flake) flake-utils nixpkgs;
          inherit (flakes) drv-tools;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.drv-tools.lib.${system})
            mkBin mkBinName writeJSON withMan mkShellApp
            framedBrackets mkShellApps man;
          inherit (builtins) isString baseNameOf dirOf;

          json2md = pkgs.buildNpmPackage rec {
            buildInputs = [
              pkgs.nodejs_18
            ];

            pname = "json2md";
            version = "2.0.0";

            src = ./.;

            npmDepsHash = "sha256-WjGhOEqL6Zg5tbBHgStr10ROcedJnkWCf5qiDwHOOp8=";
          };

          nix2mdSilent = outPath: dataNix:
            assert builtins.isString outPath;
            assert builtins.isList dataNix;
            let
              outName = baseNameOf outPath;
              outDir = dirOf outPath;
              tmpJSON = "${outName}.tmp.json";
              writeTmpJSON = writeJSON "pre-md" "./${tmpJSON}" dataNix;
              # json2md = out1.packages.${system}.default;
              mdlint = pkgs.nodePackages.markdownlint-cli2;
              name = "nix-to-md";
            in
            mkShellApp {
              name = name;
              runtimeInputs = [
                pkgs.nodejs_18
                writeTmpJSON
                json2md
                mdlint
              ];
              text = ''
                ${mkBin writeTmpJSON}
                mkdir -p ${outDir}
                ${mkBinName json2md "json2md"} ${tmpJSON} > ${outPath}
                ${mkBinName mdlint "markdownlint-cli2-fix"} ${outPath} || echo ""
                rm ${tmpJSON}
              '';
            };

          nix2md = outPath: dataNix:
            assert builtins.isString outPath;
            assert builtins.isList dataNix;
            let
              nix2md_ = nix2mdSilent outPath dataNix;
              name = nix2md_.name;
            in
            mkShellApp {
              name = name;
              text = ''
                ${mkBin nix2md_}
                printf "${framedBrackets "%s"}" "ok ${name}"
              '';
              description = "Convert a Nix expression to ${outPath}";
            }
          ;

          test = mkShellApps {
            testNix2md = {
              text = "${mkBin (nix2md "tmp/some.md" [{ h1 = "Some text"; }])}";
            };
          };
        in
        {
          packages = {
            inherit json2md;
          } // test;
          lib = {
            inherit nix2mdSilent nix2md;
          };
          devShells.default = pkgs.mkShell {
            buildInputs = [ json2md ] ++ builtins.attrValues test;
          };
        }
        );
    in
    outputs;
}
