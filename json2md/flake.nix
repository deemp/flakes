{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    dream2nix_.url = "github:deemp/flakes?dir=source-flake/dream2nix";
    dream2nix.follows = "dream2nix_/dream2nix";
    flake-utils.follows = "flake-utils_/flake-utils";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
  };
  outputs =
    { self
    , flake-utils
    , dream2nix
    , drv-tools
    , nixpkgs
    , ...
    }:
    let
      # json2md converts an input JSON into md and prints to stdout
      # Usage:
      # json2md input
      # Example:
      # json2md <(printf '{"h1": "Hello, world"}' )
      out1 =
        dream2nix.lib.makeFlakeOutputs
          {
            systems = flake-utils.lib.defaultSystems;
            config.projectRoot = ./.;
            source = ./.;
            settings = [
              {
                subsystemInfo.nodejs = 16;
              }
            ];
          };
      out2 = flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (builtins) isString baseNameOf dirOf;
          inherit (drv-tools.functions.${system})
            mkBin mkBinName writeJSON withMan mkShellApp
            framedBrackets mkShellApps;
          inherit (drv-tools.configs.${system}) man;
          nix2mdSilent = outPath: nixExpr@{ ... }:
            assert builtins.isString outPath;
            let
              outName = baseNameOf outPath;
              outDir = dirOf outPath;
              tmpJSON = "${outName}.tmp.json";
              writeTmpJSON = writeJSON "pre-md" "./${tmpJSON}" nixExpr;
              json2md = out1.packages.${system}.default;
              mdlint = pkgs.nodePackages.markdownlint-cli2;
              name = "nix-to-md";
            in
            mkShellApp {
              name = name;
              runtimeInputs = [
                pkgs.nodejs-16_x
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

          nix2md = outPath: nixExpr@{ ... }:
            assert builtins.isString outPath;
            withMan
              (
                let
                  nix2md_ = nix2mdSilent outPath nixExpr;
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
              )
              (x: ''
                ${man.DESCRIPTION}
                ${x.meta.description}.
              '');

          test = mkShellApps {
            testNix2md = {
              text = "${mkBin (nix2md "tmp/some.md" { h1 = "Some text"; })}";
            };
          };

          json2md = out1.packages.${system}.default;
        in
        {
          packages = {
            inherit json2md;
          } // test;
          functions = {
            inherit nix2mdSilent nix2md;
          };
          devShells.default = pkgs.mkShell {
            buildInputs = [ json2md ];
          };
        }
      );
    in
    out2
  ;
}
