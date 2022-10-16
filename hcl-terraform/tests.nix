{ pkgs, system, drv-tools }:
let
  inherit (drv-tools.functions.${system}) mkShellApp mkBin framedBrackets;
  inherit (pkgs.lib.attrsets) mapAttrsToList;
  inherit (pkgs.lib.strings) concatStringsSep;
  tfTools = import ./tf-tools.nix { inherit pkgs system drv-tools; };

  testData = (import ./test-data.nix { inherit pkgs system; });

  tests = {
    testWriteDocsTFs = with testData; tfTools.writeTFs [ "." ] [
      { hclExpr = docsMainTF; tfPath = "docs.main"; }
      { hclExpr = docsVariablesTF; tfPath = "docs.variable"; }
      { hclExpr = docsTfvarsTF; tfPath = "docs.tfvars"; }
    ];

    testWriteDockerTFs = with testData; tfTools.writeTFs [ "." ] [
      { hclExpr = mainTF; tfPath = "main"; }
      { hclExpr = variablesTF; tfPath = "variable"; }
      { hclExpr = tfvarsTF; tfPath = "tfvars"; }
    ];
  };

  runTests = mkShellApp {
    name = "run-tests";
    text = concatStringsSep "\n" (
      mapAttrsToList
        (
          name: val: concatStringsSep "\n" [
            (''printf '${framedBrackets name}' '')
            (mkBin val)
            (''printf '[ test verdict : '; if [[ $? == 0 ]]; then printf "ok"; else printf "not ok"; fi; printf ' ]\n\n' '')
          ]
        )
        tests
    );
  };
in
{
  inherit runTests;
} // tests
