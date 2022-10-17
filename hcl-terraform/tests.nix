{ pkgs, system, drv-tools }:
let
  inherit (drv-tools.functions.${system})
    mkShellApp mkBin framedBrackets framedBrackets_ concatStringsNewline;
  inherit (pkgs.lib.attrsets) mapAttrsToList;
  inherit (import ./tf-tools.nix { inherit pkgs system drv-tools; }) writeFiles;

  testData = (import ./test-data.nix { inherit pkgs system; });

  tests = {
    testWriteDocsTFs = with testData; writeFiles [
      { hclExpr = docsMainTF; filePath = "docs/main.tf"; }
      { hclExpr = docsVariablesTF; filePath = "docs/variables.tf"; }
      { hclExpr = docsTfvarsTF; filePath = "docs/terraform.tfvars"; }
    ];

    testWriteDockerTFs = with testData; writeFiles [
      { hclExpr = mainTF; filePath = "docker/main.tf"; }
      { hclExpr = variablesTF; filePath = "docker/variables.tf"; }
      { hclExpr = tfvarsTF; filePath = "docker/terraform.tfvars"; }
    ];
  };

  runTests = mkShellApp {
    name = "run-tests";
    text = concatStringsNewline (
      mapAttrsToList
        (
          name: val: concatStringsNewline [
            (''printf '${framedBrackets_ "\n" "" name}' '')
            (mkBin val)
            (
              ''printf '${framedBrackets_ "\n" "\n\n" "test verdict : %s"}' \
                "$(if [[ $? == 0 ]]; then printf "ok"; else printf "not ok"; fi)"''
            )
          ]
        )
        tests
    );
  };
in
{
  inherit runTests;
} // tests
