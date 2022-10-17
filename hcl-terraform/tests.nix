{ pkgs, system, drv-tools }:
let
  inherit (drv-tools.functions.${system})
    mkShellApp mkBin framedBrackets framedBrackets_ concatStringsNewline;
  inherit (pkgs.lib.attrsets) mapAttrsToList;
  inherit (import ./tf-tools.nix { inherit pkgs system drv-tools; }) writeFiles;

  testData = (import ./test-data.nix { inherit pkgs system; });

  tests = {
    write-docs = with testData; writeFiles [
      { expr = docsMain; filePath = "docs/main.tf"; }
      { expr = docsVariables; filePath = "docs/variables.tf"; }
      { expr = docsTfvars; filePath = "docs/terraform.tfvars"; }
    ];

    write-docker = with testData; writeFiles [
      { expr = dockerMain; filePath = "docker/main.tf"; }
      { expr = dockerVariables; filePath = "docker/variables.tf"; }
      { expr = dockerTfvars; filePath = "docker/terraform.tfvars"; }
    ];

    write-yandex-cloud = with testData; writeFiles [
      { expr = ycMain; filePath = "yc/main.tf"; }
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
