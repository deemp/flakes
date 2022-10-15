{ pkgs, system, drv-tools }:
let
  inherit (drv-tools.functions.${system}) mkShellApp;
  inherit (pkgs.lib.strings) escapeShellArg concatMapStringsSep;

  writeTF = hclExpr: tfPath: mkShellApp {
    name = "write-tf";
    text = ''
      printf ${escapeShellArg "${hclExpr}"} > ${tfPath}.tf
      terraform fmt ${tfPath}.tf
    '';
    runtimeInputs = [ pkgs.terraform ];
    longDescription = ''
      Write an `HCL` expression into a given `$FILE_PATH.tf` file and format it.
      No need to supply the file extension
    '';
  };
  writeTFs = dirToFormat: data: mkShellApp {
    name = "write-tfs";
    text =
      (
        concatMapStringsSep "\n"
          (
            { hclExpr, tfPath }: "printf ${escapeShellArg "${hclExpr}"} > ${tfPath}.tf"
          )
          data
      ) + "\nterraform fmt ${dirToFormat}";
    longDescription = ''
      Write `HCL` expressions into corresponding `$FILE_PATH.tf`-s 
      and format the given directory.
      No need to supply the file extensions
    '';
  };

in
{
  inherit writeTF writeTFs;
}
