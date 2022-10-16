{ pkgs, system, drv-tools }:
let
  inherit (drv-tools.functions.${system}) mkShellApp framedBrackets concatStringsNewline;
  inherit (pkgs.lib.strings) escapeShellArg concatMapStringsSep concatStringsSep;
  inherit (builtins) isList;

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
  writeTFs = dirsToFormat: data:
    assert isList dirsToFormat;
    mkShellApp {
      name = "write-tfs";
      text = concatStringsNewline
        [
          (concatMapStringsSep "\n" ({ hclExpr, tfPath }: "printf ${escapeShellArg "${hclExpr}"} > ${tfPath}.tf") data)
          (''printf '${framedBrackets "formatted files"}' '')
          (concatMapStringsSep "\n" (dir: "terraform fmt ${dir}") dirsToFormat)
        ];
      longDescription = ''
        Write `HCL` expressions into corresponding `$FILE_PATH.tf`-s
        and format the given directories.
        No need to supply the file extensions
      '';
    };

in
{
  inherit writeTF writeTFs;
}
