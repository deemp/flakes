{ pkgs, system, drv-tools }:
let
  inherit (drv-tools.functions.${system}) mkShellApp framedBrackets concatStringsNewline;
  inherit (pkgs.lib.strings) escapeShellArg concatMapStringsSep;
  inherit (pkgs.lib.lists) unique;
  inherit (builtins) isList dirOf map;

  writeTF = writeFile ".tf";
  writeFile = extension: expr: filePath: mkShellApp {
    name = "hcl-write-file";
    text = ''
      printf ${escapeShellArg "${expr}"} > ${filePath}${extension}
      terraform fmt ${filePath}${extension}
    '';
    runtimeInputs = [ pkgs.terraform ];
    longDescription = ''
      Write an `HCL` expression into `${filePath}${extension}` and format it.
    '';
  };

  writeTfvars = writeFiles_ ".auto.tfvars";
  writeTFs = writeFiles_ ".tf";
  writeFiles = writeFiles_ "";
  writeFiles_ = extension: data:
    assert isList data;
    let
      dirs = unique (map ({ filePath, ... }: dirOf filePath) data);
      f = concatMapStringsSep "\n";
    in
    mkShellApp {
      name = "hcl-write-files";
      text = concatStringsNewline
        [
          (f (dir: "mkdir -p '${dir}'") dirs)
          (f ({ expr, filePath }: "printf ${escapeShellArg "${expr}"} > '${filePath}${extension}'") data)
          ("printf '${framedBrackets "written files"}'")
          (f ({ filePath, ... }: "printf ${escapeShellArg "${filePath}\n"}") data)
          ("printf '${framedBrackets "formatted files"}'")
          (f (dir: "terraform fmt ${dir}") dirs)
        ];
      longDescription = ''
        Write `HCL` expressions into corresponding `$FILE_PATH${extension}`-s
        and format the parent directories of these files.
      '';
    };
in
{
  inherit writeTF writeTFs writeTfvars writeFiles_ writeFiles;
}
