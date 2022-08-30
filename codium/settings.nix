{
  # keys are extension IDs (see https://code.visualstudio.com/docs/editor/extension-marketplace)
  # or pre-defined names (workbench, editor)
  # in case of ID clashes, an extension setting should have a full name

  haskell = {
    "haskell.serverExecutablePath" = "haskell-language-server";
  };

  todo-tree = {
    "todo-tree.regex.regex" = "((--\\s*($TAGS))|\\{-\\s($TAGS).*(\\n.*)*-})";
  };

  files = {
    "files.watcherExclude" = {
      "**/.spago/**" = true;
    };
    "files.refactoring.autoSave" = true;
  };

  ide-purescript = {
    "purescript.outputDirectory" = "./front/output/";
    "purescript.packagePath" = "./front";
    "purescript.sourcePath" = "./front/src";
    "purescript.formatter" = "purs-tidy";
  };

  vscode-dhall-lsp-server = {
    "vscode-dhall-lsp-server.executable" = "dhall-lsp-server";
  };

  workbench = {
    "workbench.sideBar.location" = "right";
  };

  editor = {
    "editor.formatOnSave" = true;
  };
}
