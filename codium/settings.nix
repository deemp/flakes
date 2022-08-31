{
  # keys are extension IDs (see https://code.visualstudio.com/docs/editor/extension-marketplace)
  # or pre-defined names (workbench, editor)
  # in case of ID clashes, an extension setting should have a full name


  haskell = {
    "haskell.serverExecutablePath" = "haskell-language-server";
    "haskell.manageHLS" = "PATH";
  };

  todo-tree = {
    "todo-tree.regex.regex" = "((--\\s*($TAGS))|\\{-\\s($TAGS).*(\\n.*)*-})";
  };

  files = {
    "files.watcherExclude" = {
      "**/.spago/**" = true;
    };
    "files.refactoring.autoSave" = true;
    "files.autoSave" = "afterDelay";
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

  gitlens = {
    "gitlens.currentLine.enabled" = false;
    "gitlens.hovers.currentLine.over" = "line";
    "gitlens.codeLens.enabled" = false;
    "gitlens.statusBar.enabled" = false;
  };

  git = {
    "git.autofetch" = true;
  };

  window = {
    "window.zoomLevel" = 1;
  };
}
