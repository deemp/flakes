{
  # keys are extension IDs (see https://code.visualstudio.com/docs/editor/extension-marketplace)
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
    # "editor.defaultFormatter" = "nwolverson.ide-purescript";
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

  multi-command = {
    "multiCommand.commands" = [
      {
        command = "multiCommand.addTypeHole";
        sequence = [
          {
            args =
              {
                snippet = "(\${TM_SELECTED_TEXT} :: ?_)";
              };
            command = "editor.action.insertSnippet";
          }
          {
            command = "cursorLeft";
          }
        ];
      }
      {
        command = "multiCommand.addTypeHoleAutoSelect";
        sequence = [
          {
            command = "editor.action.addSelectionToNextFindMatch";
          }
          {
            args = {
              command = "multiCommand.addTypeHole";
            };
            command = "extension.multiCommand.execute";
          }
        ];
      }
      {
        command = "multiCommand.removeTypeHole";
        sequence = [
          {
            "command" = "extension.selectParenthesisOuter";
          }
          {
            args = {
              snippet = "\${TM_SELECTED_TEXT/\\((.*) :: \\?_\\)/$1/}";
            };
            command = "editor.action.insertSnippet";
          }
        ];
      }
    ];
  };
}
