{
  # keys are extension IDs (see https://code.visualstudio.com/docs/editor/extension-marketplace)
  # or pre-defined names (workbench, editor)
  # in case of ID clashes, an extension setting should have a full name

  csv = {
    "files.associations" = {
      "*.dat" = "csv (pipe)";
      "*.csv" = "csv (semicolon)";
    };
  };

  editor = {
    "editor.formatOnSave" = true;
    "editor.tabSize" = 2;
  };

  # enable IntelliSense (but not yet info on hover :( ) in jinja files
  emmet = { "emmet.includeLanguages" = { "jinja-html" = "html"; }; };

  errorlens = {
    "errorLens.enabledDiagnosticLevels" = [
      "info"
      "warning"
      "hint"
      "error"
    ];
    "errorLens.enabled" = true;
  };

  files = {
    "files.watcherExclude" = { "**/.spago/**" = true; };
    "files.refactoring.autoSave" = true;
    "files.autoSave" = "afterDelay";
    "files.associations" = {
      "*.html" = "html";
      "*.jinja" = "jinja-html";
      "*.nix" = "nix";
      "*.py" = "python";
      "*.md" = "markdown";
    };
  };

  git = {
    "git.autofetch" = true;
    "git.autofetchPeriod" = 5;
  };

  gitlens = {
    "gitlens.codeLens.authors.enabled" = false;
    "gitlens.codeLens.enabled" = false;
    "gitlens.codeLens.recentChange.enabled" = false;
    "gitlens.currentLine.enabled" = false;
    "gitlens.currentLine.pullRequests.enabled" = false;
    "gitlens.hovers.currentLine.over" = "line";
    "gitlens.hovers.enabled" = false;
    "gitlens.statusBar.enabled" = false;
  };

  haskell = {
    "haskell.manageHLS" = "PATH";
    "[haskell]" = {
      "editor.defaultFormatter" = "haskell.haskell";
    };
    "haskell.formattingProvider" = "fourmolu";
  };

  # set your path instead of `front`
  ide-purescript = {
    "purescript.outputDirectory" = "./output";
    "purescript.packagePath" = "./";
    "purescript.sourcePath" = "./src";
    "purescript.formatter" = "purs-tidy";
  };

  json-language-features = {
    "[jsonc]" = {
      "editor.defaultFormatter" = "vscode.json-language-features";
    };
    "[json]" = {
      "editor.defaultFormatter" = "vscode.json-language-features";
    };
  };

  jupyter = {
    "jupyter.notebookFileRoot" = ''''${workspaceFolder}'';
  };


  markdown-all-in-one = {
    "markdown.extension.orderedList.autoRenumber" = false;
  };

  markdown-language-features = {
    "[markdown]" = {
      "editor.defaultFormatter" = "DavidAnson.vscode-markdownlint";
    };
  };

  nix-ide = {
    "nix.enableLanguageServer" = true;
    "nix.serverPath" = "rnix-lsp";
    "nix.formatterPath" = "nixpkgs-fmt";
  };

  prettier = {
    "[typescript]" = {
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
  };

  prettier-sql-vscode = {
    "Prettier-SQL.SQLFlavourOverride" = "postgresql";
    "Prettier-SQL.indentStyle" = "tabularLeft";
    "Prettier-SQL.keywordCase" = "upper";
    "Prettier-SQL.expressionWidth" = 42;
    "[sql]" = {
      "editor.defaultFormatter" = "inferrinizzard.prettier-sql-vscode";
    };
  };

  python = {
    "python.formatting.provider" = "black";
    "python.linting.mypyCategorySeverity.error" = "Error";
    "python.linting.mypyEnabled" = true;
  };

  rescript-vscode = {
    "rescript.settings.autoRunCodeAnalysis" = true;
    "rescript.settings.codeLens" = true;
    "rescript.settings.askToStartBuild" = true;
  };

  explorer = {
    "explorer.fileNesting.enabled" = true;
    "explorer.fileNesting.patterns" = {
      "*.res" = "\${capture}.mjs, \${capture}.js, \${capture}.cmi, \${capture}.cmt, \${capture}.cmj";
      "*.resi" = "\${capture}.res";
    };
  };

  terminal = {
    "terminal.integrated.scrollback" = 100000;
  };

  todo-tree = {
    "todo-tree.regex.regex" =
      let
        # https://github.com/Gruntfuggly/todo-tree/wiki/Configuration-Examples#haskell
        haskell = "((--\\s*($TAGS))|\\{-\\s($TAGS).*(\\n.*)*-})";
        javadoc = "(//|#|<!--|/\\*|^\\s*\\*)\\s*($TAGS)";
      in
      "${haskell}|${javadoc}";
    "todo-tree.filtering.excludeGlobs" = [
      "**/vendor/**"
      "**/node_modules/**"
      "**/dist/**"
      "**/bower_components/**"
      "**/build/**"
      "**/_output/**"
      "**/*.min.*"
      "**/*.map"
      "**/*.stack-work"
    ];
  };

  vscode-dhall-lsp-server = {
    "vscode-dhall-lsp-server.executable" = "dhall-lsp-server";
  };

  workbench = {
    "workbench.sideBar.location" = "right";
    "workbench.colorTheme" = "Monokai";
  };

  yaml = {
    "yaml.schemas" = {
      "https://json.schemastore.org/github-workflow.json" =
        "workflows/**/*.yml";
      "https://json.schemastore.org/github-action.json" =
        "actions/**/action.yml";
    };
  };
}
