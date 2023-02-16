{
  # keys are extension IDs (see https://code.visualstudio.com/docs/editor/extension-marketplace)
  # or pre-defined names (workbench, editor)
  # in case of ID clashes, an extension setting should have a full name

  haskell = {
    "haskell.manageHLS" = "PATH";
    "[haskell]" = {
      "editor.defaultFormatter" = "haskell.haskell";
    };
    "haskell.formattingProvider" = "fourmolu";
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

  markdown-language-features = {
    "[markdown]" = {
      "editor.defaultFormatter" = "DavidAnson.vscode-markdownlint";
    };
  };

  markdown-all-in-one = {
    "markdown.extension.orderedList.autoRenumber" = false;
  };

  files = {
    "files.watcherExclude" = { "**/.spago/**" = true; };
    "files.refactoring.autoSave" = true;
    "files.autoSave" = "afterDelay";
    "files.associations" = {
      "*.html" = "html";
      "*.jinja" = "jinja-html";
    };
  };

  # enable IntelliSense (but not yet info on hover :( ) in jinja files
  emmet = { "emmet.includeLanguages" = { "jinja-html" = "html"; }; };

  # set your path instead of `front`
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
    "workbench.colorTheme" = "Monokai";
  };

  editor = {
    "editor.formatOnSave" = true;
    "editor.tabSize" = 2;
  };

  terminal = {
    "terminal.integrated.scrollback" = 100000;
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

  yaml = {
    "yaml.schemas" = {
      "https://json.schemastore.org/github-workflow.json" =
        "workflows/**/*.yml";
      "https://json.schemastore.org/github-action.json" =
        "actions/**/action.yml";
    };
  };

  json-language-features = {
    "[jsonc]" = {
      "editor.defaultFormatter" = "vscode.json-language-features";
    };
    "[json]" = {
      "editor.defaultFormatter" = "vscode.json-language-features";
    };
  };

  git = {
    "git.autofetch" = true;
    "git.autofetchPeriod" = 5;
  };

  nix-ide = {
    "nix.enableLanguageServer" = true;
    "nix.serverPath" = "rnix-lsp";
    "nix.formatterPath" = "nixpkgs-fmt";
  };

  python = {
    "python.formatting.provider" = "black";
    "python.linting.mypyCategorySeverity.error" = "Error";
    "python.linting.mypyEnabled" = true;
    "python.defaultInterpreterPath" = "\${workspaceFolder}/.venv/bin/python3";
  };

  errorlens = {
    "errorLens.enabledDiagnosticLevels" = [
      "info"
      "warning"
      "hint"
      "error"
    ];
    "errorLens.enabled" = true;
  };

  window = {
    "window.restoreWindows" = "none";
  };
}
