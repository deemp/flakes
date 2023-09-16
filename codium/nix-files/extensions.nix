# A set of VSCodium extensions
{ system, pkgs, nix-vscode-extensions }:
let
  vscode-marketplace = nix-vscode-extensions.extensions.${system}.vscode-marketplace;
  vscode-marketplace-release = nix-vscode-extensions.extensions.${system}.vscode-marketplace-release;
  open-vsx = nix-vscode-extensions.extensions.${system}.open-vsx;
  inherit (pkgs.lib.attrsets) mapAttrs' mapAttrsToList recursiveUpdate;
  mkExtensionsGroup = extensionSet: exts@{ ... }: builtins.foldl' recursiveUpdate { } (
    pkgs.lib.lists.flatten (
      mapAttrsToList
        (name: value:
          let value_ = if builtins.isList value then value else [ value ]; in
          map (ext: { ${ext} = extensionSet.${name}.${ext}; }) value_
        )
        exts));
  mkExtensions = extensionSet: mapAttrs' (x: y: { name = x; value = mkExtensionsGroup extensionSet y; });
in
__foldl' pkgs.lib.attrsets.recursiveUpdate { }
  [
    (
      mkExtensions vscode-marketplace
        {
          cpp = {
            ms-vscode = [
              "cmake-tools"
              "cpptools-themes"
              "cpptools"
            ];
          };
          csv = {
            "mechatroner" = "rainbow-csv";
          };
          docker = {
            exiasr = "hadolint";
            ms-azuretools = "vscode-docker";
            ms-vscode-remote = "remote-containers";
          };
          fish = {
            bmalehorn = "vscode-fish";
          };
          github = {
            github = [
              "vscode-pull-request-github"
              "vscode-github-actions"
            ];
            redhat = "vscode-yaml";
          };
          haskell = {
            haskell = "haskell";
            justusadam = "language-haskell";
            redhat = "vscode-yaml";
            visortelle = "haskell-spotlight";
          };
          jupyter = {
            ms-toolsai = [
              "jupyter"
              "jupyter-keymap"
              "vscode-jupyter-slideshow"
              "vscode-jupyter-cell-tags"
            ];
          };
          kubernetes = {
            ipedrazas = "kubernetes-snippets";
            ms-kubernetes-tools = "vscode-kubernetes-tools";
            redhat = "vscode-yaml";
          };
          markdown = {
            bierner =
              [
                "github-markdown-preview"
                "markdown-checkbox"
                "markdown-emoji"
                "markdown-footnotes"
                "markdown-preview-github-styles"
                "markdown-yaml-preamble"
              ];
            davidanson = "vscode-markdownlint";
            yzhang = "markdown-all-in-one";
          };
          misc = {
            gruntfuggly = "todo-tree";
            mkhl = "direnv";
            usernamehw = "errorlens";
          };
          nix = {
            jnoortheen = "nix-ide";
            mkhl = "direnv";
          };
          pdf = {
            tomoki1207 = "pdf";
          };
          postgresql = {
            inferrinizzard = "prettier-sql-vscode";
          };
          prolog = {
            lilr = "swi-lsp";
            jeff-hykin = "better-prolog-syntax";
          };
          purescript = {
            chunsen = "bracket-select";
            dhall = [
              "dhall-lang"
              "vscode-dhall-lsp-server"
            ];
            nwolverson = [
              "ide-purescript"
              "language-purescript"
            ];
            ryuta46 = "multi-command";
            br4ch1st0chr0n3 = "purs-keybindings";
          };
          python = {
            batisteo = "vscode-django";
            donjayamanne = "python-extension-pack";
            kevinrose = "vsc-python-indent";
            monosans = "djlint";
            ms-python = "python";
            njpwerner = "autodocstring";
            samuelcolvin = "jinjahtml";
            visualstudioexptteam = "vscodeintellicode";
          };
          rescript = {
            chenglou92 = "rescript-vscode";
          };
          rust = {
            rust-lang = "rust-analyzer";
          };
          scala = {
            scalameta = "metals";
            scala-lang = "scala";
          };
          sql = {
            inferrinizzard = "prettier-sql-vscode";
            mtxr = "sqltools";
          };
          terraform = {
            hashicorp = "terraform";
          };
          toml = {
            tamasfe = "even-better-toml";
          };
          typescript = {
            ms-vscode = "vscode-typescript-next";
            esbenp = "prettier-vscode";
          };
          vlang = {
            vlanguage = "vscode-vlang";
          };
          yaml = {
            redhat = "vscode-yaml";
          };
        }
    )
    (mkExtensions vscode-marketplace-release {
      github = {
        eamodio = "gitlens";
      };
    })
    (mkExtensions open-vsx
      {
        postgresql = {
          cweijan = "vscode-postgresql-client2";
        };
      })

  ]
