# A set of VSCodium extensions
{ system, pkgs, vscode-extensions }:
let
  inherit (vscode-extensions.extensions.${system}) vscode;
  inherit (pkgs.lib.attrsets) mapAttrs' mapAttrsToList recursiveUpdate;
  mkExtensionsGroup = exts@{ ... }: builtins.foldl' recursiveUpdate { } (
    pkgs.lib.lists.flatten (
      mapAttrsToList
        (name: value:
          let value_ = if builtins.isList value then value else [ value ]; in
          map (ext: { ${ext} = vscode.${name}.${ext}; }) value_
        )
        exts));
  mkExtensions = mapAttrs' (x: y: { name = x; value = mkExtensionsGroup y; });
in
mkExtensions
{
  haskell = {
    haskell = "haskell";
    justusadam = "language-haskell";
    visortelle = "haskell-spotlight";
    redhat = "vscode-yaml";
  };
  yaml = {
    redhat = "vscode-yaml";
  };
  purescript = {
    nwolverson = [
      "ide-purescript"
      "language-purescript"
    ];
    dhall = [
      "dhall-lang"
      "vscode-dhall-lsp-server"
    ];
    br4ch1st0chr0n3 = "purs-keybindings";
    ryuta46 = "multi-command";
    chunsen = "bracket-select";
  };
  nix = {
    mkhl = "direnv";
    jnoortheen = "nix-ide";
  };
  github = {
    github = "vscode-pull-request-github";
    eamodio = "gitlens";
    cschleiden = "vscode-github-actions";
    redhat = "vscode-yaml";
  };
  typescript = {
    ms-vscode = "vscode-typescript-next";
  };
  markdown = {
    bierner =
      [
        "github-markdown-preview"
        "markdown-emoji"
        "markdown-checkbox"
        "markdown-yaml-preamble"
        "markdown-footnotes"
        "markdown-preview-github-styles"
      ];
    davidanson = "vscode-markdownlint";
    yzhang = "markdown-all-in-one";
  };
  misc = {
    usernamehw = "errorlens";
    gruntfuggly = "todo-tree";
    mkhl = "direnv";
  };
  docker = {
    ms-vscode-remote = "remote-containers";
    ms-azuretools = "vscode-docker";
    exiasr = "hadolint";
  };
  c-cpp = {
    ms-vscode = [
      "cpptools-themes"
      "cmake-tools"
      "cpptools"
    ];
  };
  python = {
    donjayamanne = "python-extension-pack";
    njpwerner = "autodocstring";
    ms-python = "python";
    samuelcolvin = "jinjahtml";
    monosans = "djlint";
    batisteo = "vscode-django";
    kevinrose = "vsc-python-indent";
    visualstudioexptteam = "vscodeintellicode";
  };
  kubernetes = {
    ipedrazas = "kubernetes-snippets";
    ms-kubernetes-tools = "vscode-kubernetes-tools";
    redhat = "vscode-yaml";
  };
  toml = {
    tamasfe = "even-better-toml";
  };
  terraform = {
      hashicorp = "terraform";
  };
  fish = {
    bmalehorn = "vscode-fish";
  };
  postgresql = {
    cweijan = "vscode-postgresql-client2";
    inferrinizzard = "prettier-sql-vscode";
  };
  sql = {
    mtxr = "sqltools";
    inferrinizzard = "prettier-sql-vscode";
  };
}
