# A set of VSCodium extensions
{ system, pkgs, vscode-extensions, vscode-extensions-extra }:
let
  vscode-marketplace = vscode-extensions.extensions.${system}.vscode-marketplace;
  vscode-marketplace-extra = vscode-extensions-extra.extensions.${system}.vscode-marketplace;
  inherit (pkgs.lib.attrsets) mapAttrs' mapAttrsToList recursiveUpdate;
  mkExtensionsGroup = vscode-marketplace_: exts@{ ... }: builtins.foldl' recursiveUpdate { } (
    pkgs.lib.lists.flatten (
      mapAttrsToList
        (name: value:
          let value_ = if builtins.isList value then value else [ value ]; in
          map (ext: { ${ext} = vscode-marketplace_.${name}.${ext}; }) value_
        )
        exts));
  mkExtensions = vscode-marketplace_: mapAttrs' (x: y: { name = x; value = mkExtensionsGroup vscode-marketplace_ y; });
in 
pkgs.lib.attrsets.recursiveUpdate 
(
mkExtensions vscode-marketplace
{
  c-cpp = {
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
    eamodio = "gitlens";
    github = [ 
      "vscode-pull-request-github" 
      "vscode-github-actions" ];
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
  postgresql = {
    inferrinizzard = "prettier-sql-vscode";
  };
  prolog = {
    lilr = "swi-lsp";
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
  yaml = {
    redhat = "vscode-yaml";
  };
})

(mkExtensions vscode-marketplace-extra
{
  purescript = {
    br4ch1st0chr0n3 = "purs-keybindings";
  };
  postgresql = {
    cweijan = "vscode-postgresql-client2";
  };
})