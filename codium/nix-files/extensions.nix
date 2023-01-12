# A set of VSCodium extensions
{ system, vscode-extensions, vscode-extensions-selected }:
let
  inherit (vscode-extensions.packages.${system}) vscode open-vsx;
in
{
  haskell = {
    inherit (vscode.haskell) haskell;
    inherit (vscode.justusadam) language-haskell;
    inherit (vscode.visortelle) haskell-spotlight;
    inherit (vscode.redhat) vscode-yaml;
  };
  yaml = {
    inherit (vscode.redhat) vscode-yaml;
  };
  purescript = {
    inherit (open-vsx.nwolverson) ide-purescript language-purescript;
    inherit (open-vsx.dhall) dhall-lang vscode-dhall-lsp-server;
    inherit (vscode.br4ch1st0chr0n3) purs-keybindings;
    inherit (vscode.ryuta46) multi-command;
    inherit (vscode.chunsen) bracket-select;
  };
  nix = {
    inherit (open-vsx.mkhl) direnv;
    inherit (open-vsx.jnoortheen) nix-ide;
  };
  github = {
    inherit (vscode.github) vscode-pull-request-github;
    inherit (vscode.eamodio) gitlens;
    inherit (vscode.cschleiden) vscode-github-actions;
    inherit (vscode.redhat) vscode-yaml;
  };
  typescript = { inherit (open-vsx.ms-vscode) vscode-typescript-next; };
  markdown = {
    inherit (vscode.bierner)
      github-markdown-preview markdown-emoji markdown-checkbox
      markdown-yaml-preamble markdown-footnotes markdown-preview-github-styles;
    inherit (open-vsx.davidanson) vscode-markdownlint;
    inherit (open-vsx.yzhang) markdown-all-in-one;
  };
  misc = {
    inherit (open-vsx.usernamehw) errorlens;
    inherit (vscode.gruntfuggly) todo-tree;
    inherit (open-vsx.mkhl) direnv;
  };
  docker = {
    inherit (vscode.ms-vscode-remote) remote-containers;
    inherit (open-vsx.ms-azuretools) vscode-docker;
    inherit (open-vsx.exiasr) hadolint;
  };
  # c-cpp = {
  #   inherit (vscode.ms-vscode) cpptools-themes cmake-tools cpptools;
  # };
  python = {
    inherit (vscode.donjayamanne) python-extension-pack;
    inherit (vscode.njpwerner) autodocstring;
    inherit (vscode.ms-python) python;
    inherit (open-vsx.samuelcolvin) jinjahtml;
    inherit (vscode.monosans) djlint;
    inherit (vscode.batisteo) vscode-django;
    inherit (vscode.kevinrose) vsc-python-indent;
    inherit (vscode.visualstudioexptteam) vscodeintellicode;
  };
  kubernetes = {
    inherit (vscode.ipedrazas) kubernetes-snippets;
    inherit (vscode.ms-kubernetes-tools) vscode-kubernetes-tools;
    inherit (vscode.redhat) vscode-yaml;
  };
  toml = {
    inherit (open-vsx.tamasfe) even-better-toml;
  };
  terraform = {
    inherit (vscode.hashicorp) terraform;
  };
  fish = {
    inherit (open-vsx.bmalehorn) vscode-fish;
  };
  postgresql = {
    inherit (vscode.cweijan) vscode-postgresql-client2;
    inherit (vscode.inferrinizzard) prettier-sql-vscode;
  };
  sql = {
    inherit (vscode.mtxr) sqltools;
    inherit (vscode.inferrinizzard) prettier-sql-vscode;
  };
}
