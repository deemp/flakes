# A set of VSCodium extensions
{ system, vscode-extensions, vscode-extensions-selected }:
let
  inherit (vscode-extensions.packages.${system}) vscode open-vsx;
  # my-extensions = vscode-extensions-selected.packages.${system}.vscode;
in
{
  haskell = {
    inherit (open-vsx.haskell) haskell;
    inherit (open-vsx.justusadam) language-haskell;
    inherit (vscode.visortelle) haskell-spotlight;
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
    inherit (open-vsx.github) vscode-pull-request-github;
    inherit (open-vsx.eamodio) gitlens;
    inherit (open-vsx.cschleiden) vscode-github-actions;
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
    inherit (open-vsx.gruntfuggly) todo-tree;
  };
  docker = {
    inherit (vscode.ms-vscode-remote) remote-containers;
    inherit (open-vsx.ms-azuretools) vscode-docker;
    inherit (open-vsx.exiasr) hadolint;
  };
  python = {
    inherit (open-vsx.donjayamanne) python-extension-pack;
    inherit (open-vsx.njpwerner) autodocstring;
    inherit (open-vsx.ms-python) python;
    inherit (open-vsx.samuelcolvin) jinjahtml;
    inherit (vscode.monosans) djlint;
    inherit (open-vsx.batisteo) vscode-django;
    inherit (open-vsx.kevinrose) vsc-python-indent;
    inherit (vscode.visualstudioexptteam) vscodeintellicode;
  };
  toml = {
    inherit (open-vsx.tamasfe) even-better-toml;
  };
  terraform = {
    inherit (open-vsx.hashicorp) terraform;
  };
  fish = {
    inherit (open-vsx.bmalehorn) vscode-fish;
  };
  sql = {
    inherit (vscode.mtxr) sqltools;
  };
}
