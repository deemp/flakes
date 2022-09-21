# A set of VSCodium extensions
{ system, nix-vscode-marketplace, vscodium-extensions }:
let
  inherit (nix-vscode-marketplace.packages.${system}) vscode open-vsx;
  my-extensions = vscodium-extensions.packages.${system}.vscode;
in
{
  haskell = {
    inherit (open-vsx.haskell) haskell;
    inherit (open-vsx.justusadam) language-haskell;
    inherit (my-extensions.visortelle) haskell-spotlight;
    inherit (open-vsx.redhat) vscode-yaml;
  };
  purescript = {
    inherit (open-vsx.nwolverson) ide-purescript language-purescript;
    inherit (open-vsx.dhall) dhall-lang vscode-dhall-lsp-server;
    inherit (my-extensions.br4ch1st0chr0n3) purs-keybindings;
    inherit (my-extensions.ryuta46) multi-command;
    inherit (my-extensions.chunsen) bracket-select;
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
  typescript = {
    inherit (open-vsx.ms-vscode) vscode-typescript-next;
  };
  markdown = {
    inherit (my-extensions.bierner)
      github-markdown-preview
      markdown-emoji
      markdown-checkbox
      markdown-yaml-preamble
      markdown-footnotes
      ;
  };
  misc = {
    inherit (open-vsx.usernamehw) errorlens;
    inherit (open-vsx.gruntfuggly) todo-tree;
  };
  docker = {
    inherit (my-extensions.ms-vscode-remote) remote-containers;
  };
  python = {
    inherit (open-vsx.donjayamanne) python-extension-pack;
    inherit (open-vsx.njpwerner) autodocstring;
    inherit (open-vsx.ms-python) python;
    inherit (open-vsx.wholroyd) jinja;
    inherit (open-vsx.batisteo) vscode-django;
    inherit (open-vsx.kevinrose) vsc-python-indent;
    inherit (my-extensions.visualstudioexptteam) vscodeintellicode;
  };
}
