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
  };
  typescript = {
    inherit (open-vsx.ms-vscode) vscode-typescript-next;
  };
  misc = {
    inherit (open-vsx.usernamehw) errorlens;
    inherit (open-vsx.gruntfuggly) todo-tree;
    inherit (vscode.bierner) github-markdown-preview;
  };
  docker = {
    inherit (my-extensions.ms-vscode-remote) remote-containers;
  };
}
