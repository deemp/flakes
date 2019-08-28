(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    webchat-common = ./common;
    webchat-backend = ./backend;
    webchat-frontend = ./frontend;
  };

  shells = {
    ghc = ["webchat-common" "webchat-backend" "webchat-frontend"];
    ghcjs = ["webchat-common" "webchat-frontend"];
  };
  shellToolOverrides = ghc: super: {
    inherit (ghc) ghcid;
  };
})
