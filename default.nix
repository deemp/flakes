let
  reflex-platform = import ./reflex-platform {};
  project = reflex-platform.project ({ pkgs, ... }: {
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

    passthru = rec {
      static = pkgs.stdenv.mkDerivation rec {
        name = "haskell-webchat-static";
        src = pkgs.lib.cleanSource ./static;
        buildInputs = [ project.ghcjs.webchat-frontend ];
        nativeBuildInputs = [ pkgs.closurecompiler ];
        jsexeDir = "${project.ghcjs.webchat-frontend}/bin/webchat-frontend.jsexe";
        buildPhase = ''
          closure-compiler ${jsexeDir}/all.js --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --externs=${jsexeDir}/all.js.externs > all.min.js
        '';
        installPhase = ''
          mkdir -p $out/static
          cp -r * $out/static
        '';
      };

      debug = (pkgs.writeScriptBin "serve" ''
        #!${pkgs.stdenv.shell}
        ${project.ghc.webchat-backend}/bin/webchat-backend ${project.ghcjs.webchat-frontend}/bin/webchat-frontend.jsexe/ ''${1:-$PORT}
        '');

      release = (pkgs.writeScriptBin "serve" ''
        #!${pkgs.stdenv.shell}
        ${project.ghc.webchat-backend}/bin/webchat-backend ${static}/static ''${1:-$PORT}
        '');

      docker-image = pkgs.dockerTools.buildLayeredImage {
        name = "ptrhlm/haskell-webchat";
        tag = "latest";
        contents = [pkgs.bash release];
        config = {
          Cmd = ["serve" "80"];
          ExposedPorts = {
            "80/tcp" = {};
          };
        };
        maxLayers = 32;
      };

      heroku-image = pkgs.dockerTools.buildLayeredImage {
        name = "registry.heroku.com/haskell-webchat/web";
        tag = "latest";
        contents = [pkgs.bash release];
        config = {
          Cmd = ["${release}/bin/serve"];
          Entrypoint = [];
        };
        maxLayers = 120;
      };
    };
  });
in
  project
