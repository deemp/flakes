{
  inputs = {
    nixpkgs_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (pkgs.lib.attrsets) recursiveUpdate;
      inherit (builtins) foldl' attrValues mapAttrs attrNames readDir map;
      inherit (pkgs.lib.strings) concatStringsSep concatMapStringsSep;
      # if a set's attribute values are all sets, merge these values recursively
      # Note that the precedence order is undefined, so it's better to 
      # have unique values at each set level
      # Examples:
      # mergeValues {a = {b = 1;}; c = {d = 2;};} => {b = 1; d = 2;}
      mergeValues = set@{ ... }:
        foldl' recursiveUpdate { } (attrValues set);

      # a convenience function that flattens a set with set attribute values
      # toList {a = {b = 1;}; c = {d = 2;};} => [1 2]
      toList = x: attrValues (mergeValues x);

      # make shell apps
      # arg should be a set of sets of inputs
      mkShellApps = appsInputs@{ ... }: mapAttrs (name: value: mkShellApp (value // { inherit name; })) appsInputs;

      runFishScript = { name, fishScriptPath, runtimeInputs ? [ ], text ? "" }: mkShellApp {
        inherit name;
        runtimeInputs = runtimeInputs ++ [ pkgs.fish pkgs.jq ];
        text =
          let CURRENT_SYSTEM = "CURRENT_SYSTEM"; in
          ''
            export CURRENT_SYSTEM=${system}
            
            ${text}

            fish ${fishScriptPath}
          '';
        longDescription = ''
          ${DESCRIPTION}
          run a *fish* script at *${fishScriptPath}*
        '';
      };

      # read something in a directory using the builtin function
      readXs = dir: type: attrNames (
        pkgs.lib.attrsets.filterAttrs (name_: type_: type_ == type) (readDir dir)
      );

      readFiles = dir: readXs dir "regular";
      readDirectories = dir: readXs dir "directory";
      readSymlinks = dir: readXs dir "symlink";

      # assuming that a `name` of a program coincides with its main executable's name
      mkBin = drv@{ name, ... }: "${drv}/bin/${name}";

      # same as mkBin, but need to provide the necessary executable name
      mkBinName = drv@{ name, ... }: name_: "${drv}/bin/${name_}";

      # frame a text with newlines
      framedNewlines = framed_ "\n\n" "\n\n";
      framed_ = pref: suff: txt: ''${pref}${txt}${suff}'';

      # frame a text with square brackets and newlines
      framedBrackets = framedBrackets_ "\n\n" "\n\n";
      framedBrackets_ = pref: suff: framed_ "${pref}[ " " ]${suff}";

      # concat strings and separate them by a newline character
      concatStringsNewline = concatStringsSep "\n";

      # concatMap strings and separate them by a newline character
      concatMapStringsNewline = concatMapStringsSep "\n";

      # ignore shellcheck when writing a shell application
      mkShellApp = args@{ name, text, runtimeInputs ? [ ], longDescription ? "", description ? "" }:
        let
          script =
            (pkgs.lib.meta.addMetaAttrs
              { inherit longDescription description; }
              (
                pkgs.writeShellApplication ({ inherit name text; } // {
                  runtimeInputs = pkgs.lib.lists.flatten (args.runtimeInputs or [ ]);
                  checkPhase = "";
                })));
          man = ''
            ---
            title: ${name}
            section: 1
            header: User Manual
            ---
            ${longDescription}
          '';
          manPath = "$out/share/man/man1";
        in
        pkgs.symlinkJoin {
          inherit name;
          paths = [ script ];
          nativeBuildInputs = [ pkgs.pandoc ];
          postBuild = ''
            mkdir -p ${manPath}
            cat <<EOT > $out/${name}.1.md 
            ${man}
            EOT
            pandoc $out/${name}.1.md -st man -o ${manPath}/${name}.1
            rm $out/${name}.1.md
          '';
        };

      withAttrs = drv: attrSet: pkgs.lib.attrsets.recursiveUpdate drv attrSet;
      withMeta = drv: meta: withAttrs drv { inherit meta; };
      withLongDescription = drv: longDescription: withAttrs drv { meta = { inherit longDescription; }; };

      # man headings
      NAME = "# NAME";
      DESCRIPTION = "# DESCRIPTION";
      EXAMPLES = "# EXAMPLES";
      NOTES = "# NOTES";

      # for code blocks in man
      indentStrings4 = indentStrings_ 4;
      indentStrings8 = indentStrings_ 8;
      indentStrings_ = n: y: "\n" + (concatMapStringsSep "\n" (x: (applyN n (s: " " + s) "") + x) y) + "\n";

      # mkLongDescription = description: 
      # String -> String -> Set -> IO ()
      writeJSON = name: path: dataNix:
        let
          dataJSON = builtins.toJSON dataNix;
          name_ = "write-${name}-json";
          dir = builtins.dirOf path;
          file = builtins.baseNameOf path;
        in
        mkShellApp {
          name = name_;
          runtimeInputs = [ pkgs.python310 ];
          text = ''
            mkdir -p ${dir}
            printf "%s" ${
              pkgs.lib.escapeShellArg dataJSON
            } | python -m json.tool > ${path}
            printf "${framedBrackets "ok %s"}" "${name_}"
          '';
          longDescription = ''
            ${DESCRIPTION}
            Write a given **Nix** expression as **JSON** into a **path**
          '';
        };

      # use when need to generate settings.json etc.
      json2nix = mkShellApp rec {
        name = "json2nix";
        runtimeInputs = [ pkgs.nixpkgs-fmt ];
        text = ''
          json_path=$1
          nix_path=$2
          nix eval --impure --expr "with builtins; fromJSON (readFile ./$json_path)" > $nix_path
          sed -i -E "s/(\[|\{)/\1\n/g" $nix_path
          nixpkgs-fmt $nix_path
        '';
        longDescription = ''
          ${NAME}
          **${name}** - Convert *.json* to *.nix*

          ${EXAMPLES}
          **json2nix .vscode/settings.json my-settings.nix**
          :   Convert exising settings.json into a nix file
              ${indentStrings8 [ "a" "b"] }
        '';
      };

      runInEachDir = args@{ dirs, command, name, preMessage ? "", message ? "", postMessage ? "", runtimeInputs ? [ ], longDescription ? "" }:
        let dirs_ = pkgs.lib.lists.flatten dirs; in
        (mkShellApp {
          name = "${name}-in-each-dir";
          inherit runtimeInputs;
          text =
            let INITIAL_CWD = "INITIAL_CWD";
            in
            ''
              ${INITIAL_CWD}=$PWD
              printf "%s" '${preMessage}'
            '' +
            builtins.concatStringsSep "\n"
              (map
                (dir: ''
                  printf "${framedBrackets "${if message == "" then name else message} : %s"}" "${"$" + INITIAL_CWD}/${dir}"

                  cd ${"$" + INITIAL_CWD}/${dir}
            
                  ${command}
                '')
                dirs_) +
            ''
              printf "%s" '${postMessage}'
            '';
          longDescription = ''
            ${longDescription}

            ${NOTES}
            The directories relative to **CWD** are:

            ${indentStrings4 dirs_}
          '';
        });

      # apply an `op` `cnt` times to the initial value `ini` to get `res`
      # initially, `res` = `ini`
      applyN = cnt: op: res: (if cnt > 0 then applyN (cnt - 1) op (op res) else res);
    in
    {
      packages = {
        inherit
          json2nix
          ;
      };
      functions = {
        inherit
          applyN
          concatMapStringsNewline
          concatStringsNewline
          indentStrings4
          indentStrings8
          indentStrings_
          framed_
          framedBrackets
          framedBrackets_
          framedNewlines
          mergeValues
          mkBin
          mkBinName
          mkShellApp
          mkShellApps
          readDirectories
          readFiles
          readSymlinks
          readXs
          runFishScript
          runInEachDir
          toList
          withAttrs
          withLongDescription
          withMeta
          writeJSON
          ;
      };

      configs.man = {
        inherit NAME DESCRIPTION EXAMPLES NOTES;
      };

      # tests 
      devShells.default = pkgs.mkShell {
        name = "default";
        buildInputs = [ pkgs.tree json2nix pkgs.fish ];
      };

      tests = {
        t = readFiles ./.;
      };
    });
}
