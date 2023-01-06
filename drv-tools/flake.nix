{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , ...
    }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (pkgs.lib.lists) flatten;
      inherit (pkgs.lib.attrsets)
        recursiveUpdate filterAttrs genAttrs mapAttrsToList;
      inherit (builtins)
        foldl' attrValues mapAttrs attrNames readDir map toString
        isString isAttrs dirOf baseNameOf toJSON hasAttr listToAttrs;
      inherit (pkgs.lib.strings)
        concatStringsSep concatMapStringsSep
        removePrefix removeSuffix;
      inherit (pkgs.lib) escapeShellArg id;
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

      # generate an attrset from a list of attrNames where attrName = attrValue
      genAttrsId = list: genAttrs list pkgs.lib.id;

      # List -> Set
      # Generate a set from a list of sets with all keys renamed
      # in the order they go in that list
      ord_ = list:
        let ordered =
          builtins.foldl'
            (x: y:
              let
                yAttrs = mapAttrsToList
                  (name: value: {
                    name = "_${toString x.cnt}_${name}";
                    inherit value;
                  })
                  y;
                yNew = listToAttrs yAttrs;
                yKeys =
                  mapAttrs
                    (name: value: "_${toString x.cnt}_${name}")
                    y;
              in
              {
                cnt = x.cnt + 1;
                acc = x.acc // yNew;
                keys = x.keys // yKeys;
              }
            )
            { cnt = 1; acc = { }; keys = { }; }
            list;
        in ordered;

      # Get just the ordered set
      ord = list: (ord_ list).acc;

      # make shell apps
      # arg should be a set of sets of inputs
      mkShellApps = appsInputs@{ ... }: mapAttrs (name: value: mkShellApp (value // { inherit name; })) appsInputs;

      runFishScript =
        { name
        , fishScriptPath
        , runtimeInputs ? [ ]
        , text ? ""
        , description ? ''Run a `fish` script at `${fishScriptPath}`''
        , longDescription ? ''
            ${man.DESCRIPTION}
            ${description}
          ''
        }: mkShellApp {
          inherit name;
          runtimeInputs = runtimeInputs ++ [ pkgs.fish pkgs.jq ];
          text =
            let CURRENT_SYSTEM = "CURRENT_SYSTEM"; in
            ''
              export CURRENT_SYSTEM=${system}
            
              ${text}

              fish ${fishScriptPath}
            '';
          inherit description longDescription;
        };

      # read something in a directory using the builtin function
      readXs = dir: type: attrNames (
        filterAttrs (name_: type_: type_ == type) (readDir dir)
      );

      readFiles = dir: readXs dir "regular";
      readDirectories = dir: readXs dir "directory";
      readSymlinks = dir: readXs dir "symlink";

      # assuming that a `name` of a program coincides with its main executable's name
      mkBin = drv@{ pname, ... }: "${drv}/bin/${pname}";

      # same as mkBin, but need to provide the necessary executable name
      mkBinName = drv@{ pname, ... }: name_: "${drv}/bin/${name_}";

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

      # man headings
      man = {
        NAME = "# NAME";
        SYNOPSYS = "# SYNOPSYS";
        DESCRIPTION = "# DESCRIPTION";
        EXAMPLES = "# EXAMPLES";
        NOTES = "# NOTES";
      };

      # ignore shellcheck when writing a shell application
      mkShellApp =
        args@{ name
        , text
        , runtimeInputs ? [ ]
        , description ? "no description provided"
        , longDescription ? ''
            ${man.DESCRIPTION}
            ${description}
          ''
        }:
        withMan
          (withMeta
            (
              withAttrs
                (
                  pkgs.writeShellApplication ({ inherit name text; } // {
                    runtimeInputs = pkgs.lib.lists.flatten runtimeInputs;
                    checkPhase = "";
                  })
                )
                { pname = name; }
            )
            { inherit longDescription description; }
          )
          (_: longDescription)
      ;

      withAttrs = attrSet1: attrSet2: recursiveUpdate attrSet1 attrSet2;
      withMeta = drv: meta: withAttrs drv { inherit meta; };
      withLongDescription = drv: longDescription: withAttrs drv { meta = { inherit longDescription; }; };
      withDescription = drv: description: withAttrs drv { meta = { inherit description; }; };

      # for code blocks in man
      indentStrings4 = indentStrings_ 4;
      indentStrings8 = indentStrings_ 8;
      indentStrings_ = n: y: "\n" + (concatMapStringsSep "\n" (x: (applyN n (s: " " + s) "") + x) y) + "\n";

      # add a longDescription to a derivation
      # add a man generated from longDescription
      # the function :: derivation with description -> long description
      withMan = drv: fLongDescription:
        assert hasAttr "description" drv.meta;
        let
          pname = drv.pname;
          longDescription = fLongDescription drv;
          man = ''
            ---
            title: ${pname}
            section: 1
            header: User Manual
            ---
            ${longDescription}
          '';
          manPath = "$out/share/man/man1";
          drv_ = pkgs.symlinkJoin {
            name = pname;
            inherit pname;
            paths = [ drv ];
            nativeBuildInputs = [ pkgs.pandoc ];
            postBuild = ''
              mkdir -p ${manPath}
              printf '%s' ${escapeShellArg man} > $out/${pname}.1.md
              rm -rf ${manPath}
              mkdir -p ${manPath}
              ${mkBinName pkgs.pandoc "pandoc"} $out/${pname}.1.md -st man -o ${manPath}/${pname}.1
              rm $out/${pname}.1.md
            '';
          };
        in
        withLongDescription (withMeta drv_ drv.meta) longDescription;

      # String -> String -> Any -> IO ()
      # make a script to write a nix expr to a file path
      writeJSON = name: path: dataNix:
        assert isString name && isString path;
        let
          dataJSON = toJSON dataNix;
          name_ = "write-${name}-json";
          dir = dirOf path;
          description = "Write a `Nix` expression for `${name}` as `JSON` into `${path}`";
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
          inherit description;
        };

      # String -> String -> Any -> IO ()
      # make a script to write a nix expr to a file path
      writeYAML = name: path: dataNix:
        assert isString name && isString path;
        let
          name_ = "write-${name}-yaml";
          tmpJSON = "${path}.tmp";
          dir = dirOf path;
          writeJSON_ = writeJSON "yaml-tmp" tmpJSON dataNix;
        in
        mkShellApp {
          name = name_;
          runtimeInputs = [ pkgs.yq-go ];
          text = ''
            mkdir -p ${dir}
            ${mkBin writeJSON_}
            cat ${tmpJSON} | yq e -MP - > ${path}
            rm ${tmpJSON}
            printf "${framedBrackets "ok %s"}" "${name_}"
          '';
          description = "Write a `Nix` expression for `${name}` as `YAML` into `${path}`";
        };

      # use when need to generate settings.json etc.
      json2nix =
        withMan
          (mkShellApp {
            name = "json2nix";
            runtimeInputs = [ pkgs.nixpkgs-fmt ];
            text = ''
              json_path=$1
              nix_path=$2
              mkdir -p $(dirname "''${2}")
              nix eval --impure --expr "with builtins; fromJSON (readFile ./$json_path)" > $nix_path
              sed -i -E "s/(\[|\{)/\1\n/g" $nix_path
              nixpkgs-fmt $nix_path
            '';
            description = "Convert `.json` to `.nix`";
          })
          (x: ''
            ${man.DESCRIPTION}
            ${x.meta.description}
            
            ${man.EXAMPLES}
            `json2nix .vscode/settings.json nix-files/settings.nix`
            :   Convert exising `settings.json` into a nix file
          '');

      runInEachDir =
        args@{ dirs
        , command
        , name
        , preMessage ? ""
        , message ? ""
        , postMessage ? ""
        , runtimeInputs ? [ ]
        , description
        , longDescription ? ""
        }:
        let
          dirs_ = flatten dirs;
          name_ = "${name}-in-each-dir";
        in
        mkShellApp {
          name = name_;
          inherit runtimeInputs;
          text =
            let INITIAL_CWD = "INITIAL_CWD";
            in
            ''
              ${INITIAL_CWD}=$PWD
              printf "%s" '${preMessage}'
            '' +
            concatStringsSep "\n"
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
          description = "run ${name} in each given directory";
          longDescription = ''
            ${man.NAME}
            `${name_}` - ${description}

            ${longDescription}

            ${man.NOTES}
            The directories relative to `CWD` are:

            ${indentStrings4 dirs_}
          '';
        };

      # apply an `op` `cnt` times to the initial value `ini` to get `res`
      # initially, `res` = `ini`
      applyN = cnt: op: res: (if cnt > 0 then applyN (cnt - 1) op (op res) else res);

      # make accessors from an attrset so that a.b.c represents a string "a.b.c"
      mkAccessors = mkAccessors_ "";
      # make accessors with an initial path
      mkAccessors_ = path: attrs@{ ... }:
        assert isString path;
        let common = arg: {
          __toString = self: "${arg}";
          __functor = self: path_: assert isString path_; mkAccessors_ "${arg}.${path_}" { };
        }; in
        (mapAttrs
          (name: val:
            let path_ = "${path}${if path == "" then "" else "."}${name}"; in
            (
              if isAttrs val
              then x: mkAccessors_ x val
              # if it's not a set, the next attribute cannot be accessed via .
              else common
            ) path_
          )
          attrs
        ) // (common path);
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
          framed_
          framedBrackets
          framedBrackets_
          framedNewlines
          genAttrsId
          indentStrings_
          indentStrings4
          indentStrings8
          mergeValues
          mkAccessors
          mkAccessors_
          mkBin
          mkBinName
          mkShellApp
          mkShellApps
          ord
          ord_
          readDirectories
          readFiles
          readSymlinks
          readXs
          runFishScript
          runInEachDir
          toList
          withAttrs
          withLongDescription
          withDescription
          withMan
          withMeta
          writeJSON
          writeYAML
          ;
      };

      configs = { inherit man; };

      # tests 
      devShells.default = pkgs.mkShell {
        shellHook = "man ${mkBin json2nix}";
        name = "default";
        buildInputs = [ pkgs.tree json2nix pkgs.fish ];
      };

      tests = {
        t = readFiles ./.;
        accessors = mkAccessors_ "pref" {
          a.b.c = "";
        };
        mkAttrs =
          ord_ [
            { a = 3; }
            { b = 4; }
          ];
      };
    });
}
