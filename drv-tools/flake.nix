{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs: inputs.flakes.makeFlake {
    inputs = { inherit (inputs.flakes.outputs.inputs) flake-utils nixpkgs; };
    perSystem = { inputs, system }:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
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

        # map values to attrsets and merge them
        mapGenAttrs = f: list: foldl' recursiveUpdate { } (builtins.map f list);

        # map values to strings and to attrsets and merge them
        mapStrGenAttrs = f: list: foldl' recursiveUpdate { } (builtins.map (x: f (toString x)) list);

        mempty = val:
          if builtins.isString val then ""
          else if builtins.isList val then [ ]
          else if builtins.isAttrs val then { }
          else throw "Expected a string, list, or an attrset";

        singletonIf = cond: val: if cond then [ val ] else [ ];

        memptyUnless = cond: val: if cond then val else mempty val;

        # List -> Set
        # Generate a set from a list of sets with all keys renamed
        # in the order they go in that list
        ord_ = list:
          let
            ordered =
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
          in
          ordered;

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

        # get a list of immediate subdirectories
        # root should be an absolute path like ./.      
        subDirectories = root: dir: builtins.map (x: "${dir}/${x}") (readDirectories "${root}/${dir}");

        # assuming that a `pname` of a program coincides with its main executable's name
        # unsafe to use with packages whose pname may change!
        mkBin = drv@{ pname, ... }: "${drv}/bin/${pname}";

        # same as `mkBin`, but need to provide the necessary executable name
        mkBinName = drv@{ ... }: name_: "${drv}/bin/${name_}";

        inherit (pkgs.lib) getExe;

        # frame a text with newlines
        framedNewlines = framed_ "\n\n" "\n\n";
        framed_ = pref: suff: txt: ''${pref}${txt}${suff}'';

        # frame a text with square brackets and newlines
        framedBrackets = framedBrackets_ "\n\n" "\n\n";
        framedBrackets_ = pref: suff: framed_ "${pref}[ " " ]${suff}";

        # concat strings and separate them by a newline character
        concatStringsNewline = list: concatStringsSep "\n" (flatten list);

        # concatMap strings and separate them by a newline character
        concatMapStringsNewline = f: list: concatMapStringsSep "\n" f (flatten list);

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
          { name
          , text
          , runtimeInputs ? [ ]
          , description ? "no description provided :("
          , longDescription ? ''
              ${description}
            ''
          }:
          withMan
            (withMeta
              (
                withAttrs
                  (
                    pkgs.writeShellApplication ({ inherit name text; } // {
                      runtimeInputs = flatten runtimeInputs;
                      checkPhase = "";
                    })
                  )
                  { pname = name; }
              )
              (_: { inherit longDescription description; })
            )
            (_: ''
              ${man.DESCRIPTION}
              ${longDescription}
            '')
        ;

        # wrap a shell application to, e.g., set a new script name
        wrapShellApp =
          { app
          , name ? app.pname
          , text ? mkBin app
          , runtimeInputs ? [ ]
          , description ? __replaceStrings [ app.pname ] [ name ] app.meta.description
          , longDescription ? __replaceStrings [ app.pname ] [ name ] app.meta.longDescription
          }:
          withMan
            (withMeta
              (
                withAttrs
                  (
                    pkgs.writeShellApplication ({ inherit name text; } // {
                      runtimeInputs = flatten runtimeInputs;
                      checkPhase = "";
                    })
                  )
                  { pname = name; }
              )
              (_: { inherit longDescription description; })
            )
            (_: longDescription)
        ;

        withAttrs = attrSet1@{ ... }: attrSet2@{ ... }: recursiveUpdate attrSet1 attrSet2;
        withMeta = drv@{ ... }: fMeta: withAttrs drv { meta = fMeta drv; };
        withDescription = drv@{ ... }: fDescription: assert builtins.isFunction fDescription; withAttrs drv { meta.description = fDescription drv; };
        withLongDescription = drv@{ ... }: fLongDescription: assert builtins.isFunction fLongDescription; withAttrs drv { meta.longDescription = fLongDescription drv; };

        # for code blocks in man
        indentStrings4 = indentStrings_ 4;
        indentStrings8 = indentStrings_ 8;
        indentStrings_ = n: y: "\n" + (concatMapStringsSep "\n" (x: (applyN n (s: " " + s) "") + x) y) + "\n";


        # add a longDescription to a derivation
        # add a man generated from longDescription
        # the function :: derivation with description -> long description
        withMan_ = executableName: drv: fLongDescription:
          assert hasAttr "description" drv.meta;
          let
            longDescription = fLongDescription drv;
            man = ''
              ---
              title: ${executableName}
              section: 1
              header: User Manual
              ---
              ${longDescription}
            '';
            md = "$out/${executableName}.1.md";
            manPath = "$out/share/man/man1";
            drv_ =
              withAttrs
                (pkgs.runCommand executableName { nativeBuildInputs = [ pkgs.pandoc ]; }
                  ''
                    mkdir $out
                    cp -rs --no-preserve=mode,ownership ${drv}/* $out/
                    rm -rf ${manPath}
                    mkdir -p ${manPath}
                    printf '%s' ${escapeShellArg man} > ${md}
                    pandoc ${md} -st man -o ${manPath}/${executableName}.1
                    rm ${md}
                  ''
                )
                { pname = drv.pname; };
          in
          withLongDescription (withMeta drv_ (_: drv.meta)) (_: longDescription);

        withMan = drv: withMan_ drv.pname drv;

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
              ${mkBin writeJSON_} &>/dev/null
              cat ${tmpJSON} | yq e -MP - > ${path}
              rm ${tmpJSON}
              printf "${framedBrackets "ok %s"}" "${name_}"
            '';
            description = "Write a `Nix` expression for `${name}` as `YAML` into `${path}`";
          };

        # Convert JSON to Nix
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
          { dirs
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
                    printf "${framedBrackets "${if message == "" then name else message} : %s"}" "''$${INITIAL_CWD}/${dir}"

                    cd ''$${INITIAL_CWD}/${dir}
            
                    ${command}
                  '')
                  dirs_) +
              ''
                printf "%s" '${postMessage}'
              '';
            description = "run `${name}` in each given directory";
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
          let
            common = arg: {
              __toString = self: "${arg}";
              __functor = self: path_: assert isString path_; mkAccessors_ "${arg}.${path_}" { };
            };
          in
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
        lib = {
          inherit
            applyN
            concatMapStringsNewline
            concatStringsNewline
            framed_
            framedBrackets
            framedBrackets_
            framedNewlines
            genAttrsId
            getExe
            indentStrings_
            indentStrings4
            indentStrings8
            json2nix
            man
            mapGenAttrs
            mapStrGenAttrs
            mempty
            memptyUnless
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
            subDirectories
            readFiles
            readSymlinks
            readXs
            runFishScript
            runInEachDir
            singletonIf
            toList
            withAttrs
            withDescription
            withLongDescription
            withMan
            withMan_
            withMeta
            wrapShellApp
            writeJSON
            writeYAML
            ;
        };

        # tests 
        devShells.default = pkgs.mkShell {
          shellHook = ''
            printf 'Run `man ${json2nix.pname}`\n'
          '';
          name = "default";
          buildInputs = [ pkgs.tree json2nix pkgs.fish ];
          LC_ALL = "C.utf8";
        };

        tests = {
          s = subDirectories ../. "templates";
          t = readFiles ./.;
          accessors = mkAccessors_ "pref" {
            a.b.c = "";
          };
          writeYAML = writeYAML "hey" "tmp/test-yaml" { a = 3; };
          mkAttrs =
            ord_ [
              { a = 3; }
              { b = 4; }
            ];
          mg = mapGenAttrs (x: { "a${toString x}" = { "b${toString x}" = x; }; }) [ 1 2 ];
          msg = mapStrGenAttrs (x: { "a${x}" = { "b${x}" = x; }; }) [ 1 2 ];
        };
      };
  };
}
