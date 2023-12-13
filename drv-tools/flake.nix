{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs: inputs.flakes.makeFlake {
    inputs = { inherit (inputs.flakes.all) flake-utils nixpkgs; };
    perSystem = { inputs, system }:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        inherit (pkgs.lib.lists) flatten;
        inherit (pkgs.lib.attrsets)
          recursiveUpdate filterAttrs genAttrs mapAttrsToList
          mapAttrsRecursiveCond isDerivation;
        inherit (builtins)
          foldl' attrValues mapAttrs attrNames readDir map toString
          isString isAttrs dirOf baseNameOf toJSON hasAttr listToAttrs;
        inherit (pkgs.lib.strings)
          concatStringsSep concatMapStringsSep
          removePrefix removeSuffix;
        inherit (pkgs.lib) escapeShellArg id getExe getExe';
        inherit (pkgs.lib.trivial) throwIfNot;

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
        mkShellApps =
          attrs@{ ... }:
          mapAttrs
            (
              name: value:
              if isDerivation value
              then withMeta value (x: { label = name; })
              else
                throwIfNot (isAttrs value) "Expected an attrset or a derivation" (
                  let cond = value_: hasAttr "text" value_ && isString value_.text; in
                  if cond value then mkShellApp (value // { inherit name; })
                  else
                    mapAttrsRecursiveCond (value_: cond value || isDerivation value)
                      (
                        path: value_:
                        let name_ = concatStringsSep "." ([ name ] ++ path); in
                        (mkShellApps { "${name_}" = value_; }).${name_}
                      )
                      value
                )
            )
            attrs;

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

        # frame a text with newlines
        framedNewlines = framed_ "\n\n" "\n\n";
        framed_ = pref: suff: txt: ''${pref}${txt}${suff}'';

        # frame a text with square brackets and newlines
        framedBrackets = framedBrackets_ "\n\n" "\n\n";
        framedBrackets_ = pref: suff: framed_ "${pref}[ " " ]${suff}";

        mkBashHref = text: url: ''\e]8;;${url}\a${text}\e]8;;\a'';

        # concat strings and separate them by a newline character
        concatStringsNewline = list: concatStringsSep "\n" (flatten list);

        # concatMap strings and separate them by a newline character
        concatMapStringsNewline = f: list: concatMapStringsSep "\n" f (flatten list);

        # man headings
        man = {
          NAME = "## NAME";
          SYNOPSYS = "## SYNOPSYS";
          DESCRIPTION = "## DESCRIPTION";
          EXAMPLES = "## EXAMPLES";
          NOTES = "## NOTES";
        };

        mkShellApp =
          { name
          , text
          , runtimeInputs ? [ ]
          , description ? "no description provided :("
          , longDescription ? description
          , excludeShellChecks ? [ ]
          }:
          withMan
            (
              (pkgs.writeShellApplication {
                inherit name text excludeShellChecks;
                runtimeInputs = flatten runtimeInputs;
              }).overrideAttrs
                (prev: {
                  pname = name;
                  buildCommand = null;
                  passAsFile = [ ];
                  doCheck = true;
                  phases = [ "configurePhase" "buildPhase" "checkPhase" "installPhase" ];
                  configurePhase = ''
                    runHook preConfigure
                    
                    target=$out${pkgs.lib.escapeShellArg "/bin/${prev.name}"}

                    runHook postConfigure
                  '';
                  buildPhase = ''
                    runHook preBuild

                    mkdir -p "$(dirname "$target")"

                    if [ -e "$textPath" ]; then
                      mv "$textPath" "$target"
                    else
                      echo -n "$text" > "$target"
                    fi

                    if [ -n "$executable" ]; then
                      chmod +x "$target"
                    fi

                    runHook postBuild
                  '';
                  installPhase = ''
                    runHook preInstall

                    runHook postInstall
                  '';
                  meta = prev.meta // {
                    inherit longDescription description;
                    mainProgram = name;
                    label = name;
                  };
                })
            )
            (_: ''
              ${man.DESCRIPTION}
              ${longDescription}
            '')
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
              ${executableName}(1) -- ${drv.meta.description}
              ============

              ${longDescription}
            '';
            ronn = "${executableName}.1.ronn";
            manPath = "$out/share/man/man1";
            drv_ = drv.overrideAttrs (prev: {
              buildInputs = (prev.buildInputs or [ ]) ++ [ pkgs.ronn ];
              postInstall = (prev.postInstall or "") + ''
                printf "Creating a man page.\n"
                rm -rf ${manPath} 2> /dev/null    
                mkdir -p ${manPath}
                chmod +w .
                printf '%s' ${escapeShellArg man} > ${ronn}
                ronn ${ronn} --roff -o ${manPath}
                rm ${ronn}
              '';
            });
          in
          withMeta drv_ (_: drv.meta // { inherit longDescription; });

        withMan = drv: withMan_ (builtins.baseNameOf (getExe drv)) drv;

        # String -> String -> Any -> IO ()
        # make a script to write a nix expr to a file path
        writeJSON = name: path: dataNix:
          assert isString name && isString path;
          let
            dataJSON = toJSON dataNix;
            name_ = "write-${name}-json";
            dir = dirOf path;
            description = "Write a `Nix` expression for `${name}` as `JSON` to `${path}`";
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
            excludeShellChecks = [ "SC2016" ];
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
              ${getExe writeJSON_} &>/dev/null
              yq e -MP ${tmpJSON} > ${path}
              rm ${tmpJSON}
              printf "${framedBrackets "ok %s"}" "${name_}"
            '';
            description = "Write a `Nix` expression for `${name}` as `YAML` to `${path}`";
          };

        # Convert JSON to Nix
        json2nix =
          withMan
            (mkShellApp {
              name = "json2nix";
              runtimeInputs = [ pkgs.nixpkgs-fmt pkgs.nix ];
              text = ''
                json_path="$1"
                nix_path="$2"
                mkdir -p "$(dirname "''${2}")"
                nix eval --impure --expr "with builtins; fromJSON (readFile ./$json_path)" > "$nix_path"
                sed -i -E "s/(\[|\{)/\1\n/g" "$nix_path"
                nixpkgs-fmt "$nix_path"
              '';
              description = "Convert `.json` to `.nix`";
            })
            (x: ''
              ${man.EXAMPLES}
              `json2nix .vscode/settings.json nix-files/settings.nix`
              :   Convert exising `settings.json` into a nix file
            '')
        ;

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

        packages = {
          inherit json2nix;
          test = {
            json = writeJSON "test" "tmp/test.json" { a = "$b"; };
            yaml = writeYAML "hey" "tmp/test-yaml" { a = 3; };
          };
        };
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
            getExe'
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
            mkBashHref
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
            writeJSON
            writeYAML
            ;
        };

        inherit packages;

        # tests 
        devShells.default = pkgs.mkShell {
          shellHook = ''
            printf 'Run `man ${builtins.baseNameOf (getExe json2nix)}`\n'
          '';
          name = "default";
          buildInputs = [ pkgs.tree json2nix pkgs.fish ];
          LC_ALL = "C.utf8";
        };
        devShells.href = pkgs.mkShell {
          shellHook = ''printf '${mkBashHref "github link" "https://github.com"}\n' '';
        };

        tests = {
          s = subDirectories ../. "templates";
          t = readFiles ./.;
          accessors = mkAccessors_ "pref" {
            a.b.c = "";
          };
          mkAttrs =
            ord_ [
              { a = 3; }
              { b = 4; }
            ];
          shellApps =
            let
              apps = mkShellApps {
                helloScript.text = "${getExe pkgs.hello}";
                helloX2.text = "${getExe pkgs.hello}; ${getExe pkgs.hello}";
                helloRenamed = pkgs.hello;
                hello.nested = pkgs.hello;
              };
            in
            apps;
          mg = mapGenAttrs (x: { "a${toString x}" = { "b${toString x}" = x; }; }) [ 1 2 ];
          msg = mapStrGenAttrs (x: { "a${x}" = { "b${x}" = x; }; }) [ 1 2 ];
        };
      };
  };
}
