{ system ? builtins.currentSystem
, pkgs ? (import ../.).inputs.nixpkgs.legacyPackages.${system}
}:
let
  lib = builtins // pkgs.lib;

  # if a set's attribute values are all sets, merge these values recursively
  # Note that the precedence order is undefined, so it's better to 
  # have unique values at each set level
  # Examples:
  # mergeValues {a = {b = 1;}; c = {d = 2;};} => {b = 1; d = 2;}
  mergeValues = set@{ ... }: lib.foldl' lib.recursiveUpdate { } (lib.attrValues set);

  mergeAttrsRecursive = lib.foldl' lib.recursiveUpdate { };
  
  # a convenience function that lib.flattens a set with set attribute values
  # toList {a = {b = 1;}; c = {d = 2;};} => [1 2]
  toList = x: lib.attrValues (mergeValues x);

  # generate an attrset from a list of attrNames where attrName = attrValue
  genAttrsId = list: lib.genAttrs list lib.id;

  # map values to attrsets and merge them
  mapGenAttrs = f: list: lib.foldl' lib.recursiveUpdate { } (map f list);

  # map values to strings and to attrsets and merge them
  mapStrGenAttrs = f: list: lib.foldl' lib.recursiveUpdate { } (map (x: f (toString x)) list);

  mempty = val:
    if lib.isString val then ""
    else if lib.isList val then [ ]
    else if lib.isAttrs val then { }
    else throw "Expected a string, list, or an attrset";

  singletonIf = cond: val: if cond then [ val ] else [ ];

  memptyUnless = cond: val: if cond then val else mempty val;

  # List -> Set
  # Generate a set from a list of sets with all keys renamed
  # in the order they go in that list
  ord_ = list:
    let
      ordered =
        lib.foldl'
          (x: y:
            let
              yAttrs = lib.mapAttrsToList
                (name: value: {
                  name = "_${toString x.cnt}_${name}";
                  inherit value;
                })
                y;
              yNew = lib.listToAttrs yAttrs;
              yKeys =
                lib.mapAttrs
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

  mapFunctionRecursiveCond = f: lib.mapAttrsRecursiveCond
    (x: !(lib.isDerivation x || (builtins.intersectAttrs (lib.functionArgs f) x) != { }))
    (path: value:
      if lib.isDerivation value then value else
      if lib.isAttrs value then f ({ name = lib.concatStringsSep "-" path; } // value)
      else lib.throw "Expected an attrset or a derivation, but got ${lib.generators.toPretty value}"
    );

  mkShellApps = mapFunctionRecursiveCond mkShellApp;

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
  readXs = dir: type: lib.attrNames (
    lib.filterAttrs (name_: type_: type_ == type) (lib.readDir dir)
  );

  readFiles = dir: readXs dir "regular";
  readDirectories = dir: readXs dir "directory";
  readSymlinks = dir: readXs dir "symlink";

  # get a list of immediate subdirectories
  # root should be an absolute path like ./.
  subDirectories = root: dir: map (x: "${dir}/${x}") (readDirectories "${root}/${dir}");

  # frame a text with newlines
  framedNewlines = framed_ "\n\n" "\n\n";
  framed_ = pref: suff: txt: ''${pref}${txt}${suff}'';

  # frame a text with square brackets and newlines
  framedBrackets = framedBrackets_ "\n\n" "\n\n";
  framedBrackets_ = pref: suff: framed_ "${pref}[ " " ]${suff}";

  mkBashHref = text: url: '']8;;${url}\${text}]8;;\'';

  # concat strings and separate them by a newline character
  concatStringsNewline = list: lib.concatStringsSep "\n" (lib.flatten list);

  # concatMap strings and separate them by a newline character
  concatMapStringsNewline = f: list: lib.concatMapStringsSep "\n" f (lib.flatten list);

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
          runtimeInputs = lib.flatten runtimeInputs;
        }).overrideAttrs
          (prev: {
            pname = name;
            buildCommand = null;
            passAsFile = [ ];
            doCheck = true;
            phases = [ "configurePhase" "buildPhase" "installPhase" ];
            configurePhase = ''
              runHook preConfigure
                    
              target=$out${lib.escapeShellArg "/bin/${prev.name}"}

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
            };
          })
      )
      (_: ''
        ${man.DESCRIPTION}
        ${longDescription}
      '')
  ;

  # TODO fix order of args
  # TODO withAttrs should take a function, not just another attrset
  withAttrs = attrSet1@{ ... }: attrSet2@{ ... }: lib.recursiveUpdate attrSet1 attrSet2;
  withMeta = drv@{ ... }: fMeta: withAttrs drv { meta = fMeta drv; };
  withDescription = drv@{ ... }: fDescription: assert lib.isFunction fDescription; withAttrs drv { meta.description = fDescription drv; };
  withLongDescription = drv@{ ... }: fLongDescription: assert lib.isFunction fLongDescription; withAttrs drv { meta.longDescription = fLongDescription drv; };

  # for code blocks in man
  indentStrings4 = indentStrings_ 4;
  indentStrings8 = indentStrings_ 8;
  indentStrings_ = n: y: "\n" + (lib.concatMapStringsSep "\n" (x: (applyN n (s: " " + s) "") + x) y) + "\n";


  # add a longDescription to a derivation
  # add a man generated from longDescription
  # the function :: derivation with description -> long description
  withMan_ = executableName: drv: fLongDescription:
    assert lib.hasAttr "description" drv.meta;
    let
      longDescription = fLongDescription drv;
      man = ''
        ${executableName}(1) -- ${drv.meta.description or "no description provided :("}
        ============

        ${longDescription}
      '';
      ronn = "${executableName}.1.ronn";
      manPath = "$out/share/man/man1";
    in
    pkgs.stdenv.mkDerivation {
      pname = drv.pname or null;
      name = drv.name or null;
      version = drv.version or null;

      phases = [ "installPhase" ];
      buildInputs = [ pkgs.ronn ];
      installPhase = ''
        runHook preInstall

        mkdir -p $out
        cp -rs --no-preserve=mode,ownership ${drv}/* $out
              
        printf "Creating a man page.\n"
        rm -rf ${manPath} 2> /dev/null    
        mkdir -p ${manPath}
        chmod +w .
        printf '%s' ${lib.escapeShellArg man} > ${ronn}
        ronn ${ronn} --roff -o ${manPath}
        rm ${ronn}

        runHook postInstall
      '';
      meta = drv.meta // { inherit longDescription; };
    };
  withMan = drv: withMan_ (baseNameOf (lib.getExe drv)) drv;

  # String -> String -> Any -> IO ()
  # make a script to write a nix expr to a file path
  writeJSON = name: path: dataNix:
    assert lib.isString name && lib.isString path;
    let
      dataJSON = lib.toJSON dataNix;
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
          lib.escapeShellArg dataJSON
        } | python -m json.tool > ${path}
        printf "${framedBrackets "ok %s"}" "${name_}"
      '';
      excludeShellChecks = [ "SC2016" ];
      inherit description;
    };

  # String -> String -> Any -> IO ()
  # make a script to write a nix expr to a file path
  writeYAML = name: path: dataNix:
    assert lib.isString name && lib.isString path;
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
        ${lib.getExe writeJSON_} &>/dev/null
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
    , description ? "run `${name}` in each given directory"
    , longDescription ? ""
    }:
    let
      dirs_ = lib.flatten dirs;
      name_ = "${name}-in-each-dir";
    in
    mkShellApp {
      name = name_;
      inherit runtimeInputs;
      text =
        let INITIAL_CWD = "INITIAL_CWD";
        in
        ''
          ${INITIAL_CWD}="$PWD"
          printf "%s" '${preMessage}'
        '' +
        lib.concatStringsSep "\n"
          (map
            (dir: ''
              printf "${framedBrackets "${if message == "" then name else message} : %s"}" "''$${INITIAL_CWD}/${dir}"

              cd "''$${INITIAL_CWD}/${dir}"
            
              ${command}
            '')
            dirs_) +
        ''
          printf "%s" '${postMessage}'
        '';
      inherit description;
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
    assert lib.isString path;
    let
      common = arg: {
        __toString = self: "${arg}";
        __functor = self: path_: assert lib.isString path_; mkAccessors_ "${arg}.${path_}" { };
      };
    in
    (lib.mapAttrs
      (name: val:
        let path_ = "${path}${if path == "" then "" else "."}${name}"; in
        (
          if lib.isAttrs val
          then x: mkAccessors_ x val
          # if it's not a set, the next attribute cannot be accessed via .
          else common
        ) path_
      )
      attrs
    ) // (common path);

  bashHref = mkBashHref "github link" "https://github.com";

  packages = {
    inherit json2nix;
    test = {
      json = writeJSON "test-json" "tmp/test.json" { a = "$b"; };
      yaml = writeYAML "test-yaml" "tmp/test-yaml" { a = 3; };
      runInEachDir = runInEachDir {
        name = "test-yaml";
        command = lib.getExe packages.test.yaml;
        dirs = [ "tmp" ];
      };

      href = pkgs.writeShellApplication {
        name = "href";
        text = ''echo -e '${bashHref}' '';
        meta.description = "${bashHref}";
        excludeShellChecks = [ "SC1003" ];
      };
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
      indentStrings_
      indentStrings4
      indentStrings8
      json2nix
      man
      mapGenAttrs
      mapStrGenAttrs
      mempty
      memptyUnless
      mergeAttrsRecursive
      mergeValues
      mkAccessors
      mkAccessors_
      mkBashHref
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
      singletonIf
      subDirectories
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
      pwd
      printf 'Run `man ${baseNameOf (lib.getExe json2nix)}`\n'
    '';
    name = "default";
    buildInputs = [ pkgs.tree json2nix pkgs.fish ];
    LC_ALL = "C.utf8";
  };

  test = {
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
          helloScript.text = "${lib.getExe pkgs.hello}";
          helloX2.text = "${lib.getExe pkgs.hello}; ${lib.getExe pkgs.hello}";
          helloRenamed = pkgs.hello;
          hello.nested = pkgs.hello;
          hello.script.nested.text = "${lib.getExe pkgs.hello}";
        };
      in
      apps;
    mg = mapGenAttrs (x: { "a${toString x}" = { "b${toString x}" = x; }; }) [ 1 2 ];
    msg = mapStrGenAttrs (x: { "a${x}" = { "b${x}" = x; }; }) [ 1 2 ];
  };
}
