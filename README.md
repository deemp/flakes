# Nix. Tutorials and Notes

## Other

1. What is `expression`, `closure`, `derivation`?
    *  [expression, closure, derivation](https://medium.com/scientific-breakthrough-of-the-afternoon/closure-vs-derivation-in-the-nix-package-manager-ec0eccc53407)

1. Where are nix configs stored?
    * [here](https://nixos.wiki/wiki/Flakes#Permanent)

1. How to use a flake in another flake? Should I build it somehow?
    * Specify it in `inputs`, then use the derivations from its `outputs` where appropriate. They will use flake's inputs to build

1. Why do I get `warning: ignoring untrusted substituter 'https://cachix-shell.cachix.org`?
    * Due to [multi-user](https://nixos.org/manual/nix/stable/installation/multi-user.html) mode. No such problems in single-user
    * In multi-user mode, users have [environments](https://nixos.wiki/wiki/User_Environment).

1. How to convert an exising project to flakes? 
    * Like [this](https://garnix.io/blog/converting-to-flakes)

1. What are some projects using [haskell.nix](https://input-output-hk.github.io/haskell.nix/)?
    * [try-phi](https://github.com/objectionary/try-phi)
    * [discord-emoji-dl](https://github.com/br4ch1st0chr0n3/discord-emoji-dl)

1. How can I use phases (build, check, etc.)?
    * [Example](https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L49)

1. Another flake [tutorial](https://ghedam.at/a-tour-of-nix-flakes)

1. Why do I have different results when 
    1. loading `try-phi` into repl and calling `hals`
    1. trying to access `hls` there as an attribute of outputs?

1. How to integrate `stack` with Nix?
    1. [Here](https://docs.haskellstack.org/en/stable/nix_integration/)'s how

1. How to build haskell executables with cabal?
    * A very thorough [tutorial](https://yuanwang.ca/posts/getting-started-with-flakes.html) on flakes

1. A nice [tutorial](https://nix-tutorial.gitlabpages.inria.fr/nix-tutorial/experiment-packaging.html) on `default.nix`

1. What means `some-pkg.follows = "another/package";`? 
    * [src](https://discourse.nixos.org/t/nix-flake-to-aggregate-and-concurrently-update-some-dependencies/10774/8?u=br4ch1st0chr0n3)
    * [Example](https://ianthehenry.com/posts/how-to-learn-nix/flakes/):
    ```sh
    $ nix flake metadata github:edolstra/dwarffs
    Resolved URL:  github:edolstra/dwarffs
    Locked URL:    github:edolstra/dwarffs/f691e2c991e75edb22836f1dbe632c40324215c5
    Description:   A filesystem that fetches DWARF debug info from the Internet on demand
    Path:          /nix/store/769s05vjydmc2lcf6b02az28wsa9ixh1-source
    Revision:      f691e2c991e75edb22836f1dbe632c40324215c5
    Last modified: 2021-01-21 06:41:26
    Inputs:
    ├───nix: github:NixOS/nix/6254b1f5d298ff73127d7b0f0da48f142bdc753c
    │   ├───lowdown-src: github:kristapsdz/lowdown/1705b4a26fbf065d9574dce47a94e8c7c79e052f
    │   └───nixpkgs: github:NixOS/nixpkgs/ad0d20345219790533ebe06571f82ed6b034db31
    └───nixpkgs follows input 'nix/nixpkgs'
    ```
    so we can write `inputs.nixpkgs.follows = "nix/nixpkgs";`[]

1. I didn't find a way to use stack without `shell.nix` in the `cachix` project - [tutorial](https://docs.haskellstack.org/en/stable/nix_integration/#stack-and-developer-tools-on-nixos)

1. A tutorial on [Haskell.nix](https://github.com/Gabriella439/haskell-nix)

1. How to use `nix-build` with a `default.nix` that returns multiple derivations?

1. One can load a flake into repl and explore the packages - [src](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-repl.html)
    ```sh
    nix repl --extra-experimental-features 'flakes repl-flake' nixpkgs
    legacypackages.x86_64-linux
    ```

1. Locales problems [troubleshooting](https://nixos.wiki/wiki/Locales)

1. Learn about a command:
    ```sh
    nix help command
    ```

    ```nix
    {
        description = "A flake for building Hello World";

        inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-20.03;

        outputs = { self, nixpkgs }: {

            packages.x86_64-linux.default =
            # Notice the reference to nixpkgs here.
            with import nixpkgs { system = "x86_64-linux"; };
            stdenv.mkDerivation {
                name = "hello";
                src = self;
                buildPhase = "gcc -o hello ./hello.c";
                installPhase = "mkdir -p $out/bin; install -t $out/bin hello";
            };

        };
    }
    ```

    * `outputs`: A function that, given an attribute set containing the outputs of each of the input flakes keyed by their identifier, yields the Nix values provided by this flake. <text style="color: violet">Thus, in the example above, `inputs.nixpkgs` contains the result of the call to the outputs function of the `nixpkgs` flake. </text>


1. How to give aliases to built executables?
    * See `pkgs.symlinkJoin`

1. How to bring old-style packages to a flake and its shell?
    *  Use overlays - like [here](https://github.com/nix-community/npmlock2nix/issues/159#issuecomment-1156772007)

1. How to deal with `Node.js`? 
    * see [this](https://github.com/nix-community/npmlock2nix)

1. When should I use overlays?
    * You [shouldn't](https://zimbatm.com/notes/1000-instances-of-nixpkgs)

1. How to add commands for execution in child directories - [src](https://superuser.com/a/271992)
    ```sh
    (cd child && echo "hello")
    ```

1. Full Nix [manual](https://nixos.org/manual/nixpkgs/stable/), including helper functions

1. [Docs](https://devdocs.io/) for many functions
    * [config](./devdocs-config.json)

1. You can add a search bookmark (Firefox) - [src](https://superuser.com/a/7336)
    * In a search box, click with the right mouse button
    * Click on `Add a keyword`

1. When derivations are built, they may produce executables. Locations of these executables are determined by bash scripts. If you make a derivation you can use `buildInputs` to specify the derivations you'd like to be accessible during [phases](https://nixos.org/manual/nixpkgs/stable/) or in a [shellHook](https://nixos.org/manual/nixpkgs/stable/)

1. When you want to contribute, [squash](https://htmlacademy.ru/blog/articles/how-to-squash-commits-and-why-it-is-needed) commits
    * It's easy with Gitlens:
        1. Find out how many new commits you have compared to `main`
            ```sh
            $ git cherry -v upstream/main | wc -l
            N
            ```
        1. Then rebase
            ```sh
            git rebase -i HEAD~N
            ```
        1. Choose to squash the latest commits

1. It's possible to use `nix` commands inside scripts
    * E.g. `nix-instantiate --eval --strict -E "import ./settings.nix"` - print contents of a nix file
    * Moreover, one can use pinned `nixpkgs`: `nix run ${nixpkgs}#nixpkgs-fmt $nix_path`

1. What if you want a symlink from a file in store to a file in the current project (or where you suppose to run the shell)
    * Then make a derivation `drv` that contains that file in the store
    * Next, `mkShell` and in its `shellHook` refer to that `drv` like `{drv}/file`
        ```nix
        sh = pkgs.mkShell rec {
          t = pkgs.mkShell {};
          shellHook = ''
            echo ${t}
          '';
        };
        ```

1. Scripts can become packages via `pkgs.writeScriptBin`

1. The most active [chat](https://app.element.io/#/room/#nix:nixos.org)

1. Whenever possible, add a flake into a repo, and then use [flake-compat](https://github.com/edolstra/flake-compat) to create `default.nix` and `nix-shell.nix`
    * This even works for [stack with Nix](https://docs.haskellstack.org/en/stable/nix_integration/#external-c-libraries-through-a-shellnix-file)!. Just add the stack shell into `outputs` and create a `stack.nix` using `flake-compat` with `.default`.

1. How to access a `$out` folder of a derivation `drv`
    * First of all, you should create such folder, e.g. via a [builder](https://nixos.wiki/wiki/Shell_Scripts#runCommand_.2B_builder.sh)
    * Next, you can use it in a script like `''${drv.out}''`;

1. `git bisect` is your friend

1. What's the difference between flake's package and a derivation?