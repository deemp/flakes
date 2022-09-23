# Tutorials and Notes

## Nix

### Flakes

1. A basic flake - [hello-flake](hello-flake/flake.nix)

   `outputs`: A function that, given an attribute set containing the outputs of each of the input flakes keyed by their identifier, yields the Nix values provided by this flake. Thus, in the example above, `inputs.nixpkgs` contains the result of the call to the `outputs` function of the `nixpkgs` flake.

1. How to use a flake in another flake? Should I build it somehow?

   - Specify it in `inputs`, then use the derivations from its `outputs` where appropriate. They will use flake's inputs to build

1. A simple flake [tutorial](https://nix-tutorial.gitlabpages.inria.fr/nix-tutorial/flakes.html?highlight=flake) with simple examples

1. A very thorough [tutorial](https://yuanwang.ca/posts/getting-started-with-flakes.html) on flakes

1. Another flake [tutorial](https://ghedam.at/a-tour-of-nix-flakes)

1. Another [blog](https://xeiaso.net/blog/nix-flakes-2-2022-02-27) on flakes

1. How to convert an exising project to flakes?

   - Like [this](https://garnix.io/blog/converting-to-flakes)

1. How to bring old-style packages to a flake?
   - use `flake = false` in inputs - [example](https://github.com/br4ch1st0chr0n3/flakes/blob/c3c578c3798bea79897d774293e34a1fadb06f8b/inputs/flake.nix#L16)

1. Flake inputs tip
   - Store your flake inputs in a repo - [example](https://github.com/br4ch1st0chr0n3/flakes/blob/c3c578c3798bea79897d774293e34a1fadb06f8b/inputs/flake.nix)
   - Use them in your projects - [example](https://github.com/br4ch1st0chr0n3/flakes/blob/c3c578c3798bea79897d774293e34a1fadb06f8b/codium/flake.nix#L3)
   - Use `nix flake update` in dependent projects to easily update the inputs

### Docs

1. It's recommended to add bookmarks for search engines in your browser, like described here [Browser](README.md#browser)

1. Nix [Documentation gaps](https://nixos.wiki/wiki/Documentation_Gaps)

1. Full Nix [manual](https://nixos.org/manual/nixpkgs/stable/), including helper functions

1. [Docs](https://devdocs.io/) for many functions

   - [config](./devdocs-config.json)

1. Search for a package file: `nix edit nixpkgs#makeWrapper`

1. What means `some-pkg.follows = "another/package";`?

   - [src](https://discourse.nixos.org/t/nix-flake-to-aggregate-and-concurrently-update-some-dependencies/10774/8?u=br4ch1st0chr0n3)
   - [Example](https://ianthehenry.com/posts/how-to-learn-nix/flakes/):

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

1. How to use `nix-doc`?

   - Here's a [repo](https://github.com/lf-/nix-doc)

1. Grep usage in nixpkgs [here](https://search.nix.gsc.io/?q=wrapProgram&i=nope&files=&excludeFiles=&repos=)

1. `nix repl` - [src](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-repl.html)

   ```sh
   $ nix repl
   nix-repl> :lf nixpkgs
   nix-repl> legacypackages.x86_64-linux
   ```

1. Learn about a command:

   ```sh
   nix help command
   ```

### Other resources

- [The Nix Way](https://github.com/the-nix-way) - maintained by ad DevOps engineer
- [NixOS/Learn](https://nixos.org/learn.html)

### Chats

- Nix [Community](https://nixos.org/community/)
- [Telegram](https://t.me/ru_nixos/19843)

### Concepts

1. What is `expression`, `closure`, `derivation`?

   - [expression, closure, derivation](https://medium.com/scientific-breakthrough-of-the-afternoon/closure-vs-derivation-in-the-nix-package-manager-ec0eccc53407)

1. Where are nix configs stored?

   - [here](https://nixos.wiki/wiki/Flakes#Permanent)

1. When should I use overlays over `nixpkgs`?

   - You [shouldn't](https://zimbatm.com/notes/1000-instances-of-nixpkgs)

1. There are [phases](https://nixos.org/manual/nixpkgs/unstable/#sec-stdenv-phases)
   - They can be run via `nix develop` - [src](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-develop.html#examples)

### Stable

1. How to use `nix-build` with a `default.nix` that returns multiple derivations?

   - See `nix manual` [here](https://nixos.org/manual/nix/unstable/command-ref/nix-build.html#description)

### Dev shell

1. Pin `nixpkgs` - `nix registry pin`
   - more on that [here](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-registry-pin.html#examples)

1. Run a binary from `nixpkgs`:

   ```sh
   nix run nixpkgs#nixpkgs-fmt -- hello-flake/flake.nix
   ```

### mkShell

1. We can add environment variables in `pkgs.mkShell` - [example](https://discourse.nixos.org/t/provide-environmental-variables-from-nix/3453/3?u=br4ch1st0chr0n3)

### Making derivations and exes

1. When derivations are built, they may produce executables. Locations of these executables are determined by bash scripts. If you make a derivation you can use `buildInputs` to specify the derivations you'd like to be accessible during in scripts during `phases` or in a `shellHook`

1. Wrap an exe - via [makeWrapper](https://github.com/NixOS/nixpkgs/blob/0e9e77750818f40303c72ad658b3dca299591e4f/pkgs/build-support/setup-hooks/make-wrapper.sh#L130)

   - Sample usage - [here](https://nixos.wiki/wiki/Nix_Cookbook#Wrapping_packages)

     - supply runtime deps

       ```nix
       manager =
        let
          manager-exe = staticExecutable "manager" ./manager;
        in
        pkgs.symlinkJoin {
          name = "manager";
          paths = [ manager-exe ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/manager \
              --set PATH ${
                pkgs.lib.makeBinPath [
                  pkgs.hpack
                ]
              }
          '';
        };
       ```

1. Simple app that writes VS Codium settings - [here](https://github.com/br4ch1st0chr0n3/flakes/blob/c3c578c3798bea79897d774293e34a1fadb06f8b/codium/flake.nix#L135). It uses

   - `lib.escapeShellArg` - escape `$` etc. when passing a command as a string to a shell inside a `.nix` file
   - `pkgs.writeShellApplication` - it allows to use `runtimeInputs` as `python` vs `${pkgs.python310}/bin/python`
      - need to [disable](https://github.com/br4ch1st0chr0n3/flakes/blob/c3c578c3798bea79897d774293e34a1fadb06f8b/codium/flake.nix#L148) shellcheck for now

1. It's possible to use `nix` commands inside scripts

   - E.g. `nix-instantiate --eval --strict -E "import ./settings.nix"` - print contents of a nix file
   - Moreover, one can use pinned `nixpkgs`: `nix run ${pkgs}#nixpkgs-fmt $nix_path`

1. How can I use phases (build, check, etc.)? - see [docs](https://nixos.org/manual/nixpkgs/stable/#sec-stdenv-phases)

   - [Example](https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L49)

1. Get a path of a derivation in store: `${drv}`

1. How to access a `$out` folder of a derivation `drv`?

   - First of all, you should create such folder, e.g. via a [builder](https://nixos.wiki/wiki/Shell_Scripts#runCommand_.2B_builder.sh)
      - Alternatively, it will be created during the [install phase](https://nixos.org/manual/nixpkgs/stable/#ssec-install-phase)
   - Next, you can use it in a script like `${drv.out}`;

1. Scripts can become packages via `pkgs.writeScriptBin`

1. It's possible to remove some build deps from runtime deps - see this [pill](https://nixos.org/guides/nix-pills/automatic-runtime-dependencies.html#idm140737320124560)

1. We can utilize parallel build via [max-jobs](https://wiki.archlinux.org/title/Nix#Max_jobs) (also [here](https://nixos.org/manual/nix/stable/advanced-topics/cores-vs-jobs.html#tuning-cores-and-jobs))

### Helper function libs

1. [flake-compat](https://github.com/edolstra/flake-compat). Whenever possible, add a flake into a repo, and then use `flake-compat` to create `default.nix` and `shell.nix`

1. [flake-utils](https://github.com/numtide/flake-utils)

1. `nixpkgs.lib`

   ```sh
   $ nix repl
   nix-repl> :lf nixpkgs
   nix-repl> nixpkgs.lib
   ```

### Y2nix

1. Here's a sample `poetry2nix` [flake](https://github.com/nix-community/poetry2nix/blob/869580c729e658ffe74d8d1d0c3cb132d33a6126/templates/app/flake.nix) - can be used for Python
   - TODO make environment - [example](https://github.com/br4ch1st0chr0n3/devops-labs/blob/539db68da661bb9a385dbc4bb1a4bcdf6a9072b8/app_python/flake.nix#L35)

1. There's [dream2nix](https://github.com/nix-community/dream2nix)

   - I [packaged](https://github.com/objectionary/try-phi/blob/beeae361822be7db7cb3bb4bb469c9c74a51cff6/front/flake.nix#L33) try-phi front end with it, following this [tutorial](https://johns.codes/blog/building-typescript-node-apps-with-nix#dream2nix)

### Y.nix

1. A tutorial on [Haskell.nix](https://github.com/Gabriella439/haskell-nix)

1. What are some projects using [haskell.nix](https://input-output-hk.github.io/haskell.nix/)?

   - [discord-emoji-dl](https://github.com/br4ch1st0chr0n3/discord-emoji-dl)

1. [spago.nix](https://github.com/ngua/spago.nix)
   - TODO try

### Package Haskell

1. My [flakes/codium](https://github.com/br4ch1st0chr0n3/flakes/tree/c3c578c3798bea79897d774293e34a1fadb06f8b/codium) includes convenience functions - wrappers around `callCabal2nix`:

   - [callCabalGHC](https://github.com/br4ch1st0chr0n3/flakes/blob/c3c578c3798bea79897d774293e34a1fadb06f8b/codium/flake.nix#L173)
   - [staticExecutable](https://github.com/br4ch1st0chr0n3/flakes/blob/c3c578c3798bea79897d774293e34a1fadb06f8b/codium/flake.nix#L185)

1. How to package a Haskell app - [tutorial](https://www.haskellforall.com/2022/08/incrementally-package-haskell-program.html)

1. How to integrate `stack` with Nix? - [src](https://docs.haskellstack.org/en/stable/nix_integration/#supporting-both-nix-and-non-nix-developers)

1. See `flake-compat`
   - Create a `devShells.stack-shell = {ghcVersion} : ....` by following the appropriate section of [docs](https://docs.haskellstack.org/en/stable/nix_integration/#external-c-libraries-through-a-shellnix-file) (might be just `{ghc}`)
   - Create a `stack.nix` and use there `flake-compat` with `.stack-shell`
      - [Example](https://github.com/br4ch1st0chr0n3/cachix/blob/17efcd60abe547d33bb2ccc63b561797a94e5b46/stack.nix)


### Installation

1. Why do I get `warning: ignoring untrusted substituter 'https://cachix-shell.cachix.org`?

   - Due to [multi-user](https://nixos.org/manual/nix/stable/installation/multi-user.html) mode. No such problems in single-user
   - In multi-user mode, users have [environments](https://nixos.wiki/wiki/User_Environment).

1. Locales problems [troubleshooting](https://nixos.wiki/wiki/Locales)

## Browser

1. You can add a search bookmark (Firefox) - [src](https://superuser.com/a/7336)

   - In a search box, click with the right mouse button
   - Click on `Add a keyword`

1. Mine are:
   - [nixman](https://nixos.org/manual/nix/unstable/command-ref/nix-store.html?searchbar=&search=s)
   - [stack](https://docs.haskellstack.org/en/stable/)
   - [devdoc](https://devdocs.io/nix/)
   - [nixpkgs](https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=s&=)
   - [gh](https://github.com/search?type=&q=)
   - [docker](https://docs.docker.com/search/?q=s)
   - [halogen](https://purescript-halogen.github.io/purescript-halogen/index.html?search=s)

## Git

1. [git bisect](https://git-scm.com/docs/git-bisect#_basic_bisect_commands_start_bad_good) is your friend when searching for a problematic commit

1. When you want to contribute, [squash](https://htmlacademy.ru/blog/articles/how-to-squash-commits-and-why-it-is-needed) commits

   - It's easy with [Gitlens](https://marketplace.visualstudio.com/items?itemName=eamodio.gitlens):
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

## GitHub

1. GitHub dislikes `nix develop` and `nix-shel`. You should run commands via `nix develop -c bash -c 'command'`

### Actions

1. You can use composite actions - [src](https://docs.github.com/en/actions/creating-actions/creating-a-composite-action)

   - Use composite-actions-specific [syntax](https://docs.github.com/en/actions/creating-actions/metadata-syntax-for-github-actions#runs-for-composite-actions)

1. Which variables are available to acomposite action?

   - `env`, but [not](https://stackoverflow.com/a/70111134) `secrets`

1. Dynamically set env variable - [src](https://stackoverflow.com/a/70399393)

1. Develop a composite action

   - parameterize appropriately - [tutorial](https://colinsalmcorner.com/github-composite-actions/#case-study-eshoponcontainers)

1. GitHub permits to work with a single branch at a time. Use `actions/checkout` to switch to another branch.

   - otherwise, fails with `error: src refspec branch-name does not match any`

## Docker

1. Caching [trick](https://fastapi.tiangolo.com/deployment/docker/#docker-cache) - Basically, you should copy the least volatile files like `package.json` and use them as much ASAP, and the most volatile ones like general source code as late as possible.

   - [Example](https://github.com/br4ch1st0chr0n3/devops-labs/blob/539db68da661bb9a385dbc4bb1a4bcdf6a9072b8/app_purescript/Dockerfile)

1. [Ports](https://docs.docker.com/config/containers/container-networking/#published-ports)

## Shell

1. Explain shell commands - [src](https://explainshell.com/explain?cmd=tar%20xzvf%20archive.tar.gz)

1. How to execute a command as if in a specific directory - [src](https://superuser.com/a/271992)

   ```sh
   (cd child && echo "hello")
   ```

## VSCodium

1. We can add compound tasks in VS Code - [src](https://code.visualstudio.com/docs/editor/tasks#_compound-tasks)

## Questions

1. What's the difference between flake's package and a derivation?
