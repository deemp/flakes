# Tutorials and Notes

## Nix

## Shell

- `nix shell` - bring package to shell
  
  ```sh
  $ nix shell nixpkgs#jq
  $ jq --version
  jq-1.6
  $ which jq
  /nix/store/5x1jpz4grhzl1psmrxj42wzmvllqgbhm-jq-1.6-bin/bin/jq
  ```

### Flakes

1. What are flakes, how to enable them?
   - see the official [wiki](https://nixos.wiki/wiki/Flakes)

   > `outputs`: A function that, given an attribute set containing the outputs of each of the input flakes keyed by their identifier, yields the Nix values provided by this flake. Thus, in the example above, `inputs.nixpkgs` contains the result of the call to the `outputs` function of the `nixpkgs` flake.

1. A flake with a derivation - [hello-flake](hello-flake/flake.nix)

   ```sh
   cd hello-flake && nix run
   ```

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
   - Use `nix flake update` in dependent projects to update the inputs

1. What's the difference between a flake's package and a derivation?
   - `outputs.packages.${system}` is a set with derivations as values

1. Can add an additional nix config to a

### Docs

1. [nix-lib](https://teu5us.github.io/nix-lib.html)

1. [Glossary](https://nixos.org/manual/nix/unstable/glossary.html#gloss-closure)

1. It's recommended to add bookmarks for search engines in your browser, like described here [Browser](README.md#browser)

1. Nix [Documentation gaps](https://nixos.wiki/wiki/Documentation_Gaps)

1. Full Nix [manual](https://nixos.org/manual/nixpkgs/stable/), including helper functions
   - dark [version](https://ryantm.github.io/nixpkgs/languages-frameworks/texlive/#sec-language-texlive)

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

1. We can set the `nix.conf` to
   - [show-trace](https://nixos.org/manual/nix/unstable/command-ref/conf-file.html?highlight=trace#conf-show-trace)

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

1. One may write docs in Markdown
   - I use [longDescription](https://github.com/br4ch1st0chr0n3/flakes/blob/5883de8f1eabac3a5a0069b1330b4b0f7c630b9a/drv-tools/flake.nix#L151)
   - And [desc](https://github.com/br4ch1st0chr0n3/flakes/blob/5883de8f1eabac3a5a0069b1330b4b0f7c630b9a/drv-tools/flake.nix#L210) to show it in a terminal

### Troubleshooting

- Sometimes, if you `wrapProgram` with `--prefix PATH`, this program's `PATH` may not contain the specified binary paths
   1. example - [mkCodium](https://github.com/br4ch1st0chr0n3/flakes/blob/42ea51cac48ff6d17e76055c905f081fabdbf8f7/codium/flake.nix#L75) for VSCodium
   2. get this derivation's nix store path. search for the original executable, not given by `devshell`
   3. try to `nix store delete` it
   4. if fail, make it not alive
   5. `nix store repair` it

### Useful functions

- [symlinkJoin](https://discourse.nixos.org/t/basic-flake-run-existing-python-bash-script/19886/11) + `wrapProgram` provided by `pkgs.makeBinaryWrapper`

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

1. Using single `inputs.my-inputs.url = path` and then inheriting inputs doesn't make `flake.lock` smaller

## Nix store

- One can serve local nix store via [nix-serve](https://nixos.org/manual/nix/unstable/package-management/binary-cache-substituter.html#serving-a-nix-store-via-http) - [gh](https://github.com/edolstra/nix-serve)
  - to check if a package is in store, use [curl](https://nixos.wiki/wiki/Binary_Cache#4._Testing)

### Stable

1. How to use `nix-build` with a `default.nix` that returns multiple derivations?

   - See `nix manual` [here](https://nixos.org/manual/nix/unstable/command-ref/nix-build.html#description)

### devShells

1. Pin `nixpkgs` - `nix registry pin`
   - more on that [here](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-registry-pin.html#examples)

1. Run a binary from `nixpkgs`:

   ```sh
   nix run nixpkgs#nixpkgs-fmt -- hello-flake/flake.nix
   ```

### mkShell

1. We can add environment variables in `pkgs.mkShell` - [example](https://discourse.nixos.org/t/provide-environmental-variables-from-nix/3453/3?u=br4ch1st0chr0n3)

### Making derivations and exes

1. Wen derivations are built, they may produce executables. Locations of these executables are determined by bash scripts. If you make a derivation you can use `buildInputs` to specify the derivations you'd like to be accessible during in scripts during `phases` or in a `shellHook`

1. It's more reliable to use paths to binaries rather than specify, e.g. `buildInputs` and call programs by names
   - This helped me when configuring tasks for VSCodium [here](https://github.com/br4ch1st0chr0n3/devops-labs/blob/b400993e18b0e1ebc515141450c51f2d6c8b3f67/.nix/commands.nix#L37)

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

1. See available utilities in `coreutils` - [src](https://discourse.nixos.org/t/should-a-package-using-grep-sed-awk-coreutils-etc-list-them-as-buildinputs-and-wrap-them-all/9374/5?u=br4ch1st0chr0n3)
   1. Provide the missing ones

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

1. A python exe via [pkgs.writers.writePython3Bin](https://github.com/NixOS/nixpkgs/search?q=writePython3Bin&type=) - [SO](https://stackoverflow.com/a/67799667)

1. [cross-compilation](https://serokell.io/blog/what-is-nix#nixpkgs)

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
   - [example](https://serokell.io/blog/practical-nix-flakes#python-(poetry))
   - It might be necessary to
     - activate an environment for any app in a project - [src](https://github.com/br4ch1st0chr0n3/devops-labs/blob/b400993e18b0e1ebc515141450c51f2d6c8b3f67/app_python/flake.nix#L37)
     - create the envs - [src](https://github.com/br4ch1st0chr0n3/devops-labs/blob/b400993e18b0e1ebc515141450c51f2d6c8b3f67/.nix/default.nix#L128)

1. There's [dream2nix](https://github.com/nix-community/dream2nix) for TypeScript

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

- `git rebase -Xtheirs another_branch` - to favor current branch over `another_branch` - [src](https://demisx.github.io/git/rebase/2015/07/02/git-rebase-keep-my-branch-changes.html)

## GitHub

1. GitHub dislikes `nix develop` and `nix-shel`. You should run commands via `nix develop -c bash -c 'command'`

### Actions

1. You can use composite actions - [src](https://docs.github.com/en/actions/creating-actions/creating-a-composite-action)

   - Use composite-actions-specific [syntax](https://docs.github.com/en/actions/creating-actions/metadata-syntax-for-github-actions#runs-for-composite-actions)

1. Which variables are available to a composite action?

   - `env`, but [not](https://stackoverflow.com/a/70111134) `secrets`

1. Dynamically set env variable - [src](https://stackoverflow.com/a/70399393)

1. Develop a composite action

   - parameterize appropriately - [tutorial](https://colinsalmcorner.com/github-composite-actions/#case-study-eshoponcontainers)

1. GitHub permits to work with a single branch at a time. Use `actions/checkout` to switch to another branch.

   - otherwise, fails with `error: src refspec branch-name does not match any`

1. Defining objects:
   1. for [matrix](https://docs.github.com/en/actions/learn-github-actions/expressions#example-returning-a-json-object)
   1. for [general purpose](https://docs.github.com/en/actions/using-jobs/defining-outputs-for-jobs#example-defining-outputs-for-a-job)
      - output value should be a [string](https://docs.github.com/en/actions/learn-github-actions/contexts#needs-context)
      - read a config - [SO](https://stackoverflow.com/a/73639034)
        - sample [action](https://github.com/br4ch1st0chr0n3/nix-vscode-marketplace/actions/runs/3160375278/workflow)

1. `actions/checkout` doesn't pull the latest commit
   2. If a previous job pushes to the repo, need to pull in a current job

## Docker

1. Caching [trick](https://fastapi.tiangolo.com/deployment/docker/#docker-cache) - Basically, you should copy the least volatile files like `package.json` and use them as much ASAP, and the most volatile ones like general source code as late as possible.

   - [Example](https://github.com/br4ch1st0chr0n3/devops-labs/blob/539db68da661bb9a385dbc4bb1a4bcdf6a9072b8/app_purescript/Dockerfile)

1. [Ports](https://docs.docker.com/config/containers/container-networking/#published-ports)

1. There's [docker-lock](https://github.com/safe-waters/docker-lock), but we'd better [buildLayeredImage](https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/docker/examples.nix)

## Direnv

- run direnv in a separate process - [src](https://dev.to/allenap/some-direnv-best-practices-actually-just-one-4864)

- it has a wiki - [src](https://github.com/direnv/direnv/wiki)

## PureScript

1. [Halogen](https://purescript-halogen.github.io/purescript-halogen/index.html)

1. Tests

   - [purescript-spec](https://pursuit.purescript.org/packages/purescript-spec/7.0.0)
   - [example](https://github.com/citizennet/purescript-httpure/blob/1a2e1343cc272928a0e312bbe41791008089ee11/test/Test/HTTPure/BodySpec.purs#L37)

## Shell/Bash

1. Explain shell commands - [src](https://explainshell.com/explain?cmd=tar%20xzvf%20archive.tar.gz)

1. How to execute a command as if in a specific directory - [src](https://superuser.com/a/271992)

   ```sh
   (cd child && echo "hello")
   ```

1. `bash` is a superset of `sh`. So, some commands may work differently there - [src](https://www.geeksforgeeks.org/difference-between-sh-and-bash/)

1. `echo $array_name` outputs the first element of array
   1. Need to print "${array_name[@]}"

1. `set -euxo pipefail` - bash strict mode - [src](https://gist.github.com/mohanpedala/1e2ff5661761d3abd0385e8223e16425#set--o-pipefail)
   - `fish` doesn't have such flags yet - see [issue](https://github.com/fish-shell/fish-shell/issues/510)

1. `mktemp` - to create a temp file or dir - [src](https://code-maven.com/create-temporary-directory-on-linux-using-bash)

1. `xargs` - construct an argument list - [src](https://www.ibm.com/docs/en/zos/2.3.0?topic=descriptions-xargs-construct-argument-list-run-command)

## Text processing

1. `awk` is a nice tool - [src](https://www.gnu.org/software/gawk/manual/gawk.html)
   1. multiline [matches](https://stackoverflow.com/a/44547769)

1. [jq](https://www.baeldung.com/linux/jq-command-json) - for JSON
   - online [editor](https://jqplay.org/s/ekYvnaA-7IK)
   - [variables](https://stackoverflow.com/a/34747439)
   - array construction - [devdocs](https://devdocs.io/jq/index#Array/ObjectValueIterator:.[])

1. [sed](https://sed.js.org/) playground - design expressions

## VSCodium

1. We can add compound tasks in VS Code - [src](https://code.visualstudio.com/docs/editor/tasks#_compound-tasks)

1. `PAT` for GitHub should have permissions `read:user, repo, user:email, workflow` (its [checks](https://github.com/microsoft/vscode-pull-request-github/issues/3847#issue-1335886580))

## Linux

1. One can set a [cron](https://linuxhint.com/cron_jobs_complete_beginners_tutorial/) job to run e.g., `@reboot` or `@hourly`

1. [direnv](https://github.com/direnv/direnv/wiki) wiki!

1. Ignoring errors - [man](https://www.baeldung.com/linux/bash-errors)

## DevX

1. In a project, there are `solid` parts - IDE, environment, helper scripts, tasks - this should be built to make the most convenient conditions for working on `soft` parts - the code

1. Use [tmux](https://thevaluable.dev/tmux-config-mouseless/) to manage terminals

## Yandex Cloud

1. [Иерархия ресурсов Yandex Cloud](https://cloud.yandex.ru/docs/resource-manager/concepts/resources-hierarchy)

## Virtual Machines

1. For local development, need to create a VM

   - e.g., [Ubuntu](https://ubuntu.com/tutorials/how-to-run-ubuntu-desktop-on-a-virtual-machine-using-virtualbox#1-overview) on VirtualBox (live-server, without GUI)
   - need to disable Secure Boot to get VirtualBox run VMs

1. Set up [port forwarding](https://dev.to/developertharun/easy-way-to-ssh-into-virtualbox-machine-any-os-just-x-steps-5d9i) in VirtualBox

1. Connect to a VM via `ssh`
   - `ssh-keygen` - generate a key
   - `ssh-copy-id` - copy it on the target VM

## Python

1. f-strings variable pprint:

   ```python
   >>> print(f"{a = }")
   a = 2
   ```

## Networks

1. [OpenVPN](https://openvpn.net/community-resources/how-to/)

   - IP address -
   - DNS names -
   - Netmask -
   - Subnets -
   - IP routing -
   - Routers -
   - Network interfaces -
   - LAN -
   - Gateways -
   - Firewall rules -

## Pending Questions

1. How to get size of a project in terms of its nix store paths?
