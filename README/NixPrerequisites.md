# Nix Prerequisites

## Install Nix

- Complete all steps from [here](InstallNix.md)

### Community

- [NixOS/Community](https://nixos.org/community/)
- [Telegram](https://t.me/ru_nixos/19843)
- [Nix Community](https://nix-community.org/)

## Resources

- Study the `Nix` language
  - [here](https://nixos.org/manual/nix/unstable/language/index.html)
  - and [here](https://nixos.wiki/wiki/Overview_of_the_Nix_Language)
- Learn about [flakes](#flakes)
- Read:
  - [Nix manual](https://nixos.org/manual/nix/unstable/command-ref/nix-store.html)
  - [Nix glossary](https://nixos.org/manual/nix/unstable/glossary.html)
  - [nixpkgs manual](https://nixos.org/manual/nixpkgs/unstable)
  - [nix.dev](https://nix.dev/) - Good tutorials. No flakes so far
  - [NixOS/Learn](https://nixos.org/learn.html)
- Watch
  - [Nix: Under the hood by Gabriel Gonzalez](https://www.youtube.com/watch?v=GMQPzv3Sx58)

### References

I added bookmarks for search engines in my browser, like described [here](https://superuser.com/a/7336).

- [nix-lib](https://teu5us.github.io/nix-lib.html) - Nix (builtins) & Nixpkgs (lib) Functions
- [Nixpkgs manual](https://nixos.org/manual/nixpkgs/stable/#nixpkgs-manual)
- [devdocs](https://devdocs.io/) - search functions
  - [config](./devdocs-config.json)
- [grep nixpkgs](https://search.nix.gsc.io/?q=wrapProgram&i=nope&files=&excludeFiles=&repos=)
- [grep nixpkgs on GitHub](https://github.com/search?q=repo%3ANixOS%2Fnixpkgs+writePython3Bin&type=code)

## Docs

### Nix repl

Docs are usually left as comments in `Nix` code. You can find an attribute in [nix-repl](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-repl.html) and look up its comments in a file at a given position.

```console
nix repl
nix-repl> :lf nixpkgs
nix-repl> legacyPackages.x86_64-linux.lib.lists.singleton
«lambda @ /nix/store/24akvz6idhp4lxxvhbfxxq84py30v6bw-source/lib/lists.nix:23:15»
```

Sometimes, you may evaluate a `description` or a `longDescription` of a derivation:

```console
nix-repl> legacyPackages.x86_64-linux.coreutils.meta.longDescription
"The GNU Core Utilities are the basic file, shell and text manipulation\nutilities of the GNU operating system. These are the core utilities which\nare expected to exist on every operating system.\n"
```

### nix commands

Learn about a command:

```console
nix help command
```

Search for a package description:

```console
nix edit nixpkgs#coreutils
```

## Flake templates

### List templates

If a `flake.nix` contains a valid output `templates`, these templates can be listed.
List flake outputs and search for `templates` in this (`flakes`) repository:

```console
nix flake show | grep template
└───templates
evaluating ''    ├───codium-generic: template: `VSCodium` with extensions and executables
    ├───codium-haskell: template: `VSCodium` with extensions and executables for `Haskell`. Shows 5 ways to run a `Haskell` app.
evaluating ''    ├───codium-haskell-simple: template: `VSCodium` with extensions and executables for `Haskell`.
evaluating ''    ├───codium-python: template: `VSCodium` with extensions and executables for `Python`.
    └───haskell-minimal: template: Minimal flake for a `Haskell` package development.
```

### Use a template

Two ways of copying from templates:

- [nix flake init](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake-init.html)
- [nix flake new](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake-new.html)

### Update a cached template

If you notice that `nix flake init`, `nix flake new`, etc. give you an old version of a template, run `nix flake lock` on that template. This action will force `nix` to update the cached version of the template somewhere in `~/.cache/nix`. - [src](https://t.me/ru_nixos/140144)

### Explore a template

Templates can be accessed in Nix store via `nix repl`:

```console
nix repl
nix-repl> :lf .
nix-repl> templates.<TAB>
nix-repl> templates.codium-generic.path
/nix/store/j3kx4dk567y483pvszr2w8ghnkxich3d-source/templates/codium/generic
```

### Assemble

Templates may contain arbitrary files. This feature enables the following assembly:

```sh
nix flake new flake-1 -t flake1
nix flake new flake-2 -t flake2
nix flake new flake-3 -t flake3
cp flake-1/file1 flake-3
cp flake-2/file{2,3} flake-3
```

### Template repositories

- Templates for many programming languages - [src](https://github.com/the-nix-way/dev-templates)
- Some more - [src](https://github.com/NixOS/templates)
- Some templates are in [this repo](https://github.com/deemp/flakes)

## Flakes

1. What are flakes? How to enable flakes? - [wiki](https://nixos.wiki/wiki/Flakes)

1. [Nix manual](https://nixos.org/manual/nix/unstable/) answers the following questions:
   - [Glossary](https://nixos.org/manual/nix/unstable/glossary.html#glossary)
     - What does this term mean?
   - [Derivations](https://nixos.org/manual/nix/unstable/language/derivations.html#derivations)
     - What are derivations?
       - Something that describes how to create a particular `Nix` store path
       - A `derivation` can become an executable, and that executable can be used in build scripts of other derivations
   - [Description](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html#description)
     - What are flakes?
       - A flake is a function from inputs into outputs. To take this function at a point, a `flake.lock` is used
       - Don't confuse 'inputs' and 'outputs' with the terms `inputs` and `outputs`.
       - I use 'inputs' and 'outputs' to describe the high-level idea of what a flake is.
       - On the other hand, `inputs` and `outputs` have specifications described in Nix manual.
   - [Flake inputs](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html#flake-inputs):
     - How to use old-style packages as flake inputs?
     - How to declare an input? == How to use a flake in another flake?
     - How to follow an input?
     - How to pin an input?
   - [Flake format](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html#flake-format)
     - What can go into `outputs`?
     - How to access the `outputs`? (described at the bottom)
     - How can I override [nix.conf](https://nixos.org/manual/nix/unstable/command-ref/conf-file.html)? - via `nixConfig`
   - [Experimental commands](https://nixos.org/manual/nix/unstable/command-ref/experimental-commands.html)
     - How to explore a flake?
       - `nix flake show`
       - `nix flake metadata`
       - `nix repl`
         - `nix-repl> :lf nixpkgs` - load flake `nixpkgs`
     - How to see all `derivation`s used to build a specific `derivation`?
       - `nix show-derivation`
     - How can I temporarily make an executable available in my terminal?
       - `nix shell`
     - How can I install a Nix package on my OS?
       - `nix profile`
     - How can I pin my global `nixpkgs` to a specific commit SHA?
       - `nix registry`
   - And many more questions!

1. How to add a flake to a project?
    - Nix uses `git` to track flake files. So, adding a `flake.nix` to a project requires the following steps:
       1. `git init` - initialize a `git` repository
       1. Add `flake.nix` in some way (copy the existing `flake.nix`, `nix flake init`, etc.)
       1. `git add flake.nix`
       1. Add `flake.lock` in some way (copy the relevant existing `flake.lock`, generate a new one via `nix flake update`, etc.)
       1. `git add flake.lock`
       1. `git commit` these files

1. Flake tutorials: [1](https://nix-tutorial.gitlabpages.inria.fr/nix-tutorial/flakes.html?highlight=flake), [2](https://yuanwang.ca/posts/getting-started-with-flakes.html), [3](https://ghedam.at/a-tour-of-nix-flakes), [4](https://xeiaso.net/blog/nix-flakes-2-2022-02-27), [5](https://myme.no/posts/2022-01-16-nixos-the-ultimate-dev-environment.html#launch-script)

1. How to use the same flake inputs in all projects?
   - Store your flake inputs in a repo - [example](https://github.com/deemp/flakes/blob/2395f79740fdc5f14f91db10b1acd2892cdee28c/source-flake)
   - Use them in your projects with `follows` - [example](https://github.com/deemp/flakes/blob/2395f79740fdc5f14f91db10b1acd2892cdee28c/codium/flake.nix#L5)
   - Now, all your projects have the same dependencies since they come from the same source

1. How to enable a specific version of `nix` on my system? Approximately so:

   ```console
   nix registry remove nix
   nix registry add nix github:NixOS/nix/4bf70b74a78bf10f3f19ed122ae7377963e14003
   nix profile install nix --priority 4
   ```

1. How to pin `nixpkgs`?
   - [nix registry pin](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-registry-pin.html#examples)

1. When should I use overlays over `nixpkgs`? - You [shouldn't](https://zimbatm.com/notes/1000-instances-of-nixpkgs)

1. How to convert an exising project to flakes? - [tutorial](https://garnix.io/blog/converting-to-flakes)

1. How to use a `.gitignore`d file inside a flake?

   - build with `--impure`

      ```console
      zxcTest = prev.writeText "zxcTest" (builtins.readFile /home/name_snrl/nixos-configuration/<fileFromIgnore>);
      ```

   - Provide it in inputs. Inputs accept arbitrary files. Make links follow the format of [flake inputs](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html#flake-inputs)

1. How to disable querying remote binary caches?

     - `nix run <installable> --no-substitute`

## devShells

1. How to set up a development environment?
   2. `pkgs.mkShell`
   3. [devshell](https://github.com/deemp/flakes/tree/main/devshell)
   4. Add environment variables if necessary. E.g., using Nix store paths [post](https://discourse.nixos.org/t/provide-environmental-variables-from-nix/3453/3)
   5. Symlink a path from Nix store to make files available to `NodeJS` - [NixOS: The Ultimate Dev Environment?](https://myme.no/posts/2022-01-16-nixos-the-ultimate-dev-environment.html#launch-script)

## Nix

## Nix store caching

- [cachix](https://www.cachix.org/)
- [Attic](https://github.com/zhaofengli/attic)
- [cache-nix-too](https://github.com/deemp/cache-nix-too) - cache between `GitHub Actions` runs
- [nix-serve](https://github.com/edolstra/nix-serve) - serve nix store as a binary cache
  - to check if a package is in store, use [curl](https://nixos.wiki/wiki/Binary_Cache#4._Testing)

## Q&A

1. Run a binary from `nixpkgs`:

   ```sh
   nix run nixpkgs#nixpkgs-fmt -- hello-flake/flake.nix
   ```

### Making derivations and executables

1. What is `expression`, `closure`, `derivation`?

   - Check the [glossary](https://nixos.org/manual/nix/unstable/glossary.html)
   - [expression, closure, derivation](https://medium.com/scientific-breakthrough-of-the-afternoon/closure-vs-derivation-in-the-nix-package-manager-ec0eccc53407)

1. How to get a path of a derivation in store in a `.nix` file?: `${drv}`

1. There are [phases](https://nixos.org/manual/nixpkgs/unstable/#sec-stdenv-phases)
   - They can be run via `nix develop` - [src](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-develop.html#examples)
   - [Example](https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L49)

1. When derivations are built, they may produce executables. Locations of these executables are determined by bash scripts. If you make a derivation you can use `buildInputs` to specify the derivations you'd like to be accessible during in scripts during `phases` or in a `shellHook`

1. How to access a `$out` folder of a derivation `drv`?

   - First of all, you should create such folder, e.g. via a [builder](https://nixos.wiki/wiki/Shell_Scripts#runCommand_.2B_builder.sh)
      - Alternatively, it will be created during the [install phase](https://nixos.org/manual/nixpkgs/stable/#ssec-install-phase)
   - Next, you can use it in a script like `${drv.out}`;

1. Programs can be specified in `buildInputs` or called by name `{pkgs.hello}/bin/hello`
   - This helped me when configuring tasks for VSCodium [here](https://github.com/deemp/devops-labs/blob/b400993e18b0e1ebc515141450c51f2d6c8b3f67/.nix/commands.nix#L37)

1. How to wrap an executable to run it with flags?
   - See the example with `pkgs.runCommand` in [Wrapping Packages](https://nixos.wiki/wiki/Nix_Cookbook#Wrapping_packages)
   - Another example in [language-tools/haskell](https://github.com/deemp/flakes/blob/4ed230763b12a2805b0369cf4446fff1e490a582/language-tools/haskell/flake.nix#L36)
1. How to rename an executable and supply runtime dependencies?

   - [symlinkJoin](https://discourse.nixos.org/t/basic-flake-run-existing-python-bash-script/19886/11) + `wrapProgram` provided by `pkgs.makeBinaryWrapper` ([docs](https://nixos.org/manual/nixpkgs/stable/#fun-wrapProgram), [src](https://github.com/NixOS/nixpkgs/blob/0e9e77750818f40303c72ad658b3dca299591e4f/pkgs/build-support/setup-hooks/make-wrapper.sh#L130)).

    ```nix
    hello =
    pkgs.symlinkJoin {
      name = "hello";
      paths = [ hello ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/hello \
          --set PATH ${
            pkgs.lib.makeBinPath [
              pkgs.cowsay
            ]
          }
      '';
    };
    ```

1. How to escape a shell/bash command?
   2. `pkgs.lib.escapeShellArg`

1. How to see binaries that a package provides?
   2. `find  $(nix build nixpkgs#hello --no-link --print-out-paths)/bin -mindepth 1`

1. It's possible to use `nix` commands inside scripts

   - E.g. `nix-instantiate --eval --strict -E "import ./settings.nix"` - print contents of a nix file
   - Moreover, one can use pinned `nixpkgs`: `nix run ${pkgs}#nixpkgs-fmt $nix_path`

1. How to package a script?
   2. [pkgs.writeScriptBin](https://nixos.org/manual/nixpkgs/unstable/#trivial-builder-writeText)

1. How to remove build deps from runtime deps?
   - see this [pill](https://nixos.org/guides/nix-pills/automatic-runtime-dependencies.html#idm140737320124560)

1. How to use parallel builds? - [docs](https://nixos.org/manual/nix/stable/advanced-topics/cores-vs-jobs.html#tuning-cores-and-jobs)

1. [cross-compilation](https://serokell.io/blog/what-is-nix#nixpkgs)

### Helper function libs

1. [flake-compat](https://github.com/edolstra/flake-compat).
   1. Whenever possible, add a flake into a repo, and then use `flake-compat` to create `default.nix` and `shell.nix`
   1. Use it to access scripts from a flake in a child directory - [example](https://github.com/deemp/projects/blob/ce7907b4f2a9c4f9f0bb30ca3b8c50284929405c/flake.nix#L35)

1. [flake-utils](https://github.com/numtide/flake-utils)

1. `nixpkgs.lib`

   ```sh
   $ nix repl
   nix-repl> :lf nixpkgs
   nix-repl> lib.or true false
   ```

1. Symlinked things cannot be written or opened. They should first be removed - [src](https://nixos.wiki/wiki/Nix_Cookbook#Wrapping_packages)
   - [add man page](https://github.com/deemp/flakes/blob/b57918dfa6cf694e81886cb0dd858731f4987b08/drv-tools/flake.nix#L132)

1. `pkgs.dockerTools.buildLayeredImage` - build a docker image
   1. Pass static executables
   1. Set entrypoint: `config.Entrypoint = [ command ]`. No need for `bash`
   1. `docker inspect` the image

## Package code

### Haskell

See [Haskell](./Haskell.md)

### Python

1. [poetry2nix](https://github.com/nix-community/poetry2nix)
   - [example](https://serokell.io/blog/practical-nix-flakes#python-(poetry))
   - It might be necessary to
     - activate an environment for any app in a project - [src](https://github.com/deemp/devops-labs/blob/b400993e18b0e1ebc515141450c51f2d6c8b3f67/app_python/flake.nix#L37)
     - create envs - [src](https://github.com/deemp/devops-labs/blob/b400993e18b0e1ebc515141450c51f2d6c8b3f67/.nix/default.nix#L128)

1. [pkgs.writers.writePython3Bin](https://github.com/NixOS/nixpkgs/search?q=writePython3Bin&type=) - [SO](https://stackoverflow.com/a/67799667)

[dream2nix](https://github.com/nix-community/dream2nix)

### TypeScript

- I [packaged](https://github.com/objectionary/try-phi/blob/beeae361822be7db7cb3bb4bb469c9c74a51cff6/front/flake.nix#L33) try-phi front end with it, following this [tutorial](https://johns.codes/blog/building-typescript-node-apps-with-nix#dream2nix)

### PureScript

1. [spago.nix](https://github.com/ngua/spago.nix)
   - TODO try
