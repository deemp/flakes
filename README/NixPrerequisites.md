# Nix Prerequisites

## Install Nix

- Complete all steps from [here](InstallNix.md)
- Join `NixOS` [community](https://nixos.org/community/)
  - [Telegram](https://t.me/ru_nixos/19843)

## Basics

- Study [Nix language](https://nixos.wiki/wiki/Overview_of_the_Nix_Language)
- Learn about [flakes](#flakes)
- Read:
  - [Nix manual](https://nixos.org/manual/nix/unstable/command-ref/nix-store.html?searchbar=&search=s)
  - [nixpkgs manual](https://nixos.org/manual/nixpkgs/unstable)
  - [nix.dev](https://nix.dev/) - Good tutorials. No flakes so far

## Docs

### Nix repl

Docs are usually left as comments in Nix code. You can find an attribute in [nix-repl](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-repl.html) and look up its comments in a file at a given position.

```console
nix repl
nix-repl> :lf github:deemp/flakes?dir=drv-tools
nix-repl> functions.<TAB>
nix-repl> functions.x86_64-linux.mkShellApp
«lambda @ /nix/store/d82z2sx0q9h4mnijbcm9d6i0db6lf79k-source/drv-tools/flake.nix:97:9»
```

Sometimes, you may evaluate a `description` or a `longDescription` of a derivation:

```console
nix-repl> packages.x86_64-linux.json2nix.description
"Convert `.json` to `.nix`"
```

### Online

Many docs are available online

- [nix-lib](https://teu5us.github.io/nix-lib.html) - Nix (builtins) & Nixpkgs (lib) Functions
- [Nixpkgs manual](https://nixos.org/manual/nixpkgs/stable/#nixpkgs-manual)
- [devdocs](https://devdocs.io/) - search functions
  - [config](./devdocs-config.json)

## Templates

### Template format

See [nix flake init](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake-init.html)

### List templates

If a `flake.nix` contains a valid output `templates`, these templates can be listed.
List flake outputs and search for `templates` in this (`flakes`) repository:

```console
nix flake show
...
└───templates
    ├───codium-generic: template: VSCodium with extensions and executables
    └───codium-haskell: template: VSCodium with extensions and executables for Haskell
```

### Explore a template

Templates can be accessed in Nix store via `nix repl`:

```console
nix repl
nix-repl> :lf .
nix-repl> templates.<TAB>
nix-repl> templates.codium-generic.path
/nix/store/j3kx4dk567y483pvszr2w8ghnkxich3d-source/templates/codium/generic
```

### Copy

Two ways of copying from templates:

- [nix flake init](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake-init.html)
- [nix flake new](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake-new.html)

### Assemble

Templates may contain arbitrary files. This feature enables the following assembly:

```sh
nix flake new flake-1 -t flake1
nix flake new flake-2 -t flake2
nix flake new flake-3 -t flake3
cp flake-1/file1 flake-3
cp flake-2/file{2,3} flake-3
```

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

1. Flake tutorials: [1](https://nix-tutorial.gitlabpages.inria.fr/nix-tutorial/flakes.html?highlight=flake), [2](https://yuanwang.ca/posts/getting-started-with-flakes.html), [3](https://ghedam.at/a-tour-of-nix-flakes), [4](https://xeiaso.net/blog/nix-flakes-2-2022-02-27)

1. Flake inputs tip
   - Store your flake inputs in a repo - [example](https://github.com/deemp/flakes/blob/2395f79740fdc5f14f91db10b1acd2892cdee28c/source-flake)
   - Use them in your projects with `follows` - [example](https://github.com/deemp/flakes/blob/2395f79740fdc5f14f91db10b1acd2892cdee28c/codium/flake.nix#L5)
   - Now, all your projects have the same dependencies since they come from the same source

1. How to enable a specific version of `nix` on my system? Approximately so:

   ```console
   nix registry remove nix
   nix registry add nix github:NixOS/nix/4bf70b74a78bf10f3f19ed122ae7377963e14003
   nix profile install nix --priority 4
   ```

1. When should I use overlays over `nixpkgs`? - You [shouldn't](https://zimbatm.com/notes/1000-instances-of-nixpkgs)

1. How to convert an exising project to flakes? - [tutorial](https://garnix.io/blog/converting-to-flakes)
