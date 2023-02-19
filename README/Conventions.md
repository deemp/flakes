# Conventions

## READMEs

In this repo, each flake's `README.md` assumes that the directory of its `flake.nix` is your current working directory in a terminal.

## Prerequisites

To de-duplicate the docs, one of the top sections of each `README.md` here contains a `Prerequisites` section. This section provides the links to other sections that possibly contain the relevant docs. Sometimes, you'll need to traverse multiple `Prerequisites` before you achieve the relevant docs.

## nixConfig

I inserted a number of binary caches into `nixConfig` attribute of each `flake.nix`. These caches should accelerate builds for my flakes' users. It is recommended that you add the `extra-trusted-substituters` from `flake.nix` of interest into your [nix.conf](https://nixos.org/manual/nix/stable/command-ref/conf-file.html#conf-trusted-substituters) as `trusted-substituters`. See this [discussion](https://t.me/ru_nixos/137950).

## Dev tools

The outputs of a flake depend on its inputs. Inputs of a `flake.nix` are locked in an accompanying `flake.lock`.
If you expect that your flake will be used as an input to other flakes, you may want to leave only the relevant outputs in this flake.
Dev tools that you use are irrelevant to users of your flake, so they should be moved into another flake.
Usually, I place this another flake into a `./nix-dev` directory, near my main `./flake.nix`.
This way, I:

- move the irrelevant outputs from my main flake into `nix-dev`
- move the irrelevant inputs into `nix-dev`
- hence, reduce the size of my main `flake.lock`
- hence, reduce the size of `flake.lock`s of my flake's users

Then, it becomes possible to enter a dev environment as follows:

```sh
nix develop nix-dev/
```

## Pushing to a remote repo

All flakes in this repo access some other flakes from this repo via `GitHub` URLs.
Let's consider two flakes from this repo, `A` and `B`, where `A` is in `inputs` of `B`.
If I change the flake `A`, I should propagate that change into the flake `B`.
In other words, it's necessary to update `B`'s `flake.lock`.
One can update `B`'s `flake.lock` this way iff `A`'s changes are pushed to `GitHub`.
Whenever there's a push to the remote `GitHub` repo, `B`'s `flake.lock` is updated by a `GitHub Action`.
That's why, if that action works, there's no need to commit and push `flake.lock` changes.
After an update is completed, it's necessary to rebase the local changes onto remote changes.
However, sometimes, there are local uncommitted changes already.
These changes should be `git stash`ed before doing `git rebase`.
After rebasing, they can be `git stash pop`ped to continue the work.

Thus, the process is as follows:

```sh
git stash
# can be omitted in case of automatic fetches
git fetch
git rebase
git stash pop
```

## Docs

Each derivation that evaluates to an executable should have these attributes:

- `meta.description`
  - string written in `CommonMark`
  - This description will be rendered in devshells
  - It should be a single-line brief description of this executable
- `meta.longDescription`
  - string written in `CommonMark`
  - This description is used to generate `man` pages for executables
  - The format of a `longDescription` should be recognizable by `pandoc`
    - Here's a sample [input](https://pandoc.org/demo/pandoc.1.md)
