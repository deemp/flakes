# Conventions

## READMEs

In this repo, each flake's `README.md` assumes that the directory of its `flake.nix` is your current working directory in a terminal.

## Prerequisites

To de-duplicate the docs, one of the top sections of each `README.md` here contains a `Prerequisites` section. This section provides the links to other sections that possibly contain the relevant docs. Sometimes, you'll need to traverse multiple `Prerequisites` before you achieve the relevant docs.

## nixConfig

I inserted a number of binary caches into `nixConfig` attribute of each `flake.nix`.
These caches should accelerate builds.
You can add an [accept-flake-config](https://nixos.org/manual/nix/unstable/command-ref/conf-file.html#conf-accept-flake-config) to your [nix.conf](https://nixos.org/manual/nix/stable/command-ref/conf-file.html#conf-trusted-substituters).

## Dev tools

Sometimes, I put dev tools into a `nix-dev` directory.
Then, I enter a devshell as follows.

```sh
nix develop nix-dev/
```

## Pushing to a remote repo

All flakes in this repo access some other flakes from this repo via `GitHub` URLs.
Let's consider two flakes from this repo, `A` and `B`, where `A` is in `inputs` of `B`.
If I change the flake `A`, I:

- push `A` changes to GitHub.
- update `flake.lock` of `B`.

Whenever there's a push to the remote `GitHub` repo, `B`'s `flake.lock` is updated by a `GitHub Action`.
That's why, if that action works, there's no need to commit and push `flake.lock` changes.
After an update is complete, it's necessary to rebase local changes onto remote changes.
However, sometimes, there are local uncommitted changes already.
These changes should be `git stash`ed before doing `git rebase`.
After rebasing, they can be `git stash pop`ped to continue the work.

Thus, the process is as follows:

```console
# can be omitted in case of automatic fetches
git fetch
git rebase --autostash
git push
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
