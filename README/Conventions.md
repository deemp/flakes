# Conventions

## READMEs

In this repo, each flake's `README.md` assumes that the directory of its `flake.nix` is your current working directory in a terminal.

## Prerequisites

To de-duplicate the docs, one of the top sections of each `README.md` here contains a `Prerequisites` section. This section provides the links to other sections that possibly contain the relevant docs. Sometimes, you'll need to traverse multiple `Prerequisites` before you achieve the relevant docs.

## Dev tools

If you'd like to provide dev tools for your flake, store them in a separate flake under `./nix-dev`.

Then, users will be able to call them like:

```sh
nix develop nix-dev/
```

## Pushing to a remote repo

All flakes in this repo access some other flakes in this repo via `GitHub` URLs.
That's why, if a change in a flake `A` here should be propagated into a flake `B`, it's necessary to update `B`'s `flake.lock`.
One can update `B`'s `flake.lock` this way iff `A`'s changes are pushed to `GitHub`.
Whenever there's a push to the remote `GitHub` repo, `B`'s `flake.lock` is updated by a `GitHub Action`.
That's why, there's no need to commit and push `flake.lock` changes.
After an update is completed, it's necessary to rebase the local changes onto remote changes.
However, sometimes, there are local uncommitted changes already.
These changes should be `git stash`ed before doing `git rebase`.
After rebasing, they can be `git stash pop`ped to continue the work.

Thus, the process is as follows:

```sh
git add some-file
git commit -m "some message"
git stash
# can be omitted in case of automatic fetches
git fetch
git rebase
git push
git stash pop
```

## Docs

Each derivation that evaluates to an executable should have:

- `meta.description` attribute written in `CommonMark`
  - This description will be rendered in devshells
  - It should be a single-line brief description of this executable
- `meta.longDescription` attribute written in `Markdown`
  - This description is used to generate `man` pages for executables
  - The format of a `longDescription` should be recognizable by `pandoc`
    - Here's a sample [input](https://pandoc.org/demo/pandoc.1.md)
