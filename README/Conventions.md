# Conventions

## Dev tools

If you'd like to provide dev tools for your flake, store them in a separate flake under `./nix-dev`.

Then, users will be able to call them like

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
Sometimes, there are local uncommitted changes.
These changes should be `git stash`ed before doing `git rebase`.
After rebasing, they can be `git stash pop`ped to continue the work.

Thus, the process is as follows:

```sh
git commit -m "some message"
git stash
git push
# wait some time for locks to be updated and these changes to be fetched
git rebase
git stash pop
```

## Scripts

Each script should have:

- `meta.description` attribute written in `CommonMark`
  - This description will be rendered in devshells
  - This description should be a single-line brief description of this script
- `meta.longDescription` attribute written in `Markdown`
  - This description is used to generate `man` pages for this script
  - The format of a `longDescription` should be recognizable by `pandoc`
Here's a sample [Markdown input](https://pandoc.org/demo/pandoc.1.md)

## Docs

Function docs are left as comments in code

## READMEs

Each flake's `README` assumes that the directory of its `flake.nix` is your current working directory in a terminal
