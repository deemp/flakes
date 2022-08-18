# Nix. Tutorials and Notes

## Other

1. What is `expression`, `closure`, `derivation`?
    *  [expression, closure, derivation](https://medium.com/scientific-breakthrough-of-the-afternoon/closure-vs-derivation-in-the-nix-package-manager-ec0eccc53407)

1. Where are nix configs stored?
    * [here](https://nixos.wiki/wiki/Flakes#Permanent)

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

1. `.follows` input - [src](https://discourse.nixos.org/t/nix-flake-to-aggregate-and-concurrently-update-some-dependencies/10774/8?u=br4ch1st0chr0n3)

1. A tutorial on [Haskell.nix](https://github.com/Gabriella439/haskell-nix)

1. I didn't find a way to use stack without `shell.nix` in the `cachix` project.

1. How to use `nix-build` with a `default.nix` that returns multiple derivations?

1. How to give aliases to built executables?

1. How to use a flake in another flake? Should I build it somehow?

1. How to add commands for execution in child directories