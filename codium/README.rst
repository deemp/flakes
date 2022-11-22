Codium flake
============

Set up VSCodium with extensions and executables on its ``PATH`` in
several lines of Nix code

See `Prerequisites <https://github.com/deemp/flakes#prerequisites>`__

Contribute
----------

.. code:: console

   nix develop
   codium .


Troubleshooting
---------------

GitHub Personal Access Token (PAT) for VS Codium extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Create a ``classic`` PAT with permissions:
   ``read:user, repo, user:email, workflow``
-  Supply it to extensions

Missing binaries on PATH in VSCodium
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Case: VSCodium doesn't have the binaries provided in
``runtimeDependencies`` (like
`here <https://github.com/deemp/flakes/blob/7bab5d96658007f5ad0c72ec7805b5b4eb5a83dd/templates/codium/generic/flake.nix#L33>`__)
on ``PATH``:

1.  You need to repair VSCodium's derivation (see `Repair a
    derivation <#>`__)
2.  Assumptions:

    -  current directory is ``DIR``
    -  there is a ``DIR/flake.nix``
    -  VSCodium is given as a derivation ``codium``, like
       `here <https://github.com/deemp/flakes/blob/53b2e4d8bb5fb34c50da1b45f06622bffdb9b7bf/templates/codium/generic/flake.nix#L25>`__

3.  In ``DIR/flake.nix``, set ``packages.default = codium;``, like
    `here <https://github.com/deemp/flakes/blob/53b2e4d8bb5fb34c50da1b45f06622bffdb9b7bf/templates/codium/generic/flake.nix#L37>`__
4.  ``Check``:

    1. ``cd DIR``
    2. Start VSCodium: ``nix run .#``
    3. Open a VSCodium terminal
    4. ``echo $PATH`` there
    5. It doesn't contain ``/bin`` dirs of specified
       ``runtimeDependencies``

5.  Close:

    -  devshells with this VSCodium
    -  VSCodium itself

6.  Open a new terminal, ``cd DIR``
7.  Run ``nix store repair .#``
8.  Make a ``Check`` to verify binaries are on ``PATH``
9.  If still no, continue
10. Remove direnv profiles:

    -  ``cd DIR && rm -rf .direnv``

11. Restart your OS
12. ``nix store gc`` - collect garbage in Nix store -
    `man <https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-store-gc.html>`__
13. Again, make a ``Check``
