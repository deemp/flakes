flakes
======

Nix flakes for tools that I use

Prerequisites
-------------

-  `Nix prerequisites <./README/NixPrerequisites.md>`__
-  `Conventions <./README/Conventions.md>`__
-  `Troubleshooting <./README/Troubleshooting.md>`__

Contents
--------

-  `codium <./codium/README.rst>`__ - set up VSCodium with binaries on
   ``PATH`` and extensions
-  `devshell <./devshell/README.md>`__ - Easily create a CLI to your
   ``devShells``
-  `drv-tools <./drv-tools/flake.nix>`__ - convenience tools for working
   with derivations
-  `env2json <./env2json/README.md>`__ - convert ``.env`` to ``.json``
-  `flakes-tools <./flakes-tools/flake.nix>`__ - convenience tools for
   working with flakes and pushing them to
   `cachix <https://www.cachix.org/>`__
-  `json2md <./json2md/README.md>`__ - convert ``JSON`` to ``Markdown``

   -  Can be combined with ``builtins.toJSON`` and used to generate
      ``.md`` docs from Nix expressions -
      `example <https://github.com/deemp/devops-labs/blob/0ae9881ab58b99f114aaf21cb5cad85f2ce37e40/.nix/write-configs.nix#L26>`__

-  `language-tools <./flakes-tools>`__ - tools for languages that I use
-  `manager <./manager/README.md>`__

   -  automate routine actions in projects having multiple unrelated
      ``Haskell`` modules

-  `source-flake <./source-flake/>`__ - pinned flakes

   -  used to have the same flake inputs in my flakes

-  `templates <./templates/>`__ - Nix flake templates that can be used
   to initialize new projects

   -  see the `Templates <#templates>`__ section

Other flakes
------------

-  `hpack <https://github.com/deemp/hpack>`__ - ``0.35.0``, until it’s
   on ``nixpkgs``
-  `refmt <https://github.com/deemp/refmt>`__ - ``HCL`` <-> ``JSON`` <->
   ``YAML`` converter
-  `terrafix <https://github.com/deemp/terrafix>`__ - generate
   ``Terraform`` files from DRY ``Nix`` expressions
-  `try-phi <https://github.com/objectionary/try-phi>`__ - online
   interactive translator and interpreter of EO and 𝜑-calculus
-  `devops-labs <https://github.com/deemp/devops-labs>`__ - ``Nix`` in
   devops projects
-  `blockchain <https://github.com/deemp/blockchain>`__ - A repo for
   ``BDLD`` course
-  `scala <https://github.com/deemp/scala>`__ - task solutions and notes
   on Scala courses
-  `lens-examples <https://github.com/deemp/lens-examples>`__ - examples
   of ``Haskell`` lens usage

Templates
---------

This repo provides several templates. Learn how you can use them.

-  Templates `Prerequisites <./README/NixPrerequisites#templates>`__
-  Dev tools `Conventions <./README/Conventions.md#dev-tools>`__ - to
   decide where to put the templates
