# Terrafix

Writing in Terraform [configuration syntax](https://www.terraform.io/language/syntax/configuration) often violates the [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) principle due to lack of named types and other features. Let's **fix** it!

This repo contains a `Nix` [eDSL](https://wiki.haskell.org/Embedded_domain_specific_language) which can be translated into [HCL](https://github.com/hashicorp/hcl), a language used by[Terraform](https://www.terraform.io/). In other words, we can use the power of Nix to generate understandable HCL expressions.

## Related works

- [terranix](https://github.com/terranix/terranix) - a popular tool that generates `terraform.json`
- [nixform](https://github.com/brainrake/nixform) - another tool that generates `terraform.json` from a Nix eDSL
- [xinomoft](https://github.com/kreisys/xinomorf) - a pretty outdated tool that generates terraform configurations

## Repo contents

- [language definition](.nix/hcl.nix)
- [sample expressions](.nix/test-data.nix) used in tests
- [scripts](.nix/tf-tools.nix) to generate `.tf` files from the given `Nix` expressions

## Prerequisites

- Knowledge of Nix syntax
  - [Nix Language](https://nixos.org/manual/nix/unstable/language/index.html)
    - more specifically, [Data Types](https://nixos.org/manual/nix/unstable/language/values.html#attribute-set)
  - [Learn Nix in Y minutes](https://learnxinyminutes.com/docs/nix/)

- (Optionally) Understanding of Flakes
  - [Flakes](https://github.com/br4ch1st0chr0n3/the-little-things/blob/7f9703e2d7f8a3cbf39344cddb7868421b4fcc6d/README.md#L17) info

## Sample test

This is a part of [test expressions](test-data.nix). This case is implemented [here](https://github.com/br4ch1st0chr0n3/devops-labs/blob/80978ecd1826548904e442e5fb428f2127748be7/.nix/terraform/docker.nix#L62). I put the snippets int `let in` blocks to improve highlighting

There are 2 apps: `app_purescript` and `app_python`. Each of them has a server written in corresponding language, and these servers show the current time in a browser. Their Docker containers were pushed to Docker Hub. On a host, these apps are under the directories `./app_purescript` and `./app_python`. In Docker containers, each app's code is placed under the `/app` directory.

There is some data that is needed to run the apps. This data is described in `dockerVariables`. Here, notable fields are: `DOCKER_PORT` - internal port inside a Docker container, `HOST` - the address of a host on which to run the container, `NAME` - a new name for a container, `PORT` - the external port. It is worth to mention that all fields of the object in `type` are the same for both apps, except for the `HOST_PORT`. Also, the names of their variables differ. We can use `Nix` functions to use the same template inside `dockerVariables` to declare the variables for both apps:

```nix
let
  appPurescript = "app_purescript";
  appPython = "app_python";
  apps = [ appPurescript appPython ];
  
  # x may have a custom __toString
  _mod = x: { _ = "${toString x}"; try = "try_${x}"; path = "path_${x}"; renamed = "renamed_${x}"; };

  # we can apply a modifier _mod to consistently use the forms of `app`
  dockerVariables = mkVariables (modifyMapMerge apps _mod (app:
    {
      "${app._}" = {
        type = object {
          DIR = optional string "/app";
          DOCKER_PORT = optional number 80;
          HOST = optional string "0.0.0.0";
          NAME = optional string "${app.renamed}";
          HOST_PORT = number;
        };
      };
    }));
in
```

When producing HCL, we'll get such code in `variables.tf`:

```hcl
variable "app_purescript" {
  type = object({
    DIR         = optional(string, "/app")
    DOCKER_PORT = optional(number, 80)
    HOST        = optional(string, "0.0.0.0")
    HOST_PORT   = number
    NAME        = optional(string, "renamed_app_purescript")
  })
}
variable "app_python" {
  type = object({
    DIR         = optional(string, "/app")
    DOCKER_PORT = optional(number, 80)
    HOST        = optional(string, "0.0.0.0")
    HOST_PORT   = number
    NAME        = optional(string, "renamed_app_python")
  })
}
```

Next, we need the values. We can supply just the necessary data, namely `HOST_PORT`-s, and a set of variables. The mapping between the set of names in `dockerTfvars` and the set of names in `dockerVariables` should be injective. The missing values will be generated automatically according to their variables' types. Thus, if necessary, we can supply the empty set to `mkVariableValues`. In this case, the Nix expression will be:

```nix
let
  dockerTfvars = mkVariableValues dockerVariables {
    "${appPython}" = {
      HOST_PORT = 8002;
    };
    "${appPurescript}" = {
      HOST_PORT = 8003;
    };
  };
in
```

These `tfvars` will be mapped over the supplied set of `variables` to produce a set of values in `terraform.tfvars`:

```hcl
app_purescript = {
  DIR         = "/app"
  DOCKER_PORT = 80
  HOST        = "0.0.0.0"
  HOST_PORT   = 8003
  NAME        = "renamed_app_purescript"
}

app_python = {
  DIR         = "/app"
  DOCKER_PORT = 80
  HOST        = "0.0.0.0"
  HOST_PORT   = 8002
  NAME        = "renamed_app_python"
}
```

Furthermore, we will be able to use the accessors like `var.app_python.HOST_PORT` to get the same expression (`var.app_python.HOST_PORT`) in Terraform code.

Finally, we compose the main Terraform file.

There is a couple of rules. We should place a block `A` before the block `B` if:

1. `B` depends on `A`, e.g., uses its accessor

   - Here, `resource.docker_container.${app.try}` accesses `var.${app._}.DIR;`. Hence, this `var` should be supplied by previos expressions.

1. `A` should be rendered before `B`

   - Notice that in the corresponding Terraform code, the `terraform` block is placed before locals and `resource.docker_image`.
   - On the other hand, as `resource.docker_image` and `locals` are given in the same set, `locals` precedes the `resource` blocks. This happens because `locals` is lexicographically smaller than `resource` and because `Nix` orders the elements of a set lexicographically.

Again, we use the same template to declare the `docker_image` and both `locals`. These `locals` assume that `main.tf` is at `./terraform/docker/main.tf`. So, the whole expression is:

```nix
let
  dockerMain = with _lib;
    mkBlocks_ dockerTfvars.__
      {
        terraform = b {
          required_providers = b {
            docker = a {
              source = "kreuzwerker/docker";
              version = "~> 2.22.0";
            };
          };
        };
      }
      (__: with __; modifyMapMerge apps _mod (app: {
        resource.docker_image = {
          "${app.try}" = b {
            name = "dademd/${app._}:latest";
            keep_locally = false;
          };
        };
        locals = b {
          # bb means x: "${x}"
          "${app.path}" = abspath [ "${bb path.root}/../../${app._}" ];
        };
      }))
      # this __ is to pass the variables created in previous expressions into this one
      # alternatively, here, we can write __.var."${app}".DIR;
      (__: with __; modifyMapMerge apps _mod (app: {
        resource.docker_container.${app.try} = b {
          image = docker_image.${app.try} "image_id";
          name = app.try;
          restart = "always";
          volumes = b {
            container_path = var.${app._}.DIR;
            host_path = local."${app.path}";
            read_only = false;
          };
          ports = b {
            internal = var.${app._}.DOCKER_PORT;
            external = var.${app._}.HOST_PORT;
          };
          env = [ "HOST=${bb var.${app._}.HOST}" "PORT=${bb var.${app._}.DOCKER_PORT}" ];
          host = b {
            host = "localhost";
            ip = var.${app._}.HOST;
          };
        };
      }));
in
```

In fact, we can supply the missing accessors via a sequence of strings, as in `image = docker_image.${app.try} "image_id";`. This is useful when a block was declared by a provider, and we don't want to rewrite it and supply as a Nix expression.

The corresponding `main.tf`:

```hcl
terraform {
  required_providers = {
    docker = {
      source  = "kreuzwerker/docker"
      version = "~> 2.22.0"
    }
  }
}
locals {
  path_app_purescript = abspath("${path.root}/../../app_purescript")
  path_app_python     = abspath("${path.root}/../../app_python")
}
resource "docker_image" "try_app_purescript" {
  keep_locally = false
  name         = "dademd/app_purescript:latest"
}
resource "docker_image" "try_app_python" {
  keep_locally = false
  name         = "dademd/app_python:latest"
}
resource "docker_container" "try_app_purescript" {
  env = [
    "HOST=${var.app_purescript.HOST}",
    "PORT=${var.app_purescript.DOCKER_PORT}"
  ]
  host {
    host = "localhost"
    ip   = var.app_purescript.HOST
  }
  image = docker_image.try_app_purescript.image_id
  name  = "try_app_purescript"
  ports = {
    external = var.app_purescript.HOST_PORT
    internal = var.app_purescript.DOCKER_PORT
  }
  restart = "always"
  volumes = {
    container_path = var.app_purescript.DIR
    host_path      = local.path_app_purescript
    read_only      = false
  }
}
resource "docker_container" "try_app_python" {
  env = [
    "HOST=${var.app_python.HOST}",
    "PORT=${var.app_python.DOCKER_PORT}"
  ]
  host {
    host = "localhost"
    ip   = var.app_python.HOST
  }
  image = docker_image.try_app_python.image_id
  name  = "try_app_python"
  ports = {
    external = var.app_python.HOST_PORT
    internal = var.app_python.DOCKER_PORT
  }
  restart = "always"
  volumes = {
    container_path = var.app_python.DIR
    host_path      = local.path_app_python
    read_only      = false
  }
}
```

Overall, we can notice that definitely a lot of expressions have almost complete duplicates. We believe that whenever this is the case, such expressions should be generated from DRY Nix code.

## Run tests

Enter the repo

```sh
git clone https://github.com/br4ch1st0chr0n3/terrafix
```

Running the tests will write into `.tf` files the expressions generated from [test-data](test-data.nix)

```sh
nix run .#runTests
```

To run individual tests, see the outputs of

```sh
nix flake show
```

And then run

```sh
nix run .#testDocs
```

## Disclaimer

The repo owner is not an experienced Terraform user. That's why, he may have missed some Terraform's features that can make one's code DRY.

The repo owner likes Nix and likes generating configs using this language.

## Limitations

There are some HCL constructs that aren't yet supported. To name a few:

- [ ] [Conditional expressions](https://developer.hashicorp.com/terraform/language/expressions/conditionals)
  - They require making conditional accessors or merging accessors for objects in both options
- [ ] Possibly, some [built-in](https://developer.hashicorp.com/terraform/language/functions) functions
  - For now, they're all constructed using simple language constructs. It's highly likely (and I haven't yet checked) that some of them use more advanced constructs
- [ ] Output variables
  - Need a function that will extract them from blocks
- [ ] Indices and [splat expressions](https://www.terraform.io/language/expressions/references#references-to-resource-attributes)
  - can pass them as special objects inside lists to accessors, like `some_list [_ast_]` to mean `some_list[*]`
- [ ] `__functor` like in accessors to allow continuing the expressions after functions like `values(aws_instance.example)[*].id`
- [ ] `for` [expressions and conditionals](https://www.terraform.io/language/expressions/for)

## Pushing conventions

1. Commit and push

1. Wait a couple of minutes for a GH action to complete updating `flake.lock`-s

1. `git fetch` and `git rebase`

## Substituters and keys

There are `extra-trusted-public-keys`, `extra-trusted-public-keys` in [flake.nix](./flake.nix). If a substituter like `cachix` fails, comment out the lines containing its address
