# Nix eDSL for Terraform HCL

Writing in Terraform [configuration syntax](https://www.terraform.io/language/syntax/configuration) often violates the [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) principle due to lack of named types.

This repo contains a `Nix` [eDSL](https://wiki.haskell.org/Embedded_domain_specific_language) which can be translated into HCL, a language that [Terraform](https://www.terraform.io/) uses. In other words, you may use the power of Nix to generate understandable HCL expressions without having to use the built-in Terraform [functions](https://stackoverflow.com/a/69134570).

## Contents

- [language definition](hcl.nix)
- [sample expressions](test-data.nix) used in tests
- [scripts](tf-tools.nix) to generate `.tf` files from given `Nix` expressions

## Run tests

Running the tests will write into `.tf` files the expressions generated from [test-data](test-data.nix)

```sh
nix run .#writeTests
```

### Sample test

This is a part of [test expressions](test-data.nix).

Input:

```nix
  appPurescript = "app_purescript";
  appPython = "app_python";
  apps = [ appPurescript appPython ];
  
  # x may have a custom __toString
  _mod = x: { _ = "${toString x}"; try = "try_${x}"; path = "path_${x}"; renamed = "renamed_${x}"; };

  # we can apply a modifier _mod to consistently use the forms of `app`
  variablesTF = mkVariables (modifyMapMerge apps _mod (app:
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

  tfvarsTF = mkVariableValues variablesTF {
    "${appPython}" = {
      HOST_PORT = 8002;
    };
    "${appPurescript}" = {
      HOST_PORT = 8003;
    };
  };

  # should place expr A before and separately from expr B:
  # if B depends on A (uses its accessors)
  # if we want A to be rendered before B
  mainTF = with _lib;
    mkBlocks_ tfvarsTF.__
      {
        terraform = b {
          required_providers = a {
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
      }
      ))
      # this __ is to pass variables created in previous expressions into this one
      # alternatively, here, we can write __.var."${app}".DIR;
      (__: with __; modifyMapMerge apps _mod (app: {
        resource.docker_container.${app.try} = b {
          image = docker_image.${app.try} "image_id";
          name = app.try;
          restart = "always";
          volumes = a {
            container_path = var.${app._}.DIR;
            host_path = local."${app.path}";
            read_only = false;
          };
          ports = a {
            internal = var.${app._}.DOCKER_PORT;
            external = var.${app._}.HOST_PORT;
          };
          env = [ "HOST=${bb var.${app._}.HOST}" "PORT=${bb var.${app._}.DOCKER_PORT}" ];
          host = b {
            host = "localhost";
            ip = var.${app._}.HOST;
          };
        };
      }
      ));
```

Output:

`tfvars.tf`:

```nix
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

`variable.tf`:

```nix
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

`main.tf`:

```nix
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

## Limitations

There are some HCL constructs that aren't yet supported. To name a few:

- [Conditional expressions](https://developer.hashicorp.com/terraform/language/expressions/conditionals)
  - require making conditional accessors or merging accessors for objects in both options
- Possibly, some [built-in](https://developer.hashicorp.com/terraform/language/functions) functions
  - For now, they're all constructed using simple language constructs. It's highly likely (and I haven't yet checked) that some of them use more advanced constructs
