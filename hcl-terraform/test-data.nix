{ pkgs, system ? builtins.currentSystem }:
let
  inherit (import ./hcl.nix { inherit pkgs; })
    optional_ optional object list b a _lib mapMerge
    mkVariables mkVariableValues mkBlocks mkBlocks_
    string number bool bb qq modifyMapMerge
    ;
  inherit (pkgs.lib.attrsets) genAttrs;
  # Tests -----------
  # inspired by Terraform docs
  docsMainTF = mkBlocks {
    terraform = b {
      required_providers = b {
        docker = a {
          source = "kreuzwerker/docker";
          version = "~> 2.22.0";
        };
      };
    };
    resource.type.b = b { d = b { }; };
    resource.type.a = with _lib; b {
      t = tomap [{ a = "b"; c = "d"; }];
      s = abs [ (ceil [ (floor [ (-3.5) ]) ]) ];
    };
  };

  docsType = list (object {
    name = string;
    enabled = optional bool true;
    website = optional
      (object {
        index_document = optional string "index.html";
        error_document = optional string "error.html";
        routing_rules = optional_ string;
      })
      { };
  });

  docsVariablesTF = mkVariables {
    a = { type = object { a = number; }; };
    b = { type = optional string "b"; };
    buckets = { type = docsType; };
    c = { type = optional (optional_ (object { c = optional string ""; })) { }; };
    d = { type = optional_ string; default = "hey"; };
  };

  docsTfvarsTF = mkVariableValues docsVariablesTF {
    a = { a = 100; };
    b = "c";
    buckets = [
      {
        name = "production";
        website = {
          routing_rules =
            ''
              [
                {
                  "Condition" = { "KeyPrefixEquals": "img/" }
                  "Redirect"  = { "ReplaceKeyPrefixWith": "images/" }
                }
              ]
            '';
        };
      }
      {
        name = "archived";
        enabled = false;
      }
      {
        name = "docs";
        website = {
          index_document = "index.txt";
          error_document = "error.txt";
        };
      }
    ];
  };

  # Docker

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
      # this __ is to pass variables created in previous expressions into this one
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
{
  inherit mainTF tfvarsTF variablesTF;
  inherit docsMainTF docsTfvarsTF docsVariablesTF;
}
