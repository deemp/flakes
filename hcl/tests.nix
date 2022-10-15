{ pkgs, system ? builtins.currentSystem }:
let
  inherit (import ./hcl.nix { inherit pkgs; })
    optional_ optional object list b a _lib map_
    mkVariables mkVariableValues mkBlocks mkBlocks_
    string number bool bb qq
    ;
  inherit (pkgs.lib.attrsets) genAttrs;
  # Tests -----------
  # inspired by Terraform docs
  docsMainTF = mkBlocks {
    terraform = b {
      required_providers = a {
        docker = a {
          source = "kreuzwerker/docker";
          version = "~> 2.22.0";
        };
      };
    };
    resource.b = b { d = b { }; };
    variable = a { };
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
    d = { type = optional_ string; };
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
  _mod = x: { try = "try_${x}"; path = "path_${x}"; };

  variablesTF = mkVariables (genAttrs apps (app: {
    type = object {
      DIR = optional string "/app";
      DOCKER_PORT = optional number 80;
      HOST = optional string "0.0.0.0";
      NAME = optional string "renamed_${app}";
      HOST_PORT = number;
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

  # Should add stuff so that we don't refer to non-existing attributes
  mainTF =
    mkBlocks_ (tfvarsTF.__)
      (
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
        // map_ apps (app:
          let app_ = _mod app; in
          {
            resource.docker_image =
              {
                "${app_.try}" = b {
                  name = "dademd/${app}:latest";
                  keep_locally = false;
                };
              };
            locals = b {
              "${app_.path}" = _lib.abspath "${bb _lib.path.root}/../../${app}";
            };
          }
        )
      )
      (__: map_ apps
        (
          app:
          let app_ = _mod app; in
          {
            resource.docker_container."${app_.try}" = b {
              image = __.docker_image."${app_.try}" "image_id";
              name = "${app_.try}";
              restart = "always";
              volumes = a {
                container_path = __.var."${app}".DIR;
                host_path = __.local."${app_.path}";
                read_only = false;
              };
              ports = a {
                internal = __.var."${app}".DOCKER_PORT;
                external = __.var."${app}".HOST_PORT;
              };
              env = [ "HOST=${bb __.var."${app}".HOST}" "PORT=${bb __.var."${app}".DOCKER_PORT}" ];
              host = b {
                host = "localhost";
                ip = __.var."${app}".HOST;
              };
            };
          }
        )
      );
in
{
  inherit mainTF tfvarsTF variablesTF;
  inherit docsMainTF docsTfvarsTF docsVariablesTF;
}
