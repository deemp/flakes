{ lib, config }:
{
  mkWorkflows =
    lib.mapAttrs
      (_: workflow:
        let actions = lib.recursiveUpdate config.actions (workflow.actions or { }); in
        {
          inherit actions;
          jobs =
            lib.mapAttrs
              (_: job: job // {
                steps =
                  map
                    (x:
                      if x?uses && builtins.isAttrs x.uses then
                        x //
                          (
                            let
                              action = builtins.head (builtins.attrNames x.uses);
                              actions' = lib.recursiveUpdate actions x.uses;
                            in
                            actions'.${action}
                          )
                      else x
                    )
                    job.steps;
              })
              workflow.jobs;
        }
      )
      config.workflows;
}
