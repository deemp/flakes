{ lib }:
rec
{
  convertUses = x:
    x
    //
    (
      lib.optionalAttrs
        (x?uses && builtins.isAttrs x.uses)
        (
          let uses = builtins.head (builtins.attrValues x.uses); in
          {
            uses = uses.name or null;
            with' = (x.with' or { }) // (uses.with' or { });
          }
        )
    );

  removeNulls = lib.filterAttrsRecursive (_: value: value != null);

  resolveWorkflows = { config, utils, stepsPipe ? [ ] }:
    lib.mapAttrs
      (_: workflow:
        workflow
        //
        (
          let actions = lib.recursiveUpdate config.actions (workflow.actions or { }); in
          {
            inherit actions;
            jobs =
              lib.mapAttrs
                (_: job: job // {
                  steps =
                    lib.pipe job.steps (
                      [
                        (x:
                          if x?uses && builtins.isAttrs x.uses then
                            x
                            //
                            (
                              let
                                action = builtins.head (builtins.attrNames x.uses);
                                actions' = lib.recursiveUpdate actions x.uses;
                              in
                              {
                                uses = actions'.${action};
                              }
                            )
                          else x
                        )
                        (map utils.convertUses)
                      ]
                      ++
                      stepsPipe
                    );
                })
                workflow.jobs;
          }
        )
      )
      config.workflows
  ;

  # builtins.removeAttrs x [ "actions" "path" ];
  cleanWorkflow = workflow:
    (builtins.removeAttrs workflow [ "actions" "path" ])
    //
    {
      jobs =
        lib.mapAttrs
          (
            _: value: value // {
              steps = lib.pipe value.steps [
                (map convertUses)
                (map removeNulls)
                (map (x: if (x.with' or { }) == { } then builtins.removeAttrs x [ "with'" ] else x))
              ];
            }
          )
          workflow.jobs;
    };

  cleanWorkflows = lib.mapAttrs (_: cleanWorkflow);
}
