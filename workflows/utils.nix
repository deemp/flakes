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
            uses = uses.name;
            with' = (x.with' or { }) // (uses.with' or { });
          }
        )
    )
  ;

  # builtins.removeAttrs x [ "actions" "path" ];
  cleanWorkflow = workflow:
    let
      cleanJobs =
        lib.mapAttrs
          (
            _: value: value // {
              steps = lib.pipe value.steps [
                (map (lib.filterAttrsRecursive (_: value: value != null)))
                (map convertUses)
                (map (x: if x.with' == { } then builtins.removeAttrs x [ "with'" ] else x))
              ];
            }
          )
          workflow.jobs;
      clean = (builtins.removeAttrs workflow [ "actions" "path" ]) // { jobs = cleanJobs; };
    in
    clean;

  clean = lib.mapAttrs (_: cleanWorkflow);
}
