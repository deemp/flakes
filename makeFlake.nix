{
  # original inputs
  inputs
, # systems to generate outputs for
  systems ? (import ./source-flake).flake-utils.lib.defaultSystems
, # outputs per system
  perSystem ? ({ inputs, system }: { })
, # raw outputs
  raw ? (inputs: { })
}:
let
  # a function where a user can override original inputs
  inputs_ = inputs;
  customOutputs = inputs__:
    let inputs = inputs_ // inputs__; in
    inputs.flake-utils.lib.eachSystem systems (system: perSystem { inherit inputs system; })
    // raw inputs
    # save overriden inputs
    // { inherit inputs; };
in
# default outputs with original inputs
customOutputs { } // { outputs = customOutputs; }
