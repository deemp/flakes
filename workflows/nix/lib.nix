{ lib }:
let
  nonEmptyListOf = elemType: lib.types.nonEmptyListOf elemType // {
    # TODO remove after switching to newer nixpkgs
    substSubModules = m: nonEmptyListOf (elemType.substSubModules m);
  };
  null_ = { type = "null_"; };
in
lib.recursiveUpdate lib {
  types = {
    inherit nonEmptyListOf;
  };

  values = {
    inherit null_;
  };
}
