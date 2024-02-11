{ lib }:
let
  nonEmptyListOf = elemType: lib.types.nonEmptyListOf elemType // {
    # TODO remove after switching to newer nixpkgs
    substSubModules = m: nonEmptyListOf (elemType.substSubModules m);
  };
in
lib.recursiveUpdate lib {
  types = {
    inherit nonEmptyListOf;
  };
}
