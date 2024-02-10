{ lib }:
let
  nonEmptyListOf = elemType: lib.types.nonEmptyListOf elemType // {
    # TODO remove after switching to newer nixpkgs
    substSubModules = m: nonEmptyListOf (elemType.substSubModules m);
  };

  attrsOrFunc = x: arg: if builtins.isAttrs x then x else x arg;
in
lib.recursiveUpdate lib {
  inherit attrsOrFunc;
  types = {
    inherit nonEmptyListOf;
  };
}
