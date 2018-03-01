{ mkDerivation, base, bound, deriving-compat, stackmachine, stdenv
}:
mkDerivation {
  pname = "lambda";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bound deriving-compat stackmachine
  ];
  license = stdenv.lib.licenses.bsd3;
}
