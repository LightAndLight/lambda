{ mkDerivation, base, bound, deriving-compat, parsers, stackmachine
, stdenv, trifecta
}:
mkDerivation {
  pname = "lambda";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bound deriving-compat parsers stackmachine trifecta
  ];
  license = stdenv.lib.licenses.bsd3;
}
