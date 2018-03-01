{ mkDerivation, base, fetchgit, lens, stdenv, vector }:
mkDerivation {
  pname = "stackmachine";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/LightAndLight/stackmachine";
    sha256 = "0ljc3bawjapbjm2jbqcqja85aha592dlbqh4sin40xj8b2j18i4n";
    rev = "36e103939191b5030d65d4740b8d40778eca5bee";
  };
  libraryHaskellDepends = [ base lens vector ];
  license = stdenv.lib.licenses.bsd3;
}
