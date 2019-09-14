{ mkDerivation, aeson, base, bytestring, containers, fetchgit
, hspec, hspec-discover, stdenv, text, vector
}:
mkDerivation {
  pname = "aeson-tiled";
  version = "0.0.0.1";
  src = fetchgit {
    url = "https://github.com/schell/aeson-tiled.git";
    sha256 = "0x4hgcl395fz3lg85kw159mqxyhzqs825nx1ijngv59hqb2mpnms";
    rev = "617821e3c33a25b2de74a5e63f7f88f71ea40abd";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers text vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base bytestring hspec hspec-discover
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/schell/aeson-tiled#readme";
  description = "Aeson instances for the Tiled map editor";
  license = stdenv.lib.licenses.bsd3;
}
