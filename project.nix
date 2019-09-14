{ mkDerivation, aeson-tiled, apecs, apecs-gloss, base, bytestring
, gloss, gloss-juicy, linear, mtl, network, random, stdenv
, transformers, vector
}:
mkDerivation {
  pname = "hmm";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson-tiled apecs apecs-gloss base bytestring gloss gloss-juicy
    linear network random vector
  ];
  executableHaskellDepends = [
    aeson-tiled apecs apecs-gloss base gloss linear mtl random
    transformers vector
  ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
