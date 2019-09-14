{ mkDerivation, aeson-tiled, apecs, apecs-gloss, base, co-log
, game-network, gloss, gloss-juicy, linear, mtl
, natural-transformation, random, stdenv, text, transformers
, vector
}:
mkDerivation {
  pname = "hmm";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson-tiled apecs apecs-gloss base gloss gloss-juicy linear random
    text vector
  ];
  executableHaskellDepends = [
    aeson-tiled apecs apecs-gloss base co-log game-network gloss linear
    mtl natural-transformation random text transformers vector
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
