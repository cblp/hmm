{ mkDerivation, aeson-tiled, apecs, apecs-gloss, base, gloss
, linear, random, stdenv, vector
}:
mkDerivation {
  pname = "hmm";
  version = "0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson-tiled apecs apecs-gloss base gloss linear random vector
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
