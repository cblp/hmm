{ mkDerivation, aeson-tiled, apecs, apecs-gloss, base, co-log
, data-default-class, directory, doctest, exceptions, filepath
, game-network, gloss, gloss-juicy, hedgehog, lens, linear, mtl
, natural-transformation, optparse-applicative, random, sdl2
, sdl2-mixer, stdenv, tasty, tasty-discover, tasty-expected-failure
, tasty-hedgehog, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "hmm";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson-tiled apecs apecs-gloss base co-log data-default-class
    directory exceptions filepath gloss gloss-juicy lens linear mtl
    natural-transformation random sdl2 sdl2-mixer text transformers
    unordered-containers vector
  ];
  executableHaskellDepends = [
    aeson-tiled apecs apecs-gloss base co-log data-default-class
    directory exceptions filepath game-network gloss gloss-juicy lens
    linear mtl natural-transformation optparse-applicative random sdl2
    sdl2-mixer text transformers unordered-containers vector
  ];
  testHaskellDepends = [
    aeson-tiled apecs apecs-gloss base co-log data-default-class
    directory doctest exceptions filepath gloss gloss-juicy hedgehog
    lens linear mtl natural-transformation random sdl2 sdl2-mixer tasty
    tasty-expected-failure tasty-hedgehog text transformers
    unordered-containers vector
  ];
  testToolDepends = [ tasty-discover ];
  description = "Micro machines";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
