{ mkDerivation, base, bytestring, network, serialise, stdenv }:
mkDerivation {
  pname = "game-network";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring network serialise ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
