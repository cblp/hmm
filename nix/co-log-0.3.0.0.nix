{ mkDerivation, ansi-terminal, base, bytestring, chronos
, co-log-core, containers, contravariant, directory, filepath
, hedgehog, markdown-unlit, mtl, stdenv, stm, text, transformers
, typerep-map
}:
mkDerivation {
  pname = "co-log";
  version = "0.3.0.0";
  sha256 = "0a6a5bbdadff762aae73b59dd67fddf6a7934c73826cd0489214048d077b3d67";
  revision = "2";
  editedCabalFile = "12h6rgqgqd1wcm2ahl6b3wglsgq4ynjacrz9m0wkyry3k0jwzd8f";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring chronos co-log-core containers
    contravariant directory filepath mtl stm text transformers
    typerep-map
  ];
  executableHaskellDepends = [
    base co-log-core mtl text typerep-map
  ];
  executableToolDepends = [ markdown-unlit ];
  testHaskellDepends = [ base co-log-core hedgehog ];
  doCheck = false;
  homepage = "https://github.com/kowainik/co-log";
  description = "Composable Contravariant Comonadic Logging Library";
  license = stdenv.lib.licenses.mpl20;
}
