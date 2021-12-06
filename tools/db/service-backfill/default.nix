{ mkDerivation, base, brig-types, cassandra-util, conduit, hpack
, imports, lens, lib, optparse-applicative, tinylog, types-common
, unliftio
}:
mkDerivation {
  pname = "service-backfill";
  version = "1.0.0";
  src = /home/axeman/workspace/wire-server/tools/db/service-backfill;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base brig-types cassandra-util conduit imports lens
    optparse-applicative tinylog types-common unliftio
  ];
  prePatch = "hpack";
  description = "Backfill service tables";
  license = lib.licenses.agpl3Only;
}