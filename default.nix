{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, dirstream, old-locale, pipes, pipes-attoparsec, pipes-bytestring
, pipes-parse, pipes-safe, servant-server, stdenv, system-fileio
, system-filepath, text, thyme, time, wai, wai-cors, warp
}:
mkDerivation {
  pname = "restless-mail";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers dirstream old-locale
    pipes pipes-attoparsec pipes-bytestring pipes-parse pipes-safe
    servant-server system-fileio system-filepath text thyme time wai
    wai-cors warp
  ];
  homepage = "https://less.rest/mail";
  description = "The perfect mail client";
  license = stdenv.lib.licenses.agpl3;
}
