{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, directory, dirstream, lens, old-locale, pipes, pipes-attoparsec
, pipes-bytestring, pipes-parse, pipes-safe, restless-git, sandi
, stdenv, system-fileio, system-filepath, text, text-format
, text-icu, thyme, time, vector
}:
mkDerivation {
  pname = "restless-mail";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers directory dirstream
    lens old-locale pipes pipes-attoparsec pipes-bytestring pipes-parse
    pipes-safe restless-git sandi system-fileio system-filepath text
    text-format text-icu thyme time vector
  ];
  homepage = "https://less.rest/mail";
  description = "The perfect mail client";
  license = stdenv.lib.licenses.agpl3;
}
