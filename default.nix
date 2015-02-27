{ mkDerivation, base, blaze-html, HTTP, mtl, old-locale, scotty
, shakespeare, stdenv, tagsoup, text, time
}:
mkDerivation {
  pname = "howoldis";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base blaze-html HTTP mtl old-locale scotty shakespeare tagsoup text
    time
  ];
  description = "Little web app displaying the age of the last NixOS channels";
  license = stdenv.lib.licenses.bsd3;
}
