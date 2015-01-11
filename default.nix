with import <nixpkgs> { };
with haskellPackages;

cabal.mkDerivation (self: {
   pname = "howoldis";
   version = "0.0.1";
   src = ./.;
   buildDepends = [ scotty wai warp blazeHtml shakespeare tagsoup HTTP ]
                    ++ lib.optionals lib.inNixShell [ reserve hlint ];
   buildTools = [ cabalInstall ];
   isExecutable = true;
   isLibrary = false;
 })
