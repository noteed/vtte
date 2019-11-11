{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "vtte";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  description = "A text editor";
  license = stdenv.lib.licenses.bsd3;
}
