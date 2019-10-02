{ mkDerivation, base, random-shuffle, stdenv }:
mkDerivation {
  pname = "trolley-scheduler";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base random-shuffle ];
  description = "Haskell program to schedule trolleys and shifts randomly!";
  license = stdenv.lib.licenses.mit;
}
