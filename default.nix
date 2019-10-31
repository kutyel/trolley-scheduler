{ mkDerivation, base, cond, hspec, random, random-shuffle, stdenv
}:
mkDerivation {
  pname = "trolley-scheduler";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base cond random random-shuffle ];
  testHaskellDepends = [ base cond hspec random random-shuffle ];
  description = "Haskell program to schedule trolleys and shifts randomly!";
  license = stdenv.lib.licenses.mit;
}
