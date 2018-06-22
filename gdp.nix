{ mkDerivation, base, fetchgit, lawful, stdenv }:
mkDerivation {
  pname = "gdp";
  version = "0.0.0.2";
  src = fetchgit {
    url = "https://github.com/matt-noonan/gdp.git";
    sha256 = "1hanx8kqibhfvn7g48dgpgdw4d066qla6pdqs0k5nwx5s7m2k0rp";
    rev = "a327fb1f4d4c4825618ba7db1bee7c852de28f48";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base lawful ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/matt-noonan/gdp#readme";
  description = "Reason about invariants and preconditions with ghosts of departed proofs";
  license = stdenv.lib.licenses.bsd3;
}
