{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc843" }:

let
  inherit (nixpkgs) pkgs;
  f = { mkDerivation, base, gdp, ghc-prim, primitive, stdenv }:
        mkDerivation {
          pname = "st2";
          version = "0.1.0.0";
          src = ./.;
          libraryHaskellDepends = [ base gdp ghc-prim primitive ];
          homepage = "https://github.com/chessai/st2.git";
          description = "shared heap regions between local mutable state threads";
          license = stdenv.lib.licenses.bsd3;
        };
  
  haskellPackages = pkgs.haskell.packages.${compiler};
  
  
  drv = haskellPackages.callPackage f {};

in
  drv
