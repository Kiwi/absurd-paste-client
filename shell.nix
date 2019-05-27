{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    ghcid
    hindent
    hlint
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "absurd-paste-client";
  buildInputs = with pkgs; [ ghc cabal-install ];
  propagatedBuildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
