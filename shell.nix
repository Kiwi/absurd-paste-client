{ haskellNix ? import (builtins.fetchTarball
  "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003
, nixpkgsArgs ? haskellNix.nixpkgsArgs, pkgs ? import nixpkgsSrc nixpkgsArgs }:
let hsPkgs = (import ./default.nix { inherit pkgs; }).apc-project;
in hsPkgs.shellFor {
  # Include only the *local* packages of your project.
  packages = ps: with ps; [ absurd-paste-client ];

  # Builds a Hoogle documentation index of all dependencies,
  # and provides a "hoogle" command to search the index.
  withHoogle = true;

  # You might want some extra tools in the shell (optional).

  # Some common tools can be added with the `tools` argument
  tools = {
    cabal = "3.2.0.0";
    hlint = "3.2.1";
    haskell-language-server = "0.5.1";
  };
  # See overlays/tools.nix for more details

  # Some you may need to get some other way.
  buildInputs = with pkgs.haskellPackages; [ brittany ghcid ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
