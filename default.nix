# default.nix
{ # Fetch the latest haskell.nix and import its default.nix
haskellNix ? import (builtins.fetchTarball
  "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") { }

  # haskell.nix provides access to the nixpkgs pins which are used by our CI,
  # hence you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003

  # haskell.nix provides some arguments to be passed to nixpkgs, including some
  # patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

  # import nixpkgs with overlays
, pkgs ? import nixpkgsSrc nixpkgsArgs }:
let
  apc-project = pkgs.haskell-nix.project {
    index-state = "2020-11-20T00:00:00Z";
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "absurd-paste-client";
      src = ./.;
    };
    # For `cabal.project` based projects specify the GHC version to use.
    compiler-nix-name = "ghc884"; # Not used for `stack.yaml` based projects.
  };
in {
  project = apc-project.absurd-paste-client.components.exes.apc;
  apc-project = apc-project;
}
