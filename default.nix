{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./lambda.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  stackmachine = haskellPackages.callPackage ./nix/stackmachine.nix {};
  drv = haskellPackages.callPackage f { inherit stackmachine; };

in

  drv
