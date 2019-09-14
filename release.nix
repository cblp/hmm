let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          aeson-tiled = haskellPackagesNew.callPackage ./nix/aeson-tiled-0.0.0.nix { };
          co-log = haskellPackagesNew.callPackage ./nix/co-log-0.3.0.0.nix { };
          typerep-map = haskellPackagesNew.callPackage ./nix/typerep-map-0.3.2.nix { };
          game-network = haskellPackagesNew.callPackage ./game-network { };
          project = haskellPackagesNew.callPackage ./project.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project = pkgs.haskellPackages.project;
  }
