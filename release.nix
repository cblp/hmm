let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          aeson-tiled = haskellPackagesNew.callPackage ./nix/aeson-tiled-0.0.0.nix { };
          project = haskellPackagesNew.callPackage ./project.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project = pkgs.haskellPackages.project;
  }
