{ pkgs ? (import <nixpkgs> { config = { cabal.libraryProfiling = true; }; })
, haskellPackages ? pkgs.haskellPackages
}:

pkgs.lib.overrideDerivation (pkgs.lib.callPackageWith (pkgs // haskellPackages) ./default.nix { }) (x : {
  buildInputs = x.buildInputs ++ [ haskellPackages.cabalInstall ];
})
