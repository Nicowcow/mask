{ writeScriptBin
, haskellPackages
, haskell
}:
{ haskellPackages = haskellPackages.extend
    (haskell.lib.packageSourceOverrides { makefile = ../.; });
}
