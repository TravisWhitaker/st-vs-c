let pinned-nixpkgs = builtins.fetchGit
    {
        url = "https://github.com/nixos/nixpkgs";
        ref = "master";
        rev = "8dbca5e3ca41787b3af3d3695062577f3b855a21";
    };
in with import pinned-nixpkgs {};
runCommand "st-vs-c-env"
{
    buildInputs =
        let thisghc = haskell.packages.ghc865.ghcWithPackages
            (p: [ p.cabal-install
                  p.ghcid
                ]);
        in [ thisghc
             binutils
           ];
} ""
