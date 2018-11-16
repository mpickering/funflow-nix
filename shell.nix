let
  np = import <nixpkgs> {};
in
  np.mkShell { buildInputs = [ np.haskell.packages.ghc822.ghc
  np.haskell.packages.ghc861.cabal-install np.simple-server
  np.haskell.packages.ghc843.haskell-ci ]; }
