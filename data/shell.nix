let
  np = import <nixpkgs> {};
in
  np.mkShell { buildInputs = [np.jq]; }
