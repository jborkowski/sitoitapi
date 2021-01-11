{ pkgs ? import ./haskell-pkgs.nix
}:

let
  hsPkgs = pkgs.haskell-nix.stackProject {
    compiler-nix-name = "ghc8102";
    modules = [];
    src = pkgs.fetchFromGitHub {
      owner = "ndmitchell";
      repo = "hlint";
      # 3.2.6 release
      rev = "fca7ac0b20134fa085556766a7f82a75b524c550";
      sha256 = "1xffw93mjqy4nfarzjdqav32hq7pjwcwrphdg4by39002hcq61bh";
    };
  };
in hsPkgs.hlint.components.exes.hlint
