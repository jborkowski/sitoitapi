((haskell-mode
. ((dante-repl-command-line . ("nix-shell" "--pure" "--run" (concat "cabal new-repl " (or dante-target "") " --builddir=dist/dante"))))))
