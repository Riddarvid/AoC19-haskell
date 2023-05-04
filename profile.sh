#! /usr/bin/zsh
stack run $1 --profile -- +RTS -p
ghc-prof-flamegraph AoC19-Haskell-exe.prof