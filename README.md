# AoC19-Haskell

To profile a certain day, run:

```bash
stack run <day> --profile -- +RTS -p
```

If you want to do memory profiling, use `-h` instead of `-p`.

To generate a flamegraph svg, run:

```bash
ghc-prof-flamegraph AoC19-Haskell-exe.prof
```

To do these two in one step, use

```bash
./profile.sh
```
