Haskell code that prints all winning board configurations of the original
version of the game ["Do not find the fox."](https://donotfindthefox.com)

There are 2018016 possible configurations. 255304 of them are a win.

Instead of representing the board as an ADT, I represent it as a Word32.
Additionally, I use Word16s to represent each combination of positions of the
letters F, O, and X on the board. I did this because I wanted to play around
with bit representations. It might have had a performance benefit, but I didn't
measure that, since I was focused on data representation.

# Building

You'll need an installation of the [Glasgow Haskell
Compiler](https://www.haskell.org/downloads/) on your PATH. Other compilers
might work, but I haven't tested them. I'm using GHC version 9.4.8 on Windows,
at the time of writing.

Then run:

- Unoptimized: `./build.sh`
- Optimized: `./build-opt.sh`

Getting dumps from optimization passes:

```sh
ghc -O2 fox.hs -o fox-opt -ddump-simpl -fforce-recomp > dist/opt-asm.txt
ghc -O2 fox.hs -o fox-opt -ddump-stg-final -fforce-recomp > dist/opt-stg.txt
ghc -O2 fox.hs -o fox-opt -ddump-cmm -fforce-recomp > dist/opt-cmm.txt
ghc -O2 fox.hs -o fox-opt -ddump-asm -fforce-recomp > dist/opt-asm.txt
```

# Perf

Measuring using hyperfine on Windows, `fox` compiled on Windows, on commit e47dc9c908115dbe9b86e1fc36fb9828634b3fb0.

optimized:

```
Benchmark 1: fox-opt.exe
  Time (mean ± σ):     969.2 ms ±  33.8 ms    [User: 342.2 ms, System: 12.1 ms]
  Range (min … max):   916.5 ms … 1016.0 ms    10 runs
```

unoptimized:

```
Benchmark 1: fox.exe
  Time (mean ± σ):     22.800 s ±  4.927 s    [User: 11.168 s, System: 0.045 s]
  Range (min … max):   17.921 s … 32.406 s    10 runs
```
