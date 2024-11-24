# Building

- Unoptimized: `./build.sh`
- Optimized: `./build-opt.sh`

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
