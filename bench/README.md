# Benchmarks

JMH microbenchmarks for the hot paths of this library: `Regex` construction (`alt`/`concat`/
`repeat`, which drive `Set`-based ACI normalization and `hashCode`), the Brzozowski-derivative
machinery in `Subset` (`subset`/`isEmpty`/`derive`), and `CharSet.contains`.

This directory only compiles under `--jmh` — it's excluded (`--exclude "bench/**"`) from every
other `scala-cli` invocation (`compile`, `test`, `doc`, `publish`) because `@Benchmark` needs
`jmh-core` on the classpath, which `--jmh` injects automatically.

## Running locally

```sh
# everything
scala-cli run --jmh . --exclude "test/**"

# one class, faster iteration while developing a benchmark
scala-cli run --jmh . --exclude "test/**" -- SubsetBenchmark -f 1 -wi 2 -i 2 -w 300ms -r 300ms
```

## CI

On every PR, the `benchmark` job in `.github/workflows/ci.yml` runs this suite against the PR
branch and against `main` (same `bench/` harness both times, via a `git worktree`, so only
`regex/` differs) and posts a comparison table to the job summary. It's informational only — a
shared GitHub-hosted runner is too noisy for single-sample numbers to gate a merge on.
