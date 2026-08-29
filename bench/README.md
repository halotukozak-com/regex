# Benchmarks

JMH microbenchmarks for the hot paths of this library: `Regex` construction (`alt`/`concat`/
`repeat`, which drive `Set`-based ACI normalization and `hashCode`), the Brzozowski-derivative
machinery in `Subset` (`subset`/`isEmpty`/`derive`), `CharSet.contains`, and the compiled
`TokenMatcher` DFA (`matchAt`/`findFirst`/`compile`).

Its own Mill module (`bench`), a `JmhModule` depending on `jvm` — never compiled into, tested
with, or published as part of the library.

## Running locally

```sh
# everything
./mill bench.runJmh '.*'

# one class, faster iteration while developing a benchmark
./mill bench.runJmh 'SubsetBenchmark' -f 1 -wi 2 -i 2 -w 300ms -r 300ms
```

## CI

On every PR, `.github/workflows/benchmark.yml` runs this suite against the PR branch and against
`main` (same `bench/` harness both times, via a `git worktree`, so only `src/` differs) and
posts a comparison table to the job summary and as a PR comment. It's informational only — a
shared GitHub-hosted runner is too noisy for single-sample numbers to gate a merge on.
