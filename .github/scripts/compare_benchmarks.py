#!/usr/bin/env python3
"""Renders a Markdown table comparing two JMH `-rf json` result files.

Usage: compare_benchmarks.py <baseline.json> <candidate.json>
"""

import json
import sys

REGRESSION_THRESHOLD_PCT = 10.0


def bench_key(entry: dict) -> str:
    """Disambiguates @Param'd benchmarks — JMH gives every param combination the same
    `benchmark` name, so keying on that alone silently collapses them onto each other."""
    params = entry.get("params") or {}
    if not params:
        return entry["benchmark"]
    param_str = ",".join(f"{k}={v}" for k, v in sorted(params.items()))
    return f'{entry["benchmark"]}[{param_str}]'


def load(path: str) -> dict[str, dict] | None:
    """Returns None (rather than raising) when the file is missing or empty - the
    "Run benchmarks on main" CI step uses continue-on-error and can legitimately not
    produce a result file, e.g. when the PR's bench/ harness references an API that
    only exists on the PR's own branch. The caller renders that as an explicit
    "baseline unavailable" note instead of crashing this comparison outright."""
    try:
        with open(path) as f:
            content = f.read().strip()
    except FileNotFoundError:
        return None
    if not content:
        return None
    entries = json.loads(content)
    return {bench_key(e): e for e in entries}


def fmt_score(entry: dict) -> str:
    metric = entry["primaryMetric"]
    return f'{metric["score"]:.3f} {metric["scoreUnit"]}'


def main() -> None:
    baseline_path, candidate_path = sys.argv[1], sys.argv[2]
    baseline = load(baseline_path)
    candidate = load(candidate_path)
    if candidate is None:
        print(f"⚠️ No results for the current branch at `{candidate_path}` - nothing to report.")
        return
    baseline_available = baseline is not None
    if not baseline_available:
        print(
            "⚠️ No `main` baseline to compare against - the bench/ harness on this PR likely "
            "references an API that doesn't exist on `main` yet (a new benchmark for new "
            "functionality, or an updated signature for a perf change), so benchmarking `main` "
            "with it didn't compile. Showing this branch's numbers on their own instead:\n"
        )
        baseline = {}
    names = sorted(set(baseline) | set(candidate))

    print("| Benchmark | main | current | Δ |")
    print("|---|---|---|---|")
    for name in names:
        method, _, params = name.partition("[")
        short_name = method.rsplit(".", 2)[-2] + "." + method.rsplit(".", 1)[-1]
        if params:
            short_name += "[" + params
        base = baseline.get(name)
        cand = candidate.get(name)
        if base is None:
            print(f"| `{short_name}` | — | {fmt_score(cand)} | 🆕 new |")
            continue
        if cand is None:
            print(f"| `{short_name}` | {fmt_score(base)} | — | 🗑️ removed |")
            continue

        base_score = base["primaryMetric"]["score"]
        cand_score = cand["primaryMetric"]["score"]
        higher_is_better = base.get("mode") == "thrpt"
        pct = (cand_score - base_score) / base_score * 100
        signed_pct = pct if higher_is_better else -pct

        if signed_pct <= -REGRESSION_THRESHOLD_PCT:
            marker = "🔴"
        elif signed_pct >= REGRESSION_THRESHOLD_PCT:
            marker = "🟢"
        else:
            marker = "⚪"

        print(f"| `{short_name}` | {fmt_score(base)} | {fmt_score(cand)} | {marker} {pct:+.1f}% |")

    if baseline_available:
        print()
        print(
            f"Δ is candidate vs. `main`; positive means the metric's value grew "
            f"(slower for time-based benchmarks, faster for throughput ones). "
            f"🔴/🟢 mark a ≥{REGRESSION_THRESHOLD_PCT:.0f}% regression/improvement — "
            f"informational only, single-sample on a shared runner, not a merge gate."
        )


if __name__ == "__main__":
    main()
