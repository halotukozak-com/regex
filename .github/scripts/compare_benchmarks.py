#!/usr/bin/env python3
"""Renders a Markdown table comparing two JMH `-rf json` result files.

Usage: compare_benchmarks.py <baseline.json> <candidate.json>
"""

import json
import sys

REGRESSION_THRESHOLD_PCT = 10.0


def load(path: str) -> dict[str, dict]:
    with open(path) as f:
        entries = json.load(f)
    return {e["benchmark"]: e for e in entries}


def fmt_score(entry: dict) -> str:
    metric = entry["primaryMetric"]
    return f'{metric["score"]:.3f} {metric["scoreUnit"]}'


def main() -> None:
    baseline_path, candidate_path = sys.argv[1], sys.argv[2]
    baseline = load(baseline_path)
    candidate = load(candidate_path)
    names = sorted(set(baseline) | set(candidate))

    print("| Benchmark | main | current | Δ |")
    print("|---|---|---|---|")
    for name in names:
        short_name = name.rsplit(".", 2)[-2] + "." + name.rsplit(".", 1)[-1]
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

    print()
    print(
        f"Δ is candidate vs. `main`; positive means the metric's value grew "
        f"(slower for time-based benchmarks, faster for throughput ones). "
        f"🔴/🟢 mark a ≥{REGRESSION_THRESHOLD_PCT:.0f}% regression/improvement — "
        f"informational only, single-sample on a shared runner, not a merge gate."
    )


if __name__ == "__main__":
    main()
