# PROJECT.md — CohortPipeline adversarial-review fixes

Running understanding + progress doc. (Local teaching aid — add to `.Rbuildignore`.)

## Context / why we're here
A Codex **adversarial review** of the whole `cohort` package returned a
`needs-attention` verdict with 3 material findings, all in
`R/cohort_pipeline.R`, all centred on the **warm-cache replay** machinery
(plus one input-validation gap). All three were **empirically reproduced**
against the real package before any fix (`/tmp/verify_findings.R`).

## Mental model you need (reference)
A cohort `node` stores:
- `status` — per-row integer vector. `0L` = included; otherwise the **absolute
  log step index** that excluded the row.
- `log_entries` — the **full** exclusion log, *including the inherited prefix
  copied from the parent at branch time*. Own steps come after.
- `branched_at_log_len` — parent's log length at branch = the boundary between
  inherited prefix and this cohort's own steps. (Absolute.)
- `branched_at_status` — parent's `status` snapshot at branch time.
- `replay_cursor` — how far warm replay has re-confirmed this node's log.
  Initialised on load to `branched_at_log_len`. **Absolute** index.
- `frozen` — set once a child branches from it or an artifact is attached;
  blocks further `exclude_and_track` (cold path only).

Key tension the bugs live in: **absolute** log index (counts inherited prefix)
vs **branch-local** count (own steps only). `replay_cursor` and `status` step
numbers are absolute; some code treats a cursor as branch-local.

## Findings (problem → why → fix → edge cases)

### Finding 3 — non-logical predicate corrupts provenance  [status: ✅ understand / ✅ fixed / ✅ testthat]
- **Where:** `exclude_and_track()` ~L421-427.
- **Problem:** mask is only length-checked, then `NA`→`FALSE`, then
  `as.logical(mask)`. A non-logical predicate (`"sex"`, `"age"`) sails through.
- **Why it matters:** `as.logical("F")`→`FALSE`, `as.logical(40)`→`TRUE`. Numeric
  predicate silently excluded 2/3 rows; character predicate logged a phantom
  exclusion. The audit trail (the whole point of this package) is corrupted.
- **Fix:** require `is.logical(mask)` (reject matrices/non-atomic too) *before*
  any coercion; error with the same `reason`-tagged message style.
- **Edge cases:** logical `NA` still → `FALSE` (documented). Tests: character,
  numeric, factor, matrix predicate results.

### Finding 1 — stale cached child exclusion retained on divergence  [status: 🟡 understand (root-cause ✓, fix walkthrough deferred) / ✅ fixed / ✅ testthat]
- **Where:** `.invalidate_from()` L970-999, called from `exclude_and_track()`.
- **Problem:** caller passes an **absolute** cursor; `.invalidate_from` treats it
  as **branch-local** (`keep_len <- branched_at_log_len + own_cursor`). For a
  non-root cohort this double-counts the prefix → truncation under-removes (or
  misses entirely), so a changed own-step leaves the stale entry in place and
  appends the new one.
- **Why root worked:** root's `branched_at_log_len == 0`, so absolute == local.
- **Fix:** make `.invalidate_from` cursor **absolute**: `keep_len <- cursor`;
  replay own entries `branched_at_log_len+1 .. cursor`; `replay_cursor <- cursor`.
- **Edge cases:** test a *non-root* cohort changing a later own exclusion.

### Finding 2 — reordered branch point silently keeps stale snapshot  [status: 🟡 understand (walkthrough deferred) / ✅ fixed / ✅ testthat]
- **Where:** `new_cohort()` replay early-return L312-323.
- **Problem:** when the child already exists with a matching parent, it returns
  without checking the parent is at the **same replay position** as when the
  child branched. Moving `new_cohort()` before a parent exclusion keeps the old
  post-exclusion snapshot. The same script run **cold** errors (freeze rule);
  warm replay silently succeeds with wrong data.
- **Fix:** on same-parent replay, require
  `parent$replay_cursor == existing$branched_at_log_len`; otherwise error
  pointing at `$invalidate()` (a correct partial rebuild is impossible — the
  parent's intermediate status was never stored).
- **Edge cases:** benign sibling reorder (same branch point) must NOT trip it.

## Broader context / impact
- These only bite **warm-cache** users (`cache_file=`) editing scripts between
  runs — exactly the incremental-rerun workflow the cache exists for. Silent
  wrong provenance is the worst failure mode for an auditable-cohort tool.
- Finding 3 bites everyone, cache or not.

## Engineering checklist
- ✅ Fix #3, #1, #2 in `R/cohort_pipeline.R`
- ✅ Add tests for each (`tests/testthat/test_CohortPipeline.R`)
- ✅ NEWS entry + version bump 2026.5.10 → 2026.6.23
- ✅ Verify output equivalence (unchanged warm replay == cold; suite green)
- ✅ `devtools::test()` green — `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 79 ]`
- ✅ Committed to `main` (443c8f6) + pushed; CI green (R-CMD-check ✓, pkgdown ✓)

## Teaching thread (resume when you're up for it)
- F1 fix walkthrough (absolute-cursor rewrite of `.invalidate_from`) — not yet quizzed.
- F2 end-to-end (replay_cursor invariant, why error vs rebuild) — not yet covered.
- Nothing blocks the code; this is just the understanding half, on pause.
