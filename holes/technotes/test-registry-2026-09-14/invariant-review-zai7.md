# Invariant-lane review: test_registry.clj (zai-7, 2026-09-14)

Scope: futon3c src/futon3c/test_registry.clj at 88e6b06f (atop 6930dfb2,
076746d6). Full focused read; independent of the demo round-trip
(already reviewed separately).

## Verdict: ACCEPT — invariant-discipline conformant, three non-blocking notes

## What the code gets right (checked, not assumed)

1. **The honesty trinity in the ns docstring is enforced in the body**:
   SHA integrity is not authentication and not an adequacy proof
   (adequacy is the reviewer's own field); missing warrants never
   prohibit running tests (warrant-not-gate — Joe's ruling, held in
   code, not just prose).
2. **Chain integrity**: cycle detection on read (chain-cycle);
   explicit predecessor ids — no mutable global registry head across
   federations; intent records survive interrupted runs while failed
   appends leave no warrant; the review chain composes
   intent→run→reviewer-intent→sample→review, each link parent-pinned.
3. **Path safety**: canonical-path containment against the repo root
   (path-outside-repository), symlink refusal rather than silent
   traversal (symlink-scope-unsupported), 256MB dependency-directory
   bound.
4. **R9 enforced mechanically**: `not= reviewer (:author run)` —
   independence is checked, never assumed; full-scope commands
   required (an author-narrowed `-v` command is refused — the author
   cannot pre-shrink the reviewer's scope); a spot-check whose var
   runs zero or multiple tests fails `sufficient?` (outcome
   :selective-execution-failed, not a silent pass).
5. **Environment discipline**: declared allow-list {LC_ALL LANG TZ}
   only — no arbitrary env capture; mismatch refusals carry
   expected/observed diffs (the round-trip's finding 2, fixed); JVM
   metadata probed under the recorded alias with metadata-only
   override, classpath-equality enforced.
6. **Exit-code discipline**: -main exits 1 on any refusal; refusal
   records are typed throughout (C446 standard).

## Non-blocking notes (recorded, none require change now)

- **N1 — artifact locality**: logs live on local disk
  (storage/test-registry/artifacts) with sha verification at check
  time, but are not store-resident. A motivated author could
  regenerate log+results consistently. The docstring already disclaims
  (SHA ≠ authentication); when tamper-resistance beyond honesty is
  needed, artifacts should append through the evidence boundary too.
- **N2 — dynamic-var concurrency**: `*test-environment*` is
  process-global; concurrent reviews in one JVM could interleave.
  Single-use CLI today makes this latent; note for any server-side
  embedding.
- **N3 — runner coupling**: spot-check appends `-v <var>` to the
  recorded command, coupling to the cognitect runner's CLI. A
  different runner fails typed (:command-failed) — acceptable
  surfacing, but a future multi-runner registry needs a per-runner
  spot syntax.

## Cross-references

Rulings honored: warrant-not-gate; one-test spot-check default;
independence mechanical. Patterns exercised: ◈ (records carry
warrant), ⚖ (review = judgment + selective execution, not rerun),
⌁ (typed refusals throughout). The registry is the second coherence
keeper of its family (evidence hygiene), and this review is its
independent witness — proposal ≠ witness preserved end to end.
