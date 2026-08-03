# S3 scribe packet (cohort 2, a96J01, zai-1)

Ready to dispatch **when the S3 job completes**. The scribe mines completed
turn-round evidence, so it runs after the runner lands, not during.

- runner session: `zai-bec940299024470eb815607f8b13b650`
- job: `invoke-1785739461339-861-868a3d9d`
- problem: `a96J01` (fresh, cohort-2 row S3)
- scribe seat: `codex-2` (per M-zai-learning-loop; author ≠ runner by construction)

Dispatch with `scripts/agency_send.py --from claude-12 --to codex-2 --kind bell`,
prompt on stdin via a quoted heredoc, then PARK on the returned job-id.

---

SCRIBE PASS — cohort-2 S3 (a96J01), batch/per-session, three lanes.

Corpus: the Evidence Landscape, NOT buffer scrapes. Read the turn-round
evidence for session `zai-bec940299024470eb815607f8b13b650` from the store at
`http://127.0.0.1:7073/api/alpha/evidence` (entries carry
`:evidence/session-id` and `:evidence/tags [:transcript :turn-round :zaif]`).
The runner's self-talk is the `:text` of each round; its actions are `:calls`.

Distil into typed memories via the fixed `memory_record` path, three lanes:

1. Solve-lane — from the final summary and the compiling states: lemma-location
   and proof-pattern memories (problem-class -> lemma/tactic). This session's
   material includes the search for harmonic-series divergence lemmas
   (`not_summable_one_div_natCast`, `summable_nat_add_iff`,
   `Real.not_summable_one_div_nat_succ`) and how the runner settled the
   n=0 / (n+1) indexing question.
2. Arc-lane — error -> fix spans in the turn-round stream: scoped rewrite rules
   in the six-rule shape (scope / before / after / level / confidence /
   evidence-ids).
3. Trajectory-lane — expensive or failed stretches: negative and cost memories,
   process rules. Note the two `run_readonly` rejections
   ("appears destructive (readonly peripheral)") if they cost rounds.

Every memory: name / body / subjects (store-enforced), turn-round evidence ids
in the provenance, level, confidence by instance count. These land
scribe-asserted; promotion to `:reviewed` stays an operator act.

Held-out discipline: BPM (`bpm-*`) subjects are never mined and never appear as
`:mined-from`. a96J01 is APM, so it is mineable.

Report back: memories recorded (ids + lane), anything you refused to record and
why, and whether the session's typed-register marks were parseable
deterministically (the meta-lane claim). Bell claude-12 back with a summary.
