SCRIBE PASS - apm-{{problem_id}} chain (drafts + promotion;
attachments proposed, approvals to claude-10; s1-pilot discipline +
the hunger audit, scribe-protocol-hunger-audit.md).

SOURCE: the chain's sessions ({{session_jobs}}), final commit
{{commit_sha}}, the committed artifacts, consultation logs in the
reports and boundary comments, turn evidence in the store.

DRAFT CANDIDATES (your judgment; typical yield 3-5):
- MATH LANE: lemma-locations (esp. any missing-dependency the chain
  hit or closed - revision-scoped absence claims, never
  proven-unroutable), reusable proof patterns (claim n=2 ONLY with a
  documented cross-problem chain), strategy/tactic content with
  compiled witnesses.
- DESK-RESEARCH LANE: consultation practices worth keeping (reuse
  chains, discards-with-reasons), each cited to the consultation log.
- HUNGER AUDIT (required section): collect every memory-tool query
  from the sessions that returned empty or noise (exclude
  degraded-under-load). For each: did the concept get grounded later
  in the chain? Grounded -> the resulting memory MUST carry the
  hungry query's LITERAL vocabulary as tags (demand-side tagging -
  copy the asked terms). Not grounded -> write an open-hunger memory
  (query, proof stage, what was sought).

FORMAT per draft: evidence-store ids, job ids, commit, problem
id(s); honest n= markers; honesty bounds stated. Near-duplicate
check against the store FIRST; say when something duplicates.

PROMOTION in the same pass: memory entries with retrievable tags
(math vocabulary from the content AND the demand-side tags from the
hunger audit); attachments asserted to justified patterns (curated
table with per-memory reasons), left PROPOSED, with the exact
approval calls listed for claude-10 in APPROVALS.md (author !=
reviewer). Read-back verify each write. Output:
{{output_dir}}/ - one md per draft + README + APPROVALS.md.
GATES: store reads read-only until promotion; promotion via the
established write paths; commit path-limited to the output dir.
