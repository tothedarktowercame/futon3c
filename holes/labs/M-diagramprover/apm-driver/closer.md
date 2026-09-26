CLOSER HOP {{hop_n}} of 3 - apm-{{problem_id}} (sorry-loop closer
discipline; hand of the automated relay).

TARGET: /home/joe/code/apm-lean {{main_lean_path}} at commit
{{base_commit}} - {{sorry_count}} sorry(ies) remaining.

{{boundary_framing}}

--- source at the sorry site ---
{{boundary_excerpt}}
--- end excerpt ---

PROTOCOL:
1. The statement is FROZEN (normalized hash {{statement_hash}}). Do
   not weaken, restate, or restructure the theorem statement in any
   way - closure counts only against the statement as committed. If
   you believe the statement is defective, STOP and report; do not
   repair it. (`None` means no contract was frozen; the statement is
   still not yours to change - report instead.)
2. Close the sorry. Your options, all legitimate: find the missing
   dependency (prior searches were bounded, not exhaustive); prove
   it locally as helper lemmas; or route AROUND it via machinery the
   boundary comment lists as uninvestigated. DESK RESEARCH IS PART
   OF THE JOB: consult LEMMA-INDEX.md at the repo root (1095 already-proved
   helper lemmas with signatures - grep it before re-deriving),
   Mathlib source, PRIOR SOLVED PROBLEMS in problems/*/ (several may be in this problem's mathematical
   neighborhood), git history, and boundary comments in partial
   artifacts. Cite what you reuse in comments. If a consultation
   returns nothing useful, note it and move on. ConstructionTargets/
   is importable and already carries 16 sorry-free reusable modules
   (Rouche, Sinc, YoungConvolution, LusinN, ...) - check it first.
3. Validate continuously: cd /home/joe/code/apm-lean && lake env
   lean {{main_lean_path}}. Final state must be exit 0.
4. Before committing: verbatim #print axioms on the main theorem;
   confirm the statement is untouched; sanity-check non-triviality.
5. Update status.json + proof-outline.md honestly. Commit
   path-limited to the bundle with a problem-specific message.
6. If after genuine sustained attempts the bridge remains open, an
   honest partial with an ENRICHED boundary comment (what YOU tried
   beyond the prior hop, in the same protocol format) is valid -
   state it plainly.

REPORT: final sorry count, axiom output verbatim, statement
untouched confirmation, HOW the bridge was handled (found / proved /
routed-around / still open), commit sha, and a numbered list of
every resource consultation (what, what it returned, used or
discarded with reason).
