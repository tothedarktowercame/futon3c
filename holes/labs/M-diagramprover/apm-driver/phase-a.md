PHASE A ONLY - memory reconnaissance for apm-{{problem_id}} (do NOT
begin solving; a second instruction will follow separately).

Use the memory store to look for hints that can make your eventual
solution of apm-{{problem_id}} efficient. The problem files are at
{{bundle_path}} (problem.md, informal-solution.md, problem.tex) -
read them to know what you are looking for, but do NOT write any
Lean or start solving.

Tools that take a text query: psr_search (query string; searches the
pattern library and returns scored candidates with hooks to reviewed
attached memories - follow the hooks). Tools that take metadata:
memory_search (tags / type / author / since), pattern_memory (tags).
Tag-based lookup works for recently-added memories; use the
mathematical vocabulary of the problem and of your expected
obstacles.

Report: a numbered list of the hints you found - memory/pattern ids,
one line each on why it might help THIS problem, PLUS a plan-impact
field per hint, exactly one of:
  SUPPLIED - this gave me a route/lemma I did not already have;
  CONFIRMED - I had already derived this from the problem materials,
    the hint corroborates it;
  UNCLEAR - cannot honestly distinguish the above.
This field is graded later against the artifact; an honest CONFIRMED
is worth more than an optimistic SUPPLIED. Note every query
that returned nothing relevant (query text + empty/noise) - these
are recorded as demand signals. If the honest total is "nothing
useful," say exactly that and stop - a well-searched empty result is
a fully valid outcome of this task. Do not force relevance and do
not begin the proof.
