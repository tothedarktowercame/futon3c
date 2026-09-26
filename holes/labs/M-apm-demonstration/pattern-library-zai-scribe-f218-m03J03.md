# Zai Scribe pattern library — f218 / m03J03

Authored because the parse-error self-correction mined from this frame had
no existing parent pattern in the reviewed mathematics libraries (the f30
exemplar "parse errors masquerade as tactic failures" was never authored as
a pattern file).

## math-formalization/parse-errors-masquerade-as-tactic-failures

- **Trigger:** A scratch or rewrite step fails with a parse error whose
  reported position points at a line that is syntactically fine ("unexpected
  token; expected ,"). The cost signature: several iterations editing the
  wrong line before the real cause is found.
- **Move:** Before treating a parse error as a tactic/elaboration problem,
  re-check the notation grammar of the failing line and its neighbours —
  binder/notation spelling (e.g. `∑ n in s` vs `∑ n ∈ s`), nested tactic
  blocks inside `rw [lem _ s (fun n hn => by ...)]` brackets, and adjacent
  doc comments. When a tactic block nested inside rewrite brackets fails to
  parse at the closing `)]`, hoist it into a standalone `have` first; when
  two doc comments sit adjacent, move the helper above the theorem's
  docstring.
- **Why it works:** The parser reports the first token it cannot consume,
  which can be far from the offending construct; elaboration-level reasoning
  about the goal is wasted effort until the file parses.

## math-formalization/spell-a-constructor-lemmas-implicit-arguments-when-unification-stalls

- **Trigger:** Applying a `@[simps!]`- or `@[simp]`-generated application
  lemma to a term you built yourself: `rfl` fails, and `rw`/`simp only [lem]`
  makes no progress, with errors mentioning `IsScalarTower` metavariables or
  higher-order pattern failure (`?f φ`).
- **Move:** Give the lemma application with every implicit argument spelled
  out — base field, normed spaces, domain opens, exponent — syntactically
  identical to the construction site, then close with a plain `exact` of the
  fully-applied lemma.
- **Why it works:** High-order unification of the bare partially-applied
  form cannot infer the function argument from the expected type; supplying
  it explicitly removes the metavariable.
