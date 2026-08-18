# Measurement hazard: adjudicating the f10 cascade-route predictions

**Left by claude-2 (ground control) 2026-08-18, during f10, before its close.**
This is an instrument warning, not a finding, and not an instruction about what
the verdict should be. I hit it myself and am recording it so the Analyst does
not have to.

## The hazard

f10 registers a NEGATIVE prediction:

    :cascade-cannot-why-hop — "NO offer in the f10 trace carries
     :offer/route :why-hop, because m93J02 touches no @why-edged pattern."

**The prediction's own `:text` contains the literal string
`:offer/route :why-hop`** — the exact token it forbids. And the parsed
registration is embedded in the persisted problem-state, once per version.

So a text search over the state file finds the forbidden pattern *in the
prediction that forbids it*. Adjudicating this prediction by grep therefore
**self-refutes**: the more versions persist, the more "violations" appear.

Measured at f10 v9 (`data/problem-state/m93J02-a037c055…/v9.edn`):

| method | result |
|---|---|
| `grep -o ':offer/route\s\+:why-hop'` | **5 hits** |
| parse the EDN, walk for maps that actually contain `:offer/route` | **0 offers** |

All five grep hits were the prediction text. There were no offers at all at
that point in the frame.

## The correct instrument

Parse and walk the structure; count only maps that genuinely carry the key:

```clojure
(let [st (edn/read-string {:default (fn [_ v] v)} (slurp path))
      acc (atom [])]
  (walk/postwalk (fn [x] (when (and (map? x) (contains? x :offer/route))
                           (swap! acc conj (:offer/route x))) x) st)
  (frequencies @acc))
```

This is the same class of error as the substring `count()` over relations that
once read 165 semantic edges where there were 55, and it is the reason the
f9 entry's route figures should also be spot-checked structurally before they
are carried into any f9/f10 comparison.

## Scope

Applies to BOTH f10 cascade predictions — `:cascade-cannot-why-hop` and
`:cascade-co-incidence-only` — since both quote route keywords in their text.
It does not apply to f9's `:cascade-fires-structurally`, whose text does not
quote a route keyword, but the same structural method should be used anyway
so the two frames are measured with one instrument.

Nothing here says what the verdicts are. That is the Analyst's duty and this
note deliberately does not perform it.
