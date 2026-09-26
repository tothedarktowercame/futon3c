# A-LABELS-D — where admitted labels would come from in production

claude-12, 2026-09-26. Discovery, read-only: no code, no map edit, nothing run
except read-only counts over futon2 `data/`. Requisition: claude-8, PROOF-2a-PLAN
⟨2⟩2d F1, building on F1a-2b (futon2 bc66c6b1: no production caller of
`cascade-lane` passes `:observation-labels`).

Sources read at futon2 e2d52965f: `src/futon2/aif/observation_admission.clj`,
`observation_rates.clj` (`rates-by-class`, `token-likelihood-rates`, `sourced-rates`),
`check_error_rates.clj`, `observation_checks.clj` (C3, C4), `cascade_sources.clj`
(`observe-facts`), `flight.clj` (`step-observation`, the v2 writer),
`resources/wm/observation-contract.edn`; `holes/labs/wm-contract/proof2/packets/A-S.md`
§1, §4, Revision 2; futon3c `holes/labs/M-wm-wiring/F1b-D.md`.

## Answer in brief

**No reference in production.** The checks' verdicts exist, but nothing
independently establishes their truth. The smallest producer is:

1. an independent recomputation, run as the blinded observer at the tick's observe
   site (`cascade_sources/observe-facts`); and
2. a review, which the flight cannot supply.

Whether a mechanical recomputation may stand as the reference without a review is a
⟨1⟩2 row. **A-S admits it and `observation-admission/admit` does not.** Until one or
the other is settled, G scores with the zero kernel. It is recorded as the unmeasured
default (`:basis :checkable :measurement :absent`), and the decision writes
`{:status :absent :reason :no-measured-rates}`.

Even with admission settled, the records at HEAD would measure only C4 on both cells.
Admitting C3 or C6 labels alone would make `sourced-rates` refuse the whole call (§3).

## 1. What an admitted label record is, and what exists to admit

**The admission steps** (`observation_admission.clj`, pure, no storage):

- **`observer-view`** returns `(select-keys subject [:token :application
  :evidence-pointers])`. The recorded verdict and every author claim are excluded, so
  the observer is blinded by construction.
- **`adjudication`** records `{:observer id :view-digest … :finding f :cutoff {repo sha}}`,
  where `f ∈ #{:present :absent :insufficient :ambiguous :conflicting}`.
- **`review`** records `{:reviewer id :of view-digest :finding f :verdict v}`, where
  `v ∈ #{:concur :dispute :insufficient}`.
- **`admit`** returns `{:status :admitted :label :present|:absent …}` or a typed
  refusal. The refusals are:
  - `:observer-missing` and `:reviewer-missing`;
  - `:authorship-undeclared`: the subject must carry both `:author` and `:enactor`;
  - `:observer-is-author`, `:observer-is-enactor` and `:observer-is-reviewer`;
  - `:cutoff-missing` and `:view-digest-mismatch`;
  - `:review-of-other-adjudication`;
  - `:no-label`, for an insufficient, ambiguous or conflicting finding;
  - `:review-not-concur`.

  Unobserved is never absent.

**The rates input** (`rates-by-class`, `sourced-rates`):

- **Label shape:** each label is `{:token-class c :recorded r :admitted a}`, where `r`
  is the check's recorded verdict (true or false) and `a` is the admitted label.
  (The docstring spells `:recorded` as `:recordd`, a typo.)
- **Subjects:** `{class n}`, the number of subjects in the class, used for coverage.
- **Grouping:** by `:token-class`. The false-pos cell counts admitted-`:absent` labels
  whose recorded verdict was true; the false-neg cell counts admitted-`:present`
  labels whose recorded verdict was false.
- **The R5 lane's option:** `{:labels [...] :subjects {class n}}`, one per problem.
  `measured-a-version` takes the same option keyed by target.

**What exists at HEAD to admit.** Counts are read-only, over futon2 `data/`.

- **Admitted token labels:** none. No file under `data/` has the `:token-class` label
  shape. The 89 files with `:status :admitted` are selection and precision
  admissions, not token labels.
- **v2 step observations** (`:wm/step-observation-v2`, F1b-I): none. The writer
  landed after every stored flight.
- **Stored flights:** `data/wm-interpretations/flights/`, 7 records. They hold no
  checked verdicts: no attempt carries a boolean `[:check :result :observed]`.
- **Tick run records:** `data/wm-runs/tick-run-record-*.edn`, 72 files.
  - They carry the tick's check results as `:observations {:results … :refused …}`,
    written by `cascade_sources/observe-facts` (futon2 `cascade_sources.clj:219`).
  - 1,249 verdicts, but only **29 distinct (subject, verdict) pairs**: C4 23 (10 of
    them false), C3 3 (all true), C6 3 (all false), and C5 0.
  - C8 verdicts carry a differently shaped evidence map, and my count's pattern did
    not match them. `:check :C8` occurs twice in the whole of `data/`.
  - Each result carries the evidence pointers (repo, sha, resolved-sha, path, decl),
    which is exactly what `observer-view` needs. It carries **no `:author` or
    `:enactor`**, so `admit` would refuse `:authorship-undeclared` as the records
    stand.
- **The check ledger** (futon3c `holes/labs/M-futon-seams/exemplar/check-ledger.edn`,
  22 rows / 33 runs; classified in futon2
  `test/fixtures/check-ledger-classification/m-futon-seams-v1.edn`).
  - Its kinds are `:test`, `:grep`, `:validator` and `:layout`, not the token classes
    C3..C8. So its `measured-rates` output (per check kind) is not an input
    `sourced-rates` can read.
  - It measures a different population, and nothing maps a kind to a class.

## 2. The reference truth

**The rule.** A-S §1 and Revision 2 (`check_error_rates.clj`, `truth-kinds`,
`eligible-row?`, `exclusion-reason`): a row is eligible only if its truth kind is in
`#{:constructed-bad-case :later-review :independent-recomputation}`. A
`:self-truthed` row, "whose truth came from the check itself", is excluded, as is a
row with no recorded kind.

**What the flight and the tick have, against that rule:**

- **The after-observation** (`flight/step-observation`: "the after-observation is the
  later reading, so its verdict stands") re-runs the same class's check function over
  the same pointers. It is the check itself, so it is **self-truthed** and excluded.
- **A second check of a different class on the same token** does not exist. Each
  token has one locator and so one class (`:channel` in the v2 record). Checks of
  different classes check different propositions, so neither can be the reference for
  the other.
- **An independent recomputation is producible, for the git-read classes:**
  - C3 is `git cat-file -e` (`observation_checks/check-path-exists`). A second
    mechanism could list the tree at the resolved sha (`git ls-tree`) and look for the
    path.
  - C4 is a line-head prefix match (`decl-present?` over `git show sha:path`). A second
    mechanism could read the file at the sha with the Clojure reader and look for a
    top-level form whose head and name match `:decl`. This is the class where the two
    mechanisms can disagree: a declaration inside a comment block or a string, or
    unusual whitespace.
  - C6 and C5 could be recomputed the same way, over the witness and the bundle.
  - C8 reads the test registry. An independent recomputation there means re-executing
    the test, so it is expensive: a warrant, not a read.
  - These are A-S's third truth kind. The observer would be a mechanism id such as
    `"recompute/C4-reader"`. It sees only `observer-view`, so it is blinded.
- **A later review** is A-S's second truth kind: a reviewer seat reading the target's
  file at the sha. The flight cannot supply one. It needs a seat outside the flight's
  enactor.
- **The target's own text and the later publication observation** are not
  references for C3..C6. The target's text states the want, not whether the path or
  declaration exists. The publication observation is about the run record, not the
  token.

**The ⟨1⟩2 row: A-S and `admit` disagree about what a reference is.**

- A-S makes an independent recomputation, **by itself**, a reference.
- `observation-admission/admit` requires every label to have **both** an observer's
  adjudication **and** a distinct reviewer's `:concur`.
- F1b-D (futon3c, lines 15–21, 55–68) reads admission, blinded review included, as
  **how the reference truth is established** ("Review is part of establishing that
  reference, so it belongs to the measurement of A").
- So under F1b-D's reading, a mechanical recomputation is an adjudication and still
  needs a review. Under A-S's truth kinds, it is enough.
- **Settling by the definitions:**
  - The quantity A estimates is P(recorded verdict | reference), and the estimate needs
    only that the reference is not the check's own output.
  - A second mechanism that is blind to the recorded verdict and does not share the
    check's code meets that condition by construction.
  - Review guards against an observer's *judgement* error. A recomputation makes no
    judgement, but it can share a bug with the check (for example, both resolving the
    wrong sha). The resolved sha is fixed in the evidence pointers before either runs,
    so that shared step is an input, not a judgement.
  - **The reading consistent with both texts:** an `:independent-recomputation`
    adjudication is admitted with a *mechanical* review that checks only that:
    - the recomputation's mechanism differs from the check's (a declared mechanism id
      per class, compared); and
    - the view digest and cutoff match.
  - That review does not re-read the evidence.
  - That is a definition to add to A-S (a Revision 3), not a ruling for Joe.
  - Until it is added, `admit` as written refuses a label with no reviewer, and the
    flight has no reviewer.

## 3. The producer, and what it would yield

**The smallest production source** (one store, one reader, feeding both the R5 lane
and the decision):

- **Writer site:** `cascade_sources/observe-facts` (futon2 `cascade_sources.clj:219`).
  - It is the one place where the recorded verdict (`oc/observe`'s `:results`), the
    evidence pointers and the token's class (through the locator) are all in scope at
    tick time.
  - For each checked token it would run the class's recomputation mechanism on
    `observer-view`, then `adjudication`, the mechanical `review`, and `admit`.
  - It would append `{:token-class c :recorded r :admitted a :subject-key <repo sha
    path decl> :admission <the admit record>}` to a label store, deduplicated by
    `:subject-key` and the check's code sha. Deduplication matters because the same
    subject is re-checked on every tick (1,249 verdicts, 29 subjects).
  - `:author`/`:enactor` must be declared on the subject. For a tick-time check both
    are `:none`, since no agent authored the verdict, but they must be written, or
    `admit` refuses.
- **Store:** `data/wm-labels/admitted-labels.edn` (new), append-only, one record per
  subject and check-code sha. `:subjects {class n}` is the count of distinct located
  subjects per class in the same store.
- **Reader:** one function, e.g. `observation-rates/production-labels`, that reads the
  store and returns `{:labels … :subjects …}`.
  - `cascade-lane`'s R5 step (`war_machine.clj`, around :5897) calls it when
    `:observation-labels` is absent.
  - `cascade-decision-admitted` passes the same value per target to
    `measured-a-version`.
  - Both read one store at one moment, so the lane and the decision cannot disagree.

**What `sourced-rates` would yield over the records at HEAD**, if every recomputation
agreed with its recorded verdict:

- **C4** would get both cells: admitted-`:present` 13 (false-neg 0/13) and
  admitted-`:absent` 10 (false-pos 0/10). It is measured.
- **C3** would get only admitted-`:present` (3); its false-pos cell is unobserved.
- **C6** would get only admitted-`:absent` (3); its false-neg cell is unobserved.
- **C5** has no records, so it keeps the zero kernel as unmeasured.
- **The C3/C6 problem:** `token-likelihood-rates` returns `:unsupported-class` for a
  class with one cell unobserved, and it `reduced`s. So **one** such class refuses the
  **whole** call, and `measured-a-version` would then write `{:status :absent :reason
  :sourcing-refused}`. **Admitting C3 or C6 labels at HEAD's counts makes the lane
  worse, not better.**
- **The fix:** the reader must pass a class only when both of its cells have admitted
  labels, and otherwise leave the class out, so it keeps the unmeasured zero kernel.
  That filter is the reader's job, and it must be typed on the record, not silent.
- **No minimum count here:** `min-count 5` is `check_error_rates`', not
  `observation_rates`'. `rates-by-class` makes a rate from any denominator above zero,
  so 0/1 would be a measured rate of 0.
  - Smoothing exists only through an authorised `prior` (`check-prior`: positive
    α and β with an `:authority`), and production passes nil.
  - A production reader should either require a denominator of at least 5 per cell
    (A-S §2's rule, applied here) or pass A-S's Jeffreys prior, Beta(1/2, 1/2), as an
    authorised prior. Otherwise every measured rate is a raw 0 or 1 from a handful of
    subjects.

## 4. The packets

- **A-S-R3-D** (a definition, before any code): add to A-S that an
  `:independent-recomputation` adjudication with a mechanical review is an admitted
  reference.
  - The mechanical review checks a declared mechanism id that differs from the check's,
    plus view-digest and cutoff equality.
  - It states which classes have a mechanism: C3, C4, C6 and C5 by git read; C8 only by
    re-execution.
  - F1b-D's line on review is amended to match.
- **A-LABELS-I** (one behaviour, futon2): the label writer at
  `cascade_sources/observe-facts`, with the per-class recomputation mechanisms for
  C3 and C4 first, since C4 is the only class HEAD's records measure on both cells.
  Its bad cases:
  - **Self-truthed row:** the "recomputation" is the check function itself (the same
    mechanism id). The mechanical review refuses it, so no label is admitted.
  - **No reviewer:** `admit` refuses `:reviewer-missing`. The test pins that the
    writer never stores a refused admission as a label.
  - **A token counted twice:** the same subject re-checked on a second tick. The store
    holds one label per subject and check-code sha, and `rates-by-class`' denominator
    does not grow.
  - **Undeclared authorship:** no `:author`/`:enactor` on the subject. `admit` refuses,
    and the writer declares `:none` explicitly for tick-time checks.
- **A-LABELS-WIRE-I** (the two readers, one behaviour): `production-labels` feeding
  the R5 lane and `measured-a-version` from the one store.
  - The both-cells filter and the denominator/prior rule are typed on the record.
  - F1a-2b's `live-shaped-the-labels-stop-before-the-lane` flips to "rates present"
    for C4 on a hermetic run record carrying the store. C3/C6/C5 stay recorded as
    unmeasured.
  - Map: the store's writer and reader boxes, and `:r9-measured-a-version`'s reads
    gain the store field.

**Meanwhile, G scores with the zero kernel** for every checkable class. The R5
certificate records it (`:basis :checkable :measurement :absent`), and the decision
writes `[:decision :measured-a] = {:status :absent :reason :no-measured-rates}`.
