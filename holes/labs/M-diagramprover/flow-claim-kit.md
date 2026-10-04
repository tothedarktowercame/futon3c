# Flow-claim kit — taking M-diagramprover to the next project

Working kit for **rung 0** (flow maps) of the ladder in
`holes/missions/M-diagramprover.md` §Generalisation (2026-10-02). Use it
when a project needs "is it wired?"; climb the ladder (typed string
diagrams, rewriting, the mixed regime, causal, proofs) when it needs "does
it behave?", as in the Agency skeleton. The proposals in §4–§5 marked as
mission items are now unnumbered follow-ups. It is written from four
applications, not from first principles:

| # | Target | Facts came from | What was reused from futon3c | Record |
|---|---|---|---|---|
| 1 | APM problem peripheral (futon3c) | declared map + textual conformance | engine + all checks (origin) | `holes/labs/M-apm-demonstration/problem-wiring.edn` |
| 2 | Proof peripheral (futon3c) | declared map + conformance | same functor and checks, no new code | `proof-wiring.edn` |
| 3 | War Machine belief path (futon2) | declared map + conformance, cross-repo | same functor and checks | `wm-wiring.edn` |
| 4 | Lean LCNF compiler (`lean-wiring`) | **extracted** from Lean's own environment | questions, vocabulary, planted-defect discipline; **no code** | M-diagramprover §Second-opinion review |

Row 4 is why this kit exists: the method transferred and the code did not.
The kit separates the two so that the next project can take both.

---

## 1. The unit of inquiry: a flow claim

A flow claim says *what moves where* through a pipeline:

- **boxes**: the stages (peripheral phases, compiler passes, services, gates);
- **wires**: the values that pass between them (fields, records, environment
  extensions, files, messages);
- for each box, the wires it **reads** and **writes**;
- optionally, an **order** (phase chain, pass sequence) and **sites** (where
  in source each box lives).

Everything below checks, compares or lifts a flow claim. If the project has
no pipeline whose stages hand values to each other, this kit is the wrong
tool.

## 2. Step 0 — pin the referent

Before mapping anything, name the exact implementation, repo and commit the
claim is about, and why that one. The War Machine sibling nearly mapped the
futon3c pilot when the paper describes futon2 ("two-machines specimen"). A
port project has at least two referents by definition: pin both.

## 3. Step 1 — choose how the facts are sourced

| Mode | When | Example | Weakness to name |
|---|---|---|---|
| **Extract** | the host can report its own structure (reflection, compiler APIs, registries, a manifest it actually runs) | Lean: read `builtinPassManager` as a Lean value | over-approximation (reachability ≠ execution); opaque constants |
| **Declare + conform** | no reliable reflection; structure lives in conventions | futon3c peripherals: hand-authored EDN map, checked against source text by `wiring/conformance` | the map can be wrong; conformance is textual and Clojure-specific |

Prefer **extract** whenever the host offers it: it removes both the
hand-maintained inventory and a second parser. Use **declare + conform** only
where nothing better exists, and treat a conformance finding as possibly the
map author's error (it was, on the first live WS-E run).

## 4. Step 2 — emit facts in the shared schema

The interchange is the EDN that `futon3c.diagramprover.wiring/ingest`
already reads. Minimum:

```clojure
{:spec/id   :my-pipeline
 :referent  {:repo "…" :commit "…" :entry "…"}      ; step 0, required
 :source    :extracted                               ; or :declared
 :boxes     [{:box/id :pass/simp
              :reads  [:decl/body [:borrow {:record :param}]]
              :writes [:decl/body]
              :site   {:file "…" :var "…"}           ; declared mode only
              :order  12}]                           ; optional, see §5
 :phases    {:order [...] :tools {...}}              ; optional cycle chain
 :boundaries [{:kind :opaque :at :Lean.Compiler.foo
               :consequence "edges through it are absent, not disproved"}]}
```

Rules:

- A wire is a keyword, or `[field {:record r}]` when the same field name on
  two records is two different wires (`wiring/vertex-key`).
- `:boundaries` is mandatory when anything was not analysed. An absent edge
  is never evidence of absence unless the boundary list says the region was
  covered.
- An adapter in any language (Lean, Python, Clojure) emits this file. The
  graph checks in §5 then run unchanged; only conformance is per-language.

`:referent`, `:source`, `:order` and `:boundaries` are **not yet read** by
`wiring/ingest`; they are a proposed schema extension, not yet built.

## 5. Step 3 — run the check catalogue

| Check | Needs | Implemented in | Finding means |
|---|---|---|---|
| written-never-read | boxes | `wiring/written-never-read` | dead output, or a reader the map missed |
| read-never-written | boxes | `wiring/read-never-written` | missing producer, or exogenous input to declare |
| multiply-written | boxes | `wiring/multiply-written` | ownership conflict, or a scoping mistake |
| conformance | boxes + sites | `wiring/conformance` (Clojure source only) | map and code disagree |
| phase-chain | `:phases` | `wiring/phase-chain-findings` | ghost or unreachable phase |
| load-closure | sites + test-registry closure | `wiring/load-closure-findings` | site never loaded by the tests that claim it |
| phase discontinuity, duplicate occurrence | order | lean-wiring only | hand-off breaks, pass runs twice |
| reader-before-writer, overwrite-between | order | lean-wiring only | ordering defect invisible to the unordered checks |
| reachability (which boxes reach a declaration) | call graph | lean-wiring only | unreached code; or over-approximated reach |
| field matrix (box × field, read/write) | boxes | lean-wiring only | the readable summary for humans and diffs |

The bottom four rows are the Lean application's contribution. Porting them
into `wiring.clj` as order-aware checks over `:order` is a follow-up.

## 6. Step 4 — earn trust in a clean result

A `[]` result means only that the encoded properties hold. Before reporting
one:

1. **Sensitivity.** For each check you rely on, plant one defect it must
   catch: a fixture, or a replay of a pre-fix commit with a known defect.
2. **Specificity.** The post-fix or known-good state comes back clean.
3. **Mutate the checker, not only the input.** Change a classifier and see
   a test fail. This is what found the two misattributions in lean-wiring
   `d827e8c`; happy-path runs had not.
4. **Name the boundaries** in the output (§4), not only in a README.
5. **Author ≠ reviewer**, and the reviewer re-runs. Record who dispatched
   the reviewer: a reviewer briefed by the author is independent in model,
   not in framing.

## 7. Step 5 — compare two implementations (ports, migrations, rewrites)

When there are two referents (Lean and its Python port; futon and an mfuton
successor; old and new peripheral), emit facts from both into the schema,
align the box and wire names with an explicit **correspondence table**
(itself an artifact under review), and diff. Every difference must end in
exactly one of:

- **port defect**: the new implementation is wrong; fix the port;
- **checker defect**: an adapter or classifier is wrong; fix it and add the
  planted case that would have caught it;
- **representation difference**: both are right and differ by design;
  record it in the correspondence table with a reason.

Unclassified differences block a "matches" claim. This turns "the compilers
work differently" into a finite list of pass, wire and field edges.

## 8. Optional tiers, once the flow claim exists

- **Causal lift.** Read the flow map as a causal DAG and ask interventional
  questions with identification checked first (WS-B/WS-C: `causal/receipts`,
  `causal/identify`, the oracle pass under `oracle-pass/`). Use when the
  project needs "what would change if we did X", not only "is it wired".
- **Claim skeleton.** State the project's top claim as a small DAG of typed
  holes with contracts and graded warrants (§Application to
  theorem-proving capability construction). Use when the deliverable is an
  argument that the whole thing works.

## 9. Step 6 — record the application

Append a dated sibling entry to M-diagramprover with: referent(s), sourcing
mode, **reuse level** (method only / schema / checks / engine code),
findings, planted cases, boundaries, reviewer. The reuse level keeps the
mission honest about what transferred, which is the first thing an
independent reviewer will ask.
