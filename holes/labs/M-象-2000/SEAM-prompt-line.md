# SEAM: the prompt line — a line other subsystems write into

claude-17, 2026-09-27, at Joe's request ("can we expose that as a seam that other agents
can build on?"). Extends decision P7a (the top pattern shown like a shell prompt). Joe
relayed codex-5's proposal for inbox-zero markers; it is adopted below.

## What the seam is

A small registry of **segments**. A subsystem contributes a segment; the REPL and the
per-turn header render whatever segments are present. No subsystem edits the renderer.

    {:segment/id     :pattern                 ; or :inbox-zero, :surface, …
     :segment/value  "象/诺必践"
     :segment/marker "*"                      ; optional one-character suffix
     :segment/provider "futon3c.agency.pattern-card"
     :segment/as-of  "2026-09-27T19:40:00Z"
     :segment/basis  {:evidence/id "…"}}      ; what the claim rests on, auditable

Rules:
1. **Absence is no claim.** A missing segment or marker never means "fine" or "clean".
2. **Providers are reads, never gates.** A provider that errs or exceeds its time budget is
   omitted from that render (rule 1 applies); the prompt is never delayed or blocked.
3. **Every segment carries provenance** (`provider`, `as-of`, `basis`), so the line
   itself can be audited later (P6o and P21 spirit: who says so, on what).
4. **One writer per segment id.** Two providers for one id is a registration error.

## Two renderings

- **REPL prompt** (for Joe): compact, in front of the prompt, e.g. `$象/诺必践*> `.
- **Per-turn header** (for the agent), spelled out beside the surface facts:
  `Prompt: pattern 象/诺必践; inbox-zero: own changes outstanding (verified)`.

## First two segments

**`:pattern` (P7a).** The pattern in force for this agent's turn: the active pattern card
if one is set (P10: an agent may swap its card at once), else the top retrieved pattern
for its last turn. Provider reads the existing retrieval (session-mode sigil /
context-retrieval evidence) and backpack/PSR facilities; it does not re-run retrieval.

**`:inbox-zero` (codex-5's markers, adopted).**

| prompt | meaning |
|---|---|
| `$象/诺必践>` | agent and current pattern; **no claim** about cleanliness |
| `$象/诺必践*>` | outstanding changes **verified as belonging to this agent's current work** |
| `$象/诺必践?>` | dirty files exist in the shared checkout; **attribution unresolved** |

A verified-clean state is deliberately not shown: by rule 1, the absence of a marker is
not a cleanliness claim. If a clean claim is ever wanted it needs its own marker and a
basis, not the absence of `*` and `?`.

## Constraints for the implementation packet

- The REPL prompt is found by the literal `"> "` (`emacs/agent-chat.el:847-862`,
  `agent-chat--ensure-prompt-markers!`) and is read-only for a recorded reason
  (`agent-chat--insert-prompt`, 2026-09-24 incident). A prefix must keep the literal
  `"> "` suffix, keep the whole prompt read-only, and update prompt detection and repair
  to allow a prefix. Test: the 2026-09-24 kill-backward case still cannot delete it.
- `"^> "` elsewhere is a markdown blockquote; the prefix must not make those ambiguous.
- The header line is added where the surface contract is built
  (`src/futon3c/transport/http.clj` ~4535-4549), as facts, not instructions.
- Seats without a provider render exactly as today.

## Who builds what

The renderer and the `:pattern` segment are M-象-2000's (P7a). The `:inbox-zero` segment's
provider belongs to codex-5's inbox-zero claim-lifecycle work, written against this seam.

## API decisions (2026-09-27, claude-17 with codex-5)

codex-5 (M-inbox-zero-claim-lifecycle) raised the questions; the renderer owner settles
them here. Nothing below is built yet; the registry lands as packet **P7a-1**.

1. **One registry, in the futon3c JVM:** namespace `futon3c.agency.prompt-line`.
   `(register-provider! {:segment/id :inbox-zero :provider "<ns/fn name>"
   :fn f :budget-ms 100})`. Re-registering the same id with a different provider is
   refused (rule 4). The per-turn header is rendered in the JVM at invoke time; the
   Emacs REPL reads the same render via `GET /api/alpha/prompt-line?agent=&session=`
   when it inserts a prompt (at turn end), never per keystroke.
2. **Provider context** (a map, read-only): `:agent-id`, `:session-id` (exact),
   `:surface`, `:render-at` (instant set by the registry), `:budget-ms`, and
   `:worktree-roots`: the canonical roots the registry knows for that agent
   (from its registration/cwd); **absent when unknown**, and then a provider that needs
   them must return nil (omit). No provider infers ownership from root or mtime alone.
3. **Provider contract:** `(f ctx) → segment-map | nil`. nil = omit. The registry runs
   providers in parallel with a deadline (per-provider `:budget-ms`, default 100;
   whole render 250); a late, nil, invalid or throwing provider is **omitted** and the
   prompt renders without it.
4. **Provenance distinguishes observation from render:** the provider sets
   `:segment/observed-at` (when it looked) and `:segment/basis`
   `{:evidence-ref … :scope {…}}`; the registry adds `:segment/rendered-at`. A stale
   observation is the provider's to judge: it omits rather than asserting old state.
5. **Composition:** only `:pattern` supplies the prompt's *value*. Every other segment
   supplies at most one `:segment/marker` character and a `:segment/header` phrase. The
   prompt is `$` + pattern value + markers in registration order + `"> "`. No segments
   at all gives the plain `"> "`, unchanged for seats without providers.
6. **Weakest claim wins in the marker:** a provider with mixed evidence emits the
   weaker marker (for inbox-zero, `?` over `*`), and its `:segment/header` spells out
   both, e.g. "inbox-zero: 3 own (verified), 2 unattributed". A marker never
   summarises away the unresolved part.
7. **Omissions are inspectable, never blocking:** the registry keeps the last render per
   (agent, session): `{:rendered-at … :segments […] :omitted [{:segment/id … :reason
   :timeout|:error|:nil|:invalid}]}` at `GET /api/alpha/prompt-line/last?agent=&session=`.
8. **Test seam:** a pure `render` function over a context and a provider list, so
   providers are tested with fake contexts and the renderer with fake providers
   (`futon3c.agency.prompt-line-test`). codex-5's provider is written against it once
   P7a-1 lands; its `*` authority waits on codex-5's claim-lifecycle proof, as they
   stated, and until then it emits only `?` from a fresh observation.

## The `:pattern` provider's source, concretely (2026-09-27, Joe)

The per-turn embedding retrieval already exists: every turn writes a
`context-retrieval` evidence record (futon3a embeddings, top 3 with scores). Example,
claude-17 turn 76 (evidence e-76cf557a…): 1 `control/agential-pattern-hygiene` 0.4496,
2 `forward-model/the-forward-model-is-a-pattern-cascade` 0.4352, 3
`fulab/pattern-propose` 0.4298. The provider reads the latest such record for the exact
(agent, session) and takes rank 1. No new retrieval.

**Retrieved is not applied** (象/两种规格). The prompt shows which it is:
- `$~control/agential-pattern-hygiene> `: rank 1 of the last retrieval, **retrieved**
  only; the header gives the score and the other two ids;
- `$象/诺必践> `: an **active pattern card** the agent has set (P10: set or swapped at
  will), which makes the claim "I am working to this pattern";
- a card set by the agent always wins over retrieval.
By P21 neither is an attestation: attestation happens at use, and a card is a claim of
intended use that the work then bears out or not.

## Candidate P7a-2: the operator picks the pattern (Joe, 2026-09-27)

TAB at the start of an empty input line opens a short list of **nearby** patterns; the
one Joe picks overrides the retrieved rank 1 for that turn, shows in the prompt, and is
sent with the turn as an operator's stated pattern.

- **Nearby, computed classically:** the other two ids of the last retrieval, plus
  rank 1's neighbours in the mined graph (`why`, `how`, `co-cited` edges), about six in
  all, each with its title. No model call.
- **Precedence:** the operator's pick for the turn beats an agent's card, and an agent's
  card beats retrieval. The header says which: `operator-chosen`, `card`, `retrieved`.
- **Sent with the turn** as a structured field of the operator act (not prose), so the
  receiving agent sees it and it is recorded on the evidence.
- **Why it matters beyond the UI:** a pattern Joe chooses for his own turn is a human
  label stated at the moment of use. It is (a) a correct label for 小象's lexicon
  (E-classical-wastage-scanner: 大象 builds, 小象 learns), (b) a direct check on the
  retrieval ("the embedding said X, Joe chose Y"), and (c) under P21 a use-time
  attribution by an independent party, not by the proposer.
- **Constraint:** TAB keeps its current meaning everywhere except the start of an
  empty input line; check the REPL's existing TAB binding before taking it.
