# Mission: M-futon-seams

**Status:** HEAD complete; IDENTIFY complete; MAP/DERIVE/ARGUE/VERIFY in progress on **instance 4 only**; INSTANTIATE-1 taken out of order (2026-09-24). Phases 2–7 below were written *after* the work they describe, which is the mission's own most useful finding — see §Working out of order. Recorded from a Matrix conversation between Rob (`@facadebootstrap:into-the-matrix.my-familiar.com`) and claude-1, at Rob's request. Rob is having one of his evolvers record the same conversation on the mfuton side and intends to abstract the mfuton memory MCP's database calls.
**Owner:** **claude-1** (Joe, 2026-09-24: "claude-1 owns it, we're collaborating on the AIF + PROOF-2 interpretation"). claude-10 holds the PROOF-2a reading, the checker and the enactment record.
**Repo:** futon3c (the mission lives here; the instances span futon3c, futon6, mfuton).

## HEAD (Rob, 2026-09-24, verbatim sense)

When agents design and build a component in the first place, the *outer* layer should be created off the bat as a provider-agnostic interface. This is classical architecture: a US appliance is built to 120 V, an interface the electrical companies agreed so that no appliance and no generator needs to know about the other. Without it you get impedance mismatch, and the second implementation has to impersonate the first.

Rob's terminology, which this mission keeps:

- **interface** — the boundary declared *before* there is an implementation to salvage. Free.
- **seam** — the same boundary inserted *retroactively*, after one implementation has already become the de facto contract. Expensive, and Rob reserves the word for that case. (Feathers' original sense is also retroactive — "a place where you can alter behaviour without editing in that place" — which is why the word carries a salvage flavour.)

Rob's closing ask for the future: a seam in the Emacs layer, so a VS Code / TypeScript implementation could reuse the core functionality rather than copy-pasting interactions into webhooks — which is what he did, and which is "precisely what one does not want, because now the systems evolve separately, making their unification to a common interface even harder later on."

## The tension

- *Pull:* the first implementation is always concrete, and concreteness is how anything gets finished. Declaring an interface before there is a second implementation looks like speculative generality.
- *Cost of not:* every later implementation must impersonate the first. `matrix-ircd` exists because futon3c can only talk IRC. Rob ran futon3c on Windows by transcribing bash. Rob could not use futon3c's agent roles because the code reads the provider out of the agent id. Joe could not use Rob's memory MCP because it is neo4j-specific. In each case the work was done — just on the wrong side of the boundary.
- *Compounding:* copy-paste divergence is worse than either. Two implementations that have already drifted cost more to unify than one implementation plus a stub.

## The principle that came out of it

**A seam gets built where a boundary is already visible, and gets missed where the boundary is only conceptual.**

Rob observed this about his own work: he abstracted the publishing of data into the graph database with a proper adapter, "obviously a seam between python and a database so it was natural" — two languages, two processes, a driver in between. The boundary announced itself. But *neo4j versus any graph store* announces nothing: one library, one set of calls, inside one process. So that one was missed.

The same test explains the other instances. `/home/joe` is just a string. `claude-1` is just an identifier. IRC already sits in a module called `transport`, so it *looks* handled. A prompt is natural language, which no compiler inspects.

Sharpest case: **MCP presents as a seam** — it is literally a protocol — but it is the seam between the model and the tool, not between the tool and its store. A visible boundary at one level can hide a missing one directly behind it.

## Cost ordering (use this to prioritise)

1. **Declare the interface first** — free.
2. **Retrofit a seam in code** — expensive but mechanical; grep finds the literals.
3. **Retrofit a seam in prompt text** — worst. The coupling is invisible to every tool we have, so it must be pattern-matched in natural language at runtime.

## The eight instances

Each was checked against the working tree on 2026-09-24; counts and paths are as measured, not as remembered.

### 1. Filesystem root — done properly, then regressed

mfuton uses one global `MFUTON_HOME` (the root of the git tree) referenced by everything needing an absolute path: `I:\gh\mfuton` on one machine, `/home/rob/gh/mfuton` on another. Rob does *not* use `MFUTON_HOME` in futon code — it is an mfuton concept — but cites the futon6 superpod work as the equivalent.

- `futon6 6007937` "Resolve every hardcoded host path through the shared configuration" — **228 occurrences across 130 tracked scripts**. Python defaults from `futon6_config` (`config.ROOT`, `code_root`/`sibling`/`storage`); shell and Babashka derive roots from the script's own location with documented env overrides; docstring examples use placeholders.
- `futon6 8aea97c` "Separate the run contract from machine configuration; refuse, never fall back" — the data root stays separately configurable and **refuses** when unset. A silent fallback to someone's home directory is how the literals got in.
- `futon6 8e3e2d7` "Take the GPU count from the host, not from a literal 4" — same principle off the filesystem.
- `futon6 115554f` "Remove the two /home/joe paths that came in with the FM-001 merge" — the regression. Paths return through merges after a sweep.

**Acceptance test, from `6007937`'s own message:** redirect the root and confirm resolution follows — *all 91 distinct literals resolve to exactly the same location as before, and all 51 sibling/storage paths follow a redirected `FUTON_CODE_ROOT`/`FUTON6_STORAGE_ROOT`*. That checks a path interface without a second machine.

**Trap to carry forward:** inside a quoted heredoc there is neither shell expansion nor `__file__`, so "derive the root from the script's location" fails silently exactly where it looks safest. `6007937` fixed it by exporting the root beforehand; the defect had shipped in PR #51.

**Failure mode to avoid:** `MFUTON_HOME` defaulting to a literal like `/home/rob/gh/mfuton` reproduces the whole problem while looking solved. The default must be *computed* (e.g. `git rev-parse --show-toplevel`) or *absent*, never a path belonging to whoever wrote it.

**Current state:** futon6 has **30 files still containing `/home/joe`** (not yet triaged into code vs notes). claude-1 wrote 10 new occurrences today across four futon3c files — two of them *inside prompt strings* (see instance 6).

### 2. OS invocation — the model to copy

mfuton policy: put functionality in Python so it is system-invariant; use as thin as possible `.bat`/`.sh` files as the interface to Windows/Unix. futon3c's `scripts/windows/` is the worked example — **22 files**, wrappers of 4–35 lines (`repl-windows.bat` is 4, `codex-picker-windows.bat` 11).

Two things its README does right and should be imitated:
- **Names an authority:** "`Makefile.windows` is the canonical target list." A mapping table with a named authority is a different object from a second source of truth.
- **Documents its own leak:** selected targets are dispatched directly "to avoid make/shell interop drift on Windows" — written down where someone will find it rather than discovered later.

History: `985d06d7` (moved Windows files into a subdirectory), `c30f6219`, `892ac1a9`, `f2b6cae5`, `d2f44a3a`.

**Test:** if a `.bat` and its `.sh` twin ever diverge in *behaviour* rather than in *syntax*, logic has leaked into the wrapper. Line count is the cheap proxy — a 4-line wrapper cannot hold a policy.

### 3. Agent dispatch — an interface that worked

The turn-interpretation loop (`futon3c/emacs/session-turn-analysis.el`, 2026-09-24) dispatches to a delegate named by one variable, `session-mode-analysis-agent`, sending a brief that is entirely self-contained — record path, tools, rules — so the delegate needs no standing knowledge of the task. The seat was swapped four times in one evening (kimi-2 → kimi-1 → kimi-2 → kimi-1) with no code change, and when a seat left the registry mid-run the failure was "that address is gone", not "the other side broke". The 120 V property: the appliance does not know which generator.

### 4. Provider vs role — an identifier carrying a decision

Rob started with only Codex available and futon3c hardcoded to talk to Claude; configs and code had to be changed so the role Claude was playing could be served by Codex.

It is worse than naming — the provider is **parsed out of the id to make routing decisions**:

```
src/futon3c/transport/http.clj:4752   (str/starts-with? aid "codex")
src/futon3c/transport/http.clj:4795   (str/starts-with? (str/lower-case agent-id) "claude")
src/futon3c/apm/library_loop_adapter.clj:316   (= "codex" (.getName (io/file (first argv))))
```

**51 provider-literal agent ids** across `src/`.

The convention documents do it too: futon3c's `CLAUDE.md` states the coding-handoff protocol as *"substantial coding is belled to a Codex agent"* and says to find *"an idle Codex agent"*. The role is **implementer** and the constraint being enforced is **author ≠ reviewer**; neither needs a provider name, and as written the rule cannot be satisfied if no Codex seat exists though its actual requirement could be.

**What the interface would be:** a role registry. Roles resolve to seats; seats declare provider and availability; code asks for a role instead of pattern-matching an id. Evidence it is needed: the self-dispatch guard in `session-turn-analysis.el` compares the delegate id against the receiving agent's id to stop a seat interpreting turns addressed to itself. With roles that is a property of the binding, not a string comparison in the dispatcher.

### 5. Transport and Room — the seam that was never declared

Rob had to add configurable room and agent names, then an option *not to start an IRC server* (futon3c presupposed the user had no chat program), and finally ran a `matrix-ircd` adapter to mimic IRC — "not the proper many-to-many setup."

Measured:
- `grep defprotocol src/futon3c/transport/` — **nothing**. There is no interface for an adapter to satisfy.
- `transport/protocol.clj` calls itself "the boundary layer" but translates **HTTP/WebSocket ↔ pipeline** only; IRC is out of its scope.
- `transport/irc.clj` sits as a *peer* of `http.clj` and `ws.clj`, not as an implementation of anything.
- **16 hardcoded `"#futon"`-style channel literals** across `src/` and `scripts/`.

So the Matrix adapter got written — on the wrong side of the boundary, as a service impersonating the transport the code insists on.

**The library has an opinion, and it is about the wrong thing.** `futon3/library/realtime/transport-pivot.flexiarg`: *"Treat transport changes as controlled migrations, not ad-hoc chat suggestions"*, context "sessions begin on IRC, then someone proposes switching to WebSocket while work is underway." That is process discipline for *changing* transports, not an interface making them *interchangeable*. The difficulty was recognised and answered with a procedure instead of a seam.

**What the interface would be, none of it provider-named:** a **Room** (address + membership), a **Message** (author, body, time, thread ref), a **Command** (verb + args + reply channel). IRC, Matrix and Slack become adapters; `#futon` becomes configuration; the "don't start an IRC server" flag disappears because starting one is an adapter's business.

*This conversation is itself the evidence:* it arrived over Matrix through Rob's adapter, and the surface header names a Matrix room id — so the room concept exists at the edge of the system, as a string, and dies before reaching any code that could dispatch on it.

### 6. Prompts as an interface — the most painful retrofit

In the frontier-math orchestration, the couplings above were hardcoded not only in "classical code" (Rob's term, distinguishing it from steps handled by an agent) but **in the prompt text given to agents** — "tell claude-5 to do blah and store it at /home/joe". Rob deliberately left the original non-abstracted code in place and added switch code:

- `src/futon3c/mfuton_mode.clj` — one env var `FUTON3C_MFUTON_MODE`, default `"futon"`.
- `src/futon3c/agents/mfuton_prompt_override.clj` — docstring states the discipline: *"keep original prompt text in the source call sites and apply bounded rewrites here when mfuton mode is `mfuton`"*.
- `src/futon3c/agents/mfuton_invoke_override.clj`; called from `agents/tickle_orchestrate.clj`.
- Interception is by regex over natural language, e.g. `frontiermath-local-artifact-ref-re` matching `mfuton/data/frontiermath-local/FM-\d{3}/…`.

The retrofit needed: a mode flag the whole system consults; a shadow module mirroring the call sites; both versions live at once; and pattern-matching of prompt text at runtime.

**Why this is the worst case:** strings sent to agents are an interface and were never treated as one. A prompt carries a role binding, a filesystem root and a provider choice in a medium no compiler or linter inspects. With hardcoded code you can grep for the literal; with a hardcoded prompt you must match natural language at runtime.

**Not historical.** claude-1 embedded `python3 /home/joe/code/futon3c/scripts/xlate.py` inside instruction text sent to an interpreting agent on 2026-09-24 — after the futon6 sweep that removed 228 of these.

**Rob's closing note, which belongs in the record:** the switch is a transitional state, not the destination. `mfuton_prompt_override.clj` is currently a second source of truth that must be kept in sync with a dependency map by hand (its docstring says so). Left permanently, that is two systems drifting. The intended end state is the abstract path absorbing the hardcoded one and the flag disappearing.

### 7. Editor coupling — the one still compounding

Rob has onboarded most futon services but never the Emacs interactions; he uses VS Code plugins and the desktop applications. He is **not** asking for those adapters to be built now, especially since the Emacs code uses very specific Emacs concepts — but wants a seam inserted so later implementations are less painful. Without one he "lightly copy-pasted some of the interactions into webhooks and other techniques", so the systems now evolve separately.

Measured: **30 `.el` files, 23,325 lines**; `codex-repl.el` 4,733 and `agent-chat.el` 3,963 — a chat client, a session model and a dispatch layer, not an editor integration.

A fresh, small case with a clean ratio: `emacs/session-turn-analysis.el` is **497 lines, of which 48 touch buffers, overlays, points, markers, faces or windows**. Roughly a tenth is genuinely Emacs. The rest is a block-quote parser (`>>>` fencing), a record writer (sentence offsets, metadata, JSON), a delegate dispatcher, and vocabulary learning — none of which needs an editor.

**Divergence has already started here.** The turn record is written by Emacs, displayed by a ClojureScript app reading JSON, and written again by a Python batch script (`scripts/turn_batch.py`) from a different code path — three writers of one format agreeing by convention rather than construction. The Python one already had to reimplement sentence splitting to match the elisp.

**Where the seam goes:** `turn → record` (parse, split, offsets, quotes) and `record → dispatch` (choose delegate, build brief, deliver, report). Both are pure functions over text plus one I/O call. Overlays, cue painting and keybindings stay in Emacs.

**Caution:** extracting now means three existing callers move at once, or a fourth implementation appears. The cheap moment has passed; the cheapest *remaining* moment is before the VS Code version exists.

### 8. Store kind — both sides guilty

Rob uses XTDB for all of futon and neo4j/pgvector in mfuton, which is fine in itself. The problem is that mfuton's memory MCP is **neo4j-specific** — there is no interface to a generic graph/semantic database — so Joe could not use it with XTDB out of the box.

Same defect here, mirrored: `FUTON_SUBSTRATE_URL` abstracts **where the store is**, not **what it is**. `xtdb` appears **40 times across `src/`**, including in `social/shapes.clj` — store-kind assumptions have reached the type definitions. Swapping XTDB for neo4j here would be Rob's problem in reverse.

Rob's counter-example in the same system: he *did* properly abstract publishing data into the graph database with an adapter — because that boundary was visible.

## Relations

- `futon3/library/realtime/transport-pivot` — names the *procedure* for changing transports; this mission is about the missing *interface*. Worth reading as an instance of answering a seam problem with process discipline.
- `M-futon-seams` instances 4 and 7 are the same defect at different scales: an identifier or a module carrying information that should have been a declared property.
- futon6's "no host paths" discipline and its run-contract-vs-machine-config split (`8aea97c`) are instance 1's already-working half.
- The operator-turn pattern work (`scripts/session_turn_analysis.py`, `scripts/xlate.py`) is where architecture flexiargs derived from this conversation would land: Rob intends to add them on the mfuton side, and the candidate/parent mechanism here can hold their futon counterparts.

## IDENTIFY exit (when picked up)

Pick one instance and declare its interface — not all eight. The candidates in cost order:

1. **Role registry** (instance 4) — smallest surface, 51 literals plus three sites that route on the provider (two `starts-with?` and one `=` on a file name), and it unblocks provider substitution for Rob immediately.
2. **Room/Message/Command** (instance 5) — larger, but it retires `matrix-ircd` and the "don't start an IRC server" flag together.
3. **Turn→record / record→dispatch** (instance 7) — do before a VS Code implementation exists, not after.

For whichever is chosen, the exit is: the interface is declared, **at least one existing caller is converted to it**, and there is a test of the form "redirect the binding and confirm behaviour follows" (instance 1's method). An interface with no second implementation and no redirect test is a guess.

**Do not** mint the abstraction and leave the hardcoded path alive indefinitely. Instance 6 is the warning: a switch is a transitional state, and two sources of truth kept in sync by hand is the failure it was meant to prevent.


## Working out of order

This section exists because the mission cannot honestly present phases 2–7 as
though they were worked in sequence. They were not. On 2026-09-24 the mission
was recorded at 16:0x, and within the next two hours it acquired an inventory
(MAP), five cascades with receipts (DERIVE), argued pattern fits and kernel
runs (ARGUE), four static checkers (VERIFY) and live code in the serving JVM
(INSTANTIATE) — while its Status line still said IDENTIFY and it had no
section for any of them.

The cost is recorded rather than inferred. `exemplar/click-001-outcome.edn`
shows the enactment at futon3c `8e5c431e` built the **provider** grain where
the chosen cascade had specified the **role** grain, because the implementer
worked from this mission's defect description rather than from a DERIVE that
had met its exit. All three of instance 4's wants came back `:partial`. A
phase order is not ceremony: skipping DERIVE's exit is what let a reasonable
agent build a reasonable thing that was not the thing chosen.

The phases below are written now, after the fact, and say which of their exit
criteria are met on the evidence that exists. Where a criterion is not met
they say so. `holes/labs/M-futon-seams/lifecycle.edn` carries the same
statuses as data, and the working page renders them as a table.

## MAP — what exists, what is missing

**Exit criterion:** every MAP question has a concrete answer; the ready-vs-missing
table is complete. **Not met** — the answers exist, the table does not.

The inventory is §The eight instances above: every count in it was measured
against the working tree on 2026-09-24, not remembered. Ready versus missing,
for the purpose the mission actually has (which instances can be worked):

| instance | cascade | wiring | enacted | ready? |
|---|---|---|---|---|
| 1 filesystem root | no | no | already done (futon6 `6007937`) | retrospective only |
| 2 OS invocation | no | no | already done (`scripts/windows/`) | retrospective only |
| 3 agent dispatch | no | no | worked | retrospective only |
| 4 provider vs role | yes, two candidates | yes | `8e5c431e`, partial | **worked** |
| 5 transport and Room | yes | yes | no | ready |
| 6 prompts as an interface | yes | yes | no | ready |
| 7 editor coupling | yes | yes | no | ready |
| 8 store kind | no | no | no | **missing a cascade** |

What is missing to close this phase: a cascade for instance 8, which is the
only live unresolved instance without one; and this table moved from a click's
`:exclusions` into the mission, which is what has now been done.

## DERIVE — the design

**Exit criterion:** someone could implement the mission from the DERIVE section
alone. **Not met for the mission; met for instance 4.**

The design is not prose here; it is five cascades in
`holes/labs/M-futon-seams/proto/`, each pattern carrying a guard over work-state
tokens, the tokens it produces, and a receipt naming the library file, the
sha256 of the bytes read, the reading, and a scope limit saying what of the
pattern does *not* transfer. Instance 4 has two candidate routes —
registry-first (`instance-4.edn`) and observe-first (`instance-4b.edn`) — which
differ in four of seven patterns rather than in ordering.

`wiring/instance-N-wiring.edn` gives each cascade's construction as token flow:
a box's input ports are the tokens it needs, its outputs the tokens it
produces, an edge carries one named token, and a token a box forbids is an
inhibitor port. Dangling outputs and unfed wants are recorded, not hidden.

Why the exit is not met for the mission: only instance 4 has been worked to a
decision, and the cascades live in `holes/labs` rather than here. An
implementer would have to be told where to look.

## ARGUE — why this design

**Exit criterion:** the design feels inevitable given the constraints, and an
outsider can understand it from the plain-language argument alone. **Not met.**

What is argued: every pattern's receipt argues its own fit and states its scope
limit, and every containment edge says why it is there. Three candidate
transition kernels were run over every linear extension of each cascade, which
argues about the *carrier* — and found that co-application differs from any
flattening only when the click budget is shorter than the cascade is deep, and
that on instance 6 no flattening reaches the wants at any horizon because the
two conflicting patterns disable each other.

What is not argued: why instance 4's registry-first route should be preferred
to observe-first. The only comparison is p(all wants) 0.26 against 0.21 at
θ 0.8, horizon 6 — a number about how readily each cascade reaches its own
wants, which is not an argument about which is the better design. And there is
no plain-language account for a reader outside the project.

## VERIFY — checked against structural constraints

**Exit criterion:** the design has been checked against available structural
constraints; unverifiable risks spiked; DERIVE revisions recorded. **Met in
substance.**

Four checks run and have each caught real defects:

- `scripts/cascade_check.py` — every pattern id resolves, its declaration
  matches its path, every receipt's sha256 still matches the file's bytes,
  every token used is declared. 5/5 pass.
- `scripts/wiring_check.py` — an edge carries a token its source produces and
  its target needs; `:licensed-by` resolves to a pattern of the named cascade;
  recorded dangling outputs equal what the ports imply. 5/5 pass.
- `exemplar/proof2a_check.clj` (claude-10) — PROOF-2a's W_t and W_0 with nine
  falsifiers. W_t passes; W_0 fails on the construction condition, by
  construction, because the candidates are hand-built.
- `scripts/check_seams_layout.js` — the rendering, measured rather than
  eyeballed. 40 measurements, 0 failing.

DERIVE revisions recorded: the reflexive-descendants correction, which
reduced five missing meets to one (`4d748660`); `software-design/adapter-pattern`
becoming citable and closing instance 5's unproduced want; and
`gauntlet/placenta-transfer` replacing a stretched reading in instance 4.

## INSTANTIATE — demonstrations

**Exit criterion:** every completion criterion has a concrete demonstration, and
a new person could reproduce it from the mission doc. **Not met.**

One instance has been enacted. `futon3c.agency.roles` (futon3c `8e5c431e`,
claude-10) gives one function answering which provider an agent is; both
`str/starts-with?` provider branches in `transport/http.clj` now call it; a
redirect test passes in `roles_test` and 8 of its 14 assertions fail against
the old definitions, so it is a real falsifier.

Against instance 4's three completion criteria, all three are `:partial`:

- `caller-converted` — two call sites converted, but they ask for a *provider*,
  not a role.
- `redirect-test` — re-declaring a provider for a fixed id changes both routing
  decisions; rebinding a *role* cannot be tested, because roles do not exist.
- `prefix-routing-retired` — no `str/starts-with?` provider branch remains, but
  the library-loop adapter still requires the literal codex binary.

A new person could not reproduce this from the mission doc, which until now did
not mention it.

## DOCUMENT

**Exit criterion:** someone browsing the docbook can discover what this mission
built without knowing it exists. **Not started.**

The only rendered account is the working page at
`zone.hyperreal.enterprises/wip/seams.html`, reachable by URL alone. Nothing
navigates to it and there is no docbook entry.
