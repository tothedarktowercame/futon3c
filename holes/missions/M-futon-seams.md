# Mission: M-futon-seams

**Status:** HEAD complete; IDENTIFY complete; **INSTANTIATE-1 complete for instance 4** — enacted as chosen, at the role grain, after a first attempt that built the wrong grain; MAP/DERIVE/ARGUE in progress; VERIFY met in substance (2026-09-24). Phases 2–7 below were written *after* the work they describe, which is the mission's own most useful finding — see §Working out of order. Recorded from a Matrix conversation between Rob (`@facadebootstrap:into-the-matrix.my-familiar.com`) and claude-1, at Rob's request. Rob is having one of his evolvers record the same conversation on the mfuton side and intends to abstract the mfuton memory MCP's database calls.
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
table is complete. **Met.** The survey questions below are this mission's Q1–Q5,
answered by counting rather than by estimate; every figure was measured on
2026-09-24 and each says how.

MAP is research, not design: what follows is what exists, not what should.

### Q1. What already exists that this mission will use?

The **pattern library** at `futon3/library`: **1,411 patterns in 119
families**, all of them citable — an id line (`@flexiarg`, `@arg` or
`@multiarg`) matching the file's path, checked for every file. Retrieval is
`scripts/xlate.py find`, BM25 over all of them plus proposals, ~0.3 s, with a
measured recall@5 of about 0.29.

The five cascades written so far draw **21 distinct patterns from 17
families** — `agency`, `apparatus`, `cascade-construction`, `contracts`,
`coordination`, `cycle-machine`, `futon-theory`, `gauntlet`, `iching`,
`memory`, `mmca`, `or3`, `peripherals`, `realtime`, `software-design`,
`translation`, `war-room`. That spread is a finding in itself: the moves this
mission needs are not concentrated in a "software architecture" family, and
half of them come from families written for other purposes.

**Tooling this mission built and now uses** (16 programs, 2,987 lines):
`cascade_check.py`, `wiring_check.py`, `wiring_from_cascade.py`,
`mission_c_check.py`, `reanchor.py`, `mission_anchors.py`, the three renderers
and the Playwright check; and under `holes/labs/M-futon-seams/`,
`proto/kernels.clj`, `proto/meets.clj`, `exemplar/proof2a_check.clj`,
`exemplar/construct_replay.clj`, `exemplar/clauses_1_6.clj`,
`exemplar/enumerate_sites.py`.

**From outside**: futon2's `interpretation-construction/construct` (replayed,
not reimplemented), futon4's `mission-lifecycle.md`, and PROOF-2a itself.

### Q2. What data does the mission already hold?

Eight files under `proto/`, five under `wiring/`, eleven under `exemplar/`,
eight under `item6/`, and **37 annotations** anchored to exact spans of this
file. One click, one enactment with 8 attempts, one superseded outcome, one
clause computation, one site enumeration, one mission-level C.

### Q3. Which instances are ready to work, and which are missing what?

| ready (no new artefact needed) | missing (the actual work) |
|---|---|
| **4** — two cascades, wiring, click, enacted, checked | — |
| **5** — cascade, wiring | no click; not enacted |
| **6** — cascade, wiring | no click; not enacted |
| **7** — cascade, wiring | no click; not enacted |
| **1, 2, 3** — done before this mission existed | no cascade, and none needed: retrospective only |
| — | **8** — live and unresolved, and the only such instance with **no cascade at all** |

### Q4. How much of the coupling this mission is about is left?

Measured, not estimated. Instance 4: `exemplar/sites.edn` reports **43 seat
literals across 12 files and one routing site**. Instance 5: **16 hardcoded
channel literals**, and `grep defprotocol src/futon3c/transport/` still returns
nothing. Instance 7: **30 `.el` files, 23,325 lines**, and the turn-record
format has **five** readers or writers. Instance 8: `xtdb` appears **40 times**
in `src/`, including in `social/shapes.clj`.

### Q5. What can the mission's own method not yet check?

One step of nine: **choosing the grain** (DERIVE step 3). Everything else has
either a tool or a check, and the table at the end of DERIVE says which.

### Surprises, recorded before DERIVE locked anything in

- **The library was already citable.** The mission spent effort on a
  distinction — `@arg` versus `@flexiarg` — that the loader never made. 28
  files declared `@arg` and a validator refused them; the validator was wrong,
  not the files. One want that had been recorded as unreachable became
  reachable.
- **Four of five "missing meets" were a definition artefact.** Strict
  descendants made every comparable pair look like it lacked a meet. Under the
  reflexive reading the library's own Lean module uses, one remains.
- **A wiring can be internally consistent and wrong.** The first wirings drew
  every cascade leaf to the want-port; the claim was false for three of
  instance 5's four leaves and passed inspection.
- **Instances 1, 2 and 3 are already done.** They are evidence, not work, and
  a cascade for them would be a retrospective reading. That was not obvious
  when the eight were first listed.
- **The mission undercounted itself.** It said "two `starts-with?` branches"
  where enumeration found a third routing site of a different shape.

## DERIVE — the method

**Exit criterion:** someone could implement the mission from the DERIVE section
alone, without needing to ask clarifying questions. **Met for the tooled steps;
not met for four steps that are done by hand**, two of which have no check at
all. Those are named below rather than glossed.

What this mission develops is a **capability**, not a list of instances: find
an undeclared seam, choose how to declare it, do it, and check what was done.
The procedure below is that capability written out, in the order it actually
ran on instance 4. Instance 4 is the worked illustration; the steps are the
subject.

A step that went wrong is kept as a step. Step 3 is where instance 4 lost a
day's work, and the shape of the loss is the reason step 8 has a check now.

### 1. Someone hits the coupling

**In:** a person who could not do something — Rob had only Codex and the code
was hardcoded to talk to Claude. **Out:** an instance section in this mission,
in the operator's own words. **Tool:** none. An interview, by hand.
**Check:** none needed at this step, but nothing in it may be asserted: every
claim it makes becomes step 2's input.

### 2. Measure it

**In:** the claim. **Out:** counts and paths in the instance section — for
instance 4, *51 provider-literal agent ids* and *three sites that route on the
provider*, each checked against the working tree.
**Tool:** `grep`, by hand at the time; now
`holes/labs/M-futon-seams/exemplar/enumerate_sites.py`, which emits
`exemplar/sites.edn`. **Check:** re-run the command and get the same number.
The count is a token later, so it has to be re-derivable, not remembered.

*This step catches an over-claim.* The mission first said "two `starts-with?`
branches"; the enumeration found a third routing site of a different shape, and
the mission was corrected rather than the count rounded.

### 3. Choose the grain — by hand, and now checked

**In:** the measured sites. **Out:** a `:grain {:keyed-by … :statement …}` on
the cascade's grain pattern. For instance 4: **`:role`**, not the seat and not
the provider. **Tool:** none — the judgement is a person's. **Check:**
`scripts/grain_check.py`.

This is the step instance 4 got wrong. The grain was chosen correctly in the
cascade — `cascade-construction/choose-the-grain-where-state-lives` answered
"at the role" — and then the first enactment read the *defect description* in
§4 instead, which describes providers being parsed out of ids, and built a
provider lookup. Nothing compared the two until all three wants came back
`:partial`.

**What made it checkable** was noticing that a resolver's grain is *what its
lookup is keyed by*, and that this is visible in its argument list:

```clojure
(defn seat-for [role])       ; keyed by :role      — the chosen grain
(defn provider [agent-id])   ; keyed by :agent-id  — the grain it was built at
```

So the check is three comparisons rather than one. The cascade declares its
grain; the enactment declares the grain it was built at, naming a resolver as
evidence; and that resolver must **exist, with the recorded argument list, in
the file at the recorded sha256**. The third is what stops the check being two
agents agreeing with each other — a declaration no code answers to is not
evidence.

All four cascades that choose a grain now declare it: instance 4 `:role`,
5 `:room`, 6 `:fragment`, 7 `:turn`.

*It catches the original error.* Run against `click-001-outcome.edn`, the
first attempt, it fails:

```
cascade grain role, enacted grain agent-id
FAIL GRAIN MISMATCH: the cascade chose role, the enactment is keyed by agent-id
```

and against a record naming a function that does not exist, or one whose
argument list has drifted, it fails on the evidence instead. That record is
kept unchanged for exactly this purpose.

### 4. Write the cascade

**In:** the instance section and the grain. **Out:**
`holes/labs/M-futon-seams/proto/instance-4.edn` — patterns with a guard over
work-state tokens, the tokens each produces, and a receipt naming the library
file, the sha256 of the bytes read, the reading, and a **scope limit** saying
what of the pattern does *not* transfer.
**Tool:** `scripts/xlate.py find "<the move>" --with-candidates` for retrieval;
the authoring is by hand. **Check:** `scripts/cascade_check.py` — every id
resolves, its declaration matches its path, every receipt sha still matches the
file's bytes, every token used is declared, wants with no producer are reported.

*The worked illustration, instance 4:* two candidate routes, not one.
`instance-4.edn` is registry-first (name the grain, make the binding a record,
give it one producer, transfer a caller, test, retire) and `instance-4b.edn` is
observe-first (a read-only role view, routing behind a switch, then the single
authority). They differ in four of seven patterns, which is what makes them two
candidates rather than two orderings.

### 5. Write the wiring

**In:** the cascade. **Out:** `holes/labs/M-futon-seams/wiring/instance-4-wiring.edn`
— the construction as token flow: each box's input ports are the tokens it
needs, its outputs the tokens it produces, and a token it forbids is an
inhibitor port. **Tool:** `scripts/wiring_from_cascade.py` derives it; an
enacted instance gets satiety filled in by hand from its outcome record.
**Check:** `scripts/wiring_check.py`, which derives the edges the *cascade*
implies and compares in both directions.

*This step catches a wiring that is internally consistent and wrong.* The first
version drew an edge from every cascade leaf to the want-port, captioned "what
it produces is wanted directly" — false for three of instance 5's four leaves.
The check exists because that version passed inspection.

### 6. Choose the target and record the click

**In:** the cascades that exist. **Out:**
`holes/labs/M-futon-seams/exemplar/click-001.edn` — every target considered,
partitioned into feasible and excluded with a typed reason and what would make
each feasible, the declared cost ordering and how it entered, the score inputs,
and the chosen target with its candidate field.
**Tool:** by hand. **Check:** `holes/labs/M-futon-seams/exemplar/proof2a_check.clj`
(clause T and clause 0, with nine falsifiers).

*What the check found, twice.* When this click was first recorded, clause 0
**failed**: both candidates were hand-built and no replay of a constructor
existed, and the click said so in its own `:construction-receipt`. Since
`exemplar/construct_replay.clj` (futon3c `a3f65fcc`) replays futon2's
`interpretation-construction/construct` on the recorded interpretations and
reproduces both candidates, clause 0 now **passes by replay** —
`proof2a_check.clj` passes W₀ and fails it on a replay that does not reproduce
the candidate. What remains hand-authored, and is recorded as such because W₀
permits it, is the **interpretations** and the containment edges. The
constructor reproduces a candidate from those; it does not produce them.

The verdict changed because the apparatus changed, not because the record did.
Both readings are kept in `click-001.edn` for that reason.

### 7. Predict

**In:** the chosen candidate, a per-pattern success rate θ, a horizon.
**Out:** `:prediction` inside the click — for instance 4, p(all wants) 0.26 at
θ 0.8, horizon 6, with the spread over every linear extension recorded.
**Tool:** `bb holes/labs/M-futon-seams/proto/kernels.clj θ horizon FILE`, and
`proto/meets.clj` for the semilattice condition.
**Check:** clause 4 later recomputes the prediction from the recorded inputs
and compares; on instance 4 it reproduced (0.260 against 0.26).

### 8. Enact — **by hand, and this is where conformance is decided**

**In:** the chosen candidate. **Out:** commits, plus
`holes/labs/M-futon-seams/exemplar/click-001-enactment.edn`: one row per
attempt with the pattern it enacted, and a `:conformance` block naming every
deviation. **Tool:** an agent, by hand. **Check:** each want token's own check
— for instance 4, two tests and a grep.

*Both runs are recorded.* The first (`exemplar/click-001-outcome.edn`,
`8e5c431e`) built the provider grain and returned all three wants `:partial`.
The second enacted the cascade as chosen and all three are met. The deviations
in the second are recorded too: `realtime/mode-gate` was done before its guard
held, and the rotation roster was left because it cycles seats rather than
looking a role up.

**What checks this step now, and what still does not.** Clause C
(`proof2a_check.clj`, W_c, since futon3c `401469fd`) checks the enactment
record against the chosen candidate: it passes on `click-001-enactment.edn`,
and fails on the first attempt alone, on attempts with their checks removed,
and on an untyped deviation. So "the change that was made is the chosen
candidate" is now a checkable claim, where at the time of the first enactment
it was not.

It remains a check on a **record**, made after the fact by whoever enacted the
change, rather than a guard during enactment. Nothing stops an agent building
something else; what exists is a check that notices afterwards, and it notices
only what the record says. A deviation left out of the record is invisible to
it.

### 9. Observe, and update the belief

**In:** the enactment's observations. **Out:**
`holes/labs/M-futon-seams/exemplar/click-001-clauses.edn`.
**Tool:** `bb holes/labs/M-futon-seams/exemplar/clauses_1_6.clj`.
**Check:** the clause statuses themselves, which say which inputs were
*declared* rather than measured.

### What is by hand, and what has no check

| step | by hand | has a check |
|---|---|---|
| 1 hit the coupling | yes | n/a |
| 2 measure | no (`enumerate_sites.py`) | yes, re-run it |
| 3 choose the grain | yes | yes, `grain_check.py` |
| 4 write the cascade | yes (retrieval tooled) | yes, `cascade_check.py` |
| 5 write the wiring | no (derived) | yes, `wiring_check.py` |
| 6 choose the target | yes | yes, `proof2a_check.clj` (W₀ by replay) |
| 7 predict | no (`kernels.clj`) | yes, clause 4 |
| **8 enact** | **yes** | wants checked; conformance checked **on the record** (clause C) |
| 9 observe | no (`clauses_1_6.clj`) | yes, clause statuses |

**Every step now has a check.** Four are still done by hand — hitting the
coupling, choosing the grain, writing the cascade, enacting — and that is not a
defect to be fixed: they are judgements, and the method's claim is that a
judgement must be *recorded in a form something else can disagree with*, not
that it be automated away.

Instance 4's failure took two steps with it, and both are closed. Step 8 by
clause C, which checks the enactment record against the chosen candidate; step
3 by `grain_check.py`, which compares the grain the cascade chose against the
grain the code is keyed by. Both are checks on records rather than guards
during the work, and both fail on the real first attempt rather than on a
constructed example.

## ARGUE — which patterns, and why those

**Exit criterion:** the design feels *inevitable* given the constraints, not
merely *possible*, and someone outside the project can understand what it does
and why from the plain-language argument alone. **Not met, and both halves now say
why rather than promising more work.** The inevitability half is answered
with a measurement and the answer is negative: the design is defensible, not
inevitable. The plain-language half is DOCUMENT's, and this phase closes when
DOCUMENT is written.

ARGUE for this mission is pattern selection: a cascade *is* a claim that these
patterns, in this containment order, are what the move requires. The material
for the argument already exists — every one of the **37 pattern entries** across
the five cascades carries three fields written when it was chosen:

- **`:forces`** — the general condition the pattern addresses, stated without
  reference to this mission.
- **`:reading`** — why it fits *here*, in this instance's terms.
- **`:scope`** — what of the pattern does **not** transfer. This is the field
  that makes the others answerable: `choose-the-grain-where-state-lives` is
  taken "verbatim on grain selection" and its "many-item invariants in adapters
  and gates" half is explicitly not used.

Every containment edge carries a **`:via`** saying why it is there — not that
one pattern follows another, but what distinction the first opens that the
second needs. "Choosing the role as the grain is what makes a binding record
about roles rather than about processes."

That is a strong argument for **fit**. It is not an argument for
**inevitability**, and the difference is measurable.

### Could the library have been searched into these patterns?

`proto/selection-margins.edn` (regenerate with `proto/selection_margins.py`)
runs `scripts/xlate.py find` twice for each of instance 4's seven slots: once
with the move stated in **the mission's** vocabulary, once in **the pattern's**
own. Rank is where the chosen pattern appears; *nil* means it did not appear at
all.

| slot | from the problem | from the name |
|---|---|---|
| `choose-the-grain-where-state-lives` | — | **1** |
| `coordination/assignment-binding` | **1** | 1 |
| `cycle-machine/single-producer` | — | 2 |
| `or3/count-every-card-back` | — | 1 |
| `gauntlet/placenta-transfer` | — | 3 |
| `translation/test-by-reproducing-behaviour` | **1** | 2 |
| `realtime/mode-gate` | — | 1 |

**Two of seven** are reachable from the move as the mission states it. **All
seven** are reachable once the query uses the pattern's own words. The
selection was made by someone who already knew the library, not by searching
it — and the mission should say so, because the capability it is trying to
develop cannot depend on that.

This is the same defect the mission is about, turned on its own tools. A
coupling in prose is invisible to a tool that greps for literals; a pattern
whose vocabulary differs from the problem's is invisible to retrieval that
matches words. `or3/count-every-card-back` is about enumerating before acting,
and nothing in "enumerate every site that hardcodes the thing" reaches it.

**The reconstruction's limit, stated:** the original selection recorded no
rejected candidates, so this cannot show how the patterns were actually found.
It shows what retrieval offers today for each phrasing. A cascade that recorded
its rejects at the time would not need reconstructing — that is a change to
DERIVE step 4, not a finding about these seven.

### Registry-first against observe-first

The click preferred `:cand/a-registry-first` on p(all wants) **0.26 against
0.21**, and recorded its own caveat: both candidates were authored by the same
agent from the same mission within an hour, so a field of two from one author
is not a field of two from two. That number measures how readily a cascade
reaches *its own* wants under a kernel. It does not compare designs.

`proto/candidate-comparison.edn` compares them on something a design argument
can use. They share four of seven patterns; the three that differ are each a
different **reading of the same requirement**:

| requirement | 4a registry-first | 4b observe-first |
|---|---|---|
| what the routing state belongs to | **names** the grain (`:role`), makes the binding a record | does not name a grain; builds a **read-only role view** first, so what a binding needs is learned from watching |
| one place decides routing | `cycle-machine/single-producer` — the general form | `agency/single-routing-authority` — the domain form, which names the failure modes |
| retire the old path | `realtime/mode-gate` — a gate closes | `iching/hexagram-49-ge` — 革, replacement with **legitimacy and timing** as conditions |

**The test that means something: what would each have done about the two
things that actually went wrong?**

*The grain error.* 4a named the grain and named it correctly, and nothing
compared the name to the code until the wants failed. But **because** it names
it, the error is checkable afterwards — `grain_check.py` exists only because
4a declares something to check, and it fails on the first attempt. 4b's first
move produces `:roles-observable`: a read-only view of *roles*, displayed,
before anything routes through it. That would likely have **shown** a provider
lookup as the wrong thing early — it would have displayed providers where
roles were expected. But 4b declares no grain at all, so `grain_check.py` has
nothing to compare and cannot check it. Its protection is that someone would
notice, which is not a check.

*The step-order deviation.* `realtime/mode-gate` was enacted in attempt 1,
before its guard `:redirect-test` held. A guard in a cascade is not enforced
during enactment; clause C catches it afterwards, on the record. 4b's
`hexagram-49-ge` makes timing a **condition of legitimacy** rather than a
guard on a step: a replacement done before its test has passed is not an early
replacement, it is not a replacement. The deviation that happened is precisely
what that reading forbids — and 4b's own receipt had already called 4a's
reading the weaker one, before either was enacted.

**The verdict, which is not a clean win.** 4a is better on **checkability**;
4b is better on **prevention**; and the number that chose between them
measures neither. So the honest answer to this phase's criterion is that the
design is **defensible, not inevitable** — one of two readings, each stronger
on a different axis, selected on a measurement of neither.

This comparison was available before the enactment. Both cascades and both
receipts existed. Nobody made it, because the click compared candidates on the
one number the tooling produced, and nothing asked what each candidate made
*checkable*.

**Proposed change to DERIVE step 6:** a click over two candidates should
record, per candidate, which of its judgements it makes checkable — the
declarations a later check can compare against code. 4a declares a grain and
can be grain-checked; 4b declares none and cannot. That is a property of a
candidate a selector can read, and it is not p(all wants).

### What is still not argued

**The plain-language account for an outsider.** It does not exist. This is the
second half of this phase's exit criterion, and it is the half DOCUMENT
produces — Rob's requested example is the vehicle. Worth naming as a fact
about the lifecycle rather than about this mission: ARGUE is specified to come
before DOCUMENT, and one clause of ARGUE's exit can only be satisfied by work
DOCUMENT does. This phase stays open until then.



### What the kernel runs argue, which is something else

Three candidate transition kernels over every linear extension of each cascade.
The finding is about the **carrier**, not this design: co-application differs
from any flattening only when the click budget is shorter than the cascade is
deep, and on instance 6 no flattening reaches the wants at any horizon, because
the two conflicting patterns disable each other. Worth keeping and not to be
mistaken for an argument that these patterns are the right ones.

## VERIFY — checked against structural constraints

**Exit criterion:** the design has been checked against available structural
constraints; unverifiable risks spiked; DERIVE revisions recorded. **Met.**
Three clauses, taken separately below, because the section previously said
"met in substance" without saying which clause the hedge was on. It was the
second.

### Checked against available structural constraints

Seven checks run, and each exists because it caught something:

- `scripts/cascade_check.py` — every pattern id resolves, its declaration
  matches its path, every receipt's sha256 still matches the file's bytes,
  every token used is declared. **5/5**.
- `scripts/wiring_check.py` — derives the edges the *cascade* implies and
  compares in both directions, so a wiring cannot be internally consistent and
  wrong. **5/5**.
- `scripts/grain_check.py` — the grain the cascade chose against the argument
  list of the enacted resolver, and that resolver must exist at the recorded
  sha. **Passes** on the second enactment, **fails** on the first.
- `exemplar/proof2a_check.clj` (claude-10) — W_t, W₀ and W_c with nine
  falsifiers. W_t and **W₀ both pass**, W₀ by constructor replay since
  `a3f65fcc`; W_c checks the enactment against the chosen candidate and fails
  on the first attempt alone, on attempts with their checks removed, and on an
  untyped deviation.
- `scripts/mission_c_check.py` — every cue-quote is the text at its cue, every
  quote occurs exactly once, every served-by want exists in its cascade.
- `scripts/check_seams_layout.js` — the rendering, measured rather than
  eyeballed. **40 measurements, 0 failing**.
- `holes/labs/M-futon-seams/proto/meets.clj` — the restricted semilattice
  condition, computed rather than assumed.
- `exemplar/check-ledger.edn` (claude-10) — every check run recorded with what
  was true, so the checks themselves can be scored. It found a grep regex that
  passed when the truth was false, which is the cell an error-rate table exists
  to expose.

**What scoring the checks found.** With independence established, the rates
stand: test fp .167 / fn .125; grep fp .300 / fn .167; validator fp .179 /
**fn .750**; layout `insufficient` at n=4, recorded as a typed absence rather
than a number. The validator figure is the best-powered of the four — 7 rows
carrying 18 runs — and it says a validator in this mission let through three
of four bad cases. A check that is trusted and wrong three times in four is
worse than no check, and this is the first measurement any of them has had.

### Risks that cannot be checked statically, and what was done about each

| risk | spiked? | how |
|---|---|---|
| Does a role indirection actually let a different seat serve? | **yes** | `roles_test`: rebind `:reviewer` claude-1 → oxf-codex-7, behaviour follows; 8 of its 14 assertions fail against the old definitions |
| Does the replay reproduce the candidates, or merely echo them? | **yes** | `proof2a_check.clj` fails W₀ on a replay that does not reproduce the candidate |
| Does a grain declaration correspond to real code? | **yes** | `grain_check.py` resolves the named resolver and compares its argument list; tested against a renamed function and a drifted arglist |
| Do the recorded predictions reproduce? | **yes** | clause 4 recomputes from the recorded inputs: 0.260 against the recorded 0.26 |
| **Is a coupling carried in prompt text really invisible to tooling?** | **yes** | `exemplar/spike-prompt-coupling.edn` — see below |
| Are the check error rates real? | **yes** | `exemplar/check-ledger.edn`: 22 rows, 33 runs, each row carrying `:truth` and a `:truth-source`. Clause 1 (A) reads `:measured-from-check-ledger` — per-kind rates from counts |
| Is each recorded truth independent of the check it judges? | **yes**, and judged from outside | futon2 `f758d702` / `fc90808d`, fixture `test/fixtures/check-ledger-classification/m-futon-seams-v1.edn`: every row classified by kimi-4 — 7 constructed-bad-case, 9 later-review, 6 independent-recomputation, **none self-truthed**. The estimator now refuses a row naming no independent act, and an unclassified ledger reads all-insufficient |

**The prompt-coupling spike.** Instance 6 *claims* a coupling in prompt text is
invisible to tooling. That is a claim, so it was tried: change
`upstream-strategy-doc-ref` in `mfuton_prompt_override.clj` to name a file that
does not exist, run the test namespace for that area, restore, run again.
**22 tests, 75 assertions, 1 failure — identical both ways.** The one failure
is pre-existing and unrelated (a channel assertion, `#math` against `#futon`),
recorded so it is not mistaken for the result. Nothing noticed.

Two things turned up that the mission had not claimed:

- **The coupling is not at risk of breaking; it is already broken.** Both
  upstream doc refs name files that do not exist in the working tree today.
  The spike did not need to introduce a fault — one was there, unreported.
- **The second copy is inside the file.** Each path is written twice, once as
  a private `def` and once as a literal argument to `frontiermath-doc-ref`.
  Instance 6's "two sources kept in sync by hand" is not only between the
  dependency map and this file.

### DERIVE revisions recorded

The reflexive-descendants correction, which reduced five missing meets to one
(`4d748660`); `software-design/adapter-pattern` becoming citable and closing
instance 5's unproduced want; `gauntlet/placenta-transfer` replacing a
stretched reading in instance 4; and the grain declarations added to all four
cascades that choose one.

PROOF-2a clauses 1–6 have been computed over this click by claude-10 —
`exemplar/clauses_1_6.clj` → `click-001-clauses.edn`. The results are in that
file rather than restated here.

## INSTANTIATE — demonstrations

**Exit criterion:** every completion criterion has a concrete demonstration, and
a new person could reproduce it from the mission doc. **Met for instance 4**,
which is the scope the IDENTIFY exit set ("pick one instance"). Not met for the
mission, which has seven other instances.

Instance 4's three completion criteria are its three want tokens. All three are
now observed met, each by a check recorded in
`exemplar/click-001-enactment.edn`:

| want | check | result |
|---|---|---|
| `caller-converted` | test | passed — `request-review!` asks for `:reviewer`; tickle's implementer default and mentor resolve through `roles/seat-for` |
| `redirect-test` | test | passed — rebinding `:reviewer` from claude-1 to oxf-codex-7 changes which seat is asked, and the same test fails against the old code |
| `prefix-routing-retired` | grep | passed — no `str/starts-with?` provider branch remains |

Reproduce it without help:

```
git -C /home/joe/code/futon3c show eafd07b7 d6927670 139caa97
clojure -M:test -n futon3c.agency.roles-test
python3 holes/labs/M-futon-seams/exemplar/enumerate_sites.py
bb holes/labs/M-futon-seams/exemplar/clauses_1_6.clj
```

**The first attempt built the wrong thing, and that record stays.**
`exemplar/click-001-outcome.edn` is the enactment at `8e5c431e`: it read the
registered `:agent/type` and answered which **provider** an agent is, where the
chosen cascade had specified the **role** grain. All three wants came back
`:partial`. It is kept rather than replaced because it is the mission's
evidence for why phase order is not ceremony — the implementer worked from
this mission's defect description rather than from a DERIVE that had met its
exit, and built a reasonable thing that was not the chosen thing.

`exemplar/click-001-enactment.edn` records the whole sequence: 8 attempts, 7
succeeding, with the grain step failing once (attempt 1) and succeeding at
attempt 3. Two conformance deviations are recorded rather than smoothed:

- **step order** — `realtime/mode-gate` (retire the prefix routing) was done in
  attempt 1, before its guard `:redirect-test` held; the cascade orders it last.
- **scope** — `tickle_orchestrate`'s rotation roster still names seats. It lists
  seats to cycle rather than looking a role up, so it was left.

**What remains for this instance.** `exemplar/sites.edn` (from
`enumerate_sites.py`, which is the `:sites-enumerated` token made checkable)
counts **43 seat literals across 12 files** and **one routing site**: the
library-loop adapter's check on the codex binary name. That site and the
rotation roster are the instance's remaining coupling, and neither is a
`str/starts-with?` on an agent id.

## DOCUMENT

**Exit criterion:** someone browsing the docbook can discover what this mission
built without knowing it exists. **Not started.**

The only rendered account is the working page at
`zone.hyperreal.enterprises/wip/seams.html`, reachable by URL alone. Nothing
navigates to it and there is no docbook entry.

### Proposed additional exit criterion (Rob, via Joe, 2026-09-24)

Rob's suggestion: DOCUMENT should be discharged by **building a seam, in this
mission's sense, for the annotation-of-user-inputs feature** — the loop that
records each operator turn, has a delegate agent interpret it into intents,
cues and pattern citations, and renders it. Documenting the mission by
extracting one seam is a working example rather than prose about seams, and
the feature is one this mission's instance 7 already names.

**What the seam would be**, from what the feature is today:

- `emacs/session-turn-analysis.el` — 497 lines, of which 48 touch buffers,
  overlays, points, markers, faces or windows. The other nine tenths is a
  turn parser, a record writer, a delegate dispatcher and the cue-vocabulary
  learner, none of which needs an editor.
- The record format has **five** readers or writers: that file,
  `scripts/session_turn_analysis.py` (the validator), `scripts/turn_batch.py`
  (the historical path), `scripts/turn_margin_html.py` (the renderer) and
  `scripts/xlate.py`. They agree by convention.
- The boundary: **turn → record** (parse, split into sentences with exact
  offsets, extract quoted regions) and **record → dispatch** (choose a
  delegate, build the brief, deliver it, report failure). Both are pure
  functions over text plus one I/O call.

**The evidence that the convention is already drifting**, which is what makes
this a seam rather than a tidy-up: the `>>>` block-quote convention is
implemented in the Emacs path alone. A turn recorded live has its quoted
block replaced by the token `QUOTE` and the text preserved beside the record;
the same turn recorded through `turn_batch.py` keeps the quoted block inline
as though the operator had said it. Two paths, one format, different meanings
for the same input. The sentence splitters were checked against each other on
abbreviations, decimals and URLs and agree there — so the drift is in what
each path knows about the format, not in how each parses a sentence.

This is recorded as **proposed**, not adopted. It changes what DOCUMENT has
to do, and the mission's IDENTIFY exit deliberately scopes work to one
instance at a time.
