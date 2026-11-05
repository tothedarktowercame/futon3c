# Labels for shard-3 (see build.py).  Ids/texts come from the inputs verbatim.
T = "claude-15-turn-"

def f(turn, s, i=0):
    return f"{T}{turn}-f-s{s}-{i}"

def r(turn, i):
    return f"{T}{turn}-r-{i}"

LABELS = {}

# ---- turn-0aeNn8 (turn 21) ----
LABELS["turn-0aeNn8"] = [
 {"act": f(21,1), "kind": "disagree", "move": "disagree/already-built",
  "quote": "already considerably done in the form of xiaoxiang and daxiang"},
 {"act": f(21,2), "kind": "explain", "move": "explain/analogy",
  "quote": "The hook structure is similar to way 象 works."},
 {"act": f(21,3), "kind": "constrain", "move": "constrain/for-now",
  "quote": "Let's not reinvent the wheel.", "opens": "no-reinvention"},
 {"act": r(21,0), "kind": "explain", "move": "explain/mapping",
  "quote": "my rule-based entity extraction plays 小象's role",
  "answers": [{"act": f(21,1), "quote": "the TF-IDF candidate pairs play `find-many`'s role", "effect": "closes"},
              {"act": f(21,2), "quote": "the TF-IDF candidate pairs play `find-many`'s role", "effect": "closes"}],
  "within": "no-reinvention"},
 {"act": r(21,1), "kind": "propose", "move": "propose/next-step",
  "quote": "Treat a memory write as an act in the turn that made it",
  "answers": [{"act": f(21,2), "quote": "the way 大象 already joins a turn's commits", "effect": "closes"}],
  "opens": "memory-writes-as-acts"},
 {"act": r(21,2), "kind": "qualify", "move": "qualify/caveat",
  "quote": "I've read the READMEs, not `turn-seam/dispatch-contract.md`",
  "answers": [], "confidence": "high"},
 {"act": r(21,3), "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "Shall I read the dispatch contract and the 大象 commit join", "answers": []},
]

# ---- turn-1ij7FV (turn 10) ----
LABELS["turn-1ij7FV"] = [
 {"act": f(10,1,0), "kind": "redirect", "move": "redirect/back-to-plan",
  "quote": "ok, let's go back to late August"},
 {"act": f(10,1,1), "kind": "prioritize", "move": "prioritize/next-milestone",
  "quote": "then milestone 3"},
 {"act": r(10,0), "kind": "report", "move": "report/partial",
  "quote": "The `ls -l` listings are committed (`b64aa43`).",
  "answers": [{"act": f(10,1,0), "quote": "The `ls -l` listings are committed (`b64aa43`).", "effect": "closes"},
              {"act": f(10,1,1), "quote": "candidate selection and the judge both work", "effect": "keeps"}]},
 {"act": r(10,1), "kind": "report", "move": "report/components",
  "quote": "picks 631 pairs with no model involved",
  "answers": [{"act": f(10,1,1), "quote": "asks for one of the spec's verdicts", "effect": "keeps"}]},
 {"act": r(10,2), "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "one decision: which model judges the full 631 pairs", "answers": []},
]

# ---- turn-4Vnjnj (turn 20) ----
LABELS["turn-4Vnjnj"] = [
 {"act": f(20,1), "kind": "explore", "move": "explore/analogy",
  "quote": "at -40°, water thrown into the air turns into crystals before it hits the ground"},
 {"act": f(20,2), "kind": "propose", "move": "propose/guiding-principle",
  "quote": "That could be a useful guiding principle for our DB.", "opens": "crystallise-on-write"},
 {"act": r(20,0), "kind": "explain", "move": "explain/reading-of-idea",
  "quote": "structure is added at write time, on the way in",
  "answers": [{"act": f(20,2), "quote": "structure is added at write time, on the way in", "effect": "closes"}],
  "within": "crystallise-on-write"},
 {"act": r(20,1), "kind": "explain", "move": "explain/concretisation",
  "quote": "Hot memory is the puddle.",
  "answers": [{"act": f(20,1), "quote": "Hot memory is the puddle.", "effect": "closes"},
              {"act": f(20,2), "quote": "the ingest step does that work", "effect": "closes"}],
  "within": "crystallise-on-write"},
 {"act": r(20,2), "kind": "report", "move": "report/gap-status",
  "quote": "Entities and the conflict check still only run as batches",
  "answers": [{"act": f(20,2), "quote": "Entities and the conflict check still only run as batches", "effect": "keeps"}]},
 {"act": r(20,3), "kind": "propose", "move": "propose/plan",
  "quote": "rule-based entity extraction (seed aliases, repos, seats, ports) runs in the hook itself",
  "answers": [{"act": f(20,2), "quote": "the hook queues the new memory for a Haiku entity pass", "effect": "closes"}],
  "opens": "write-time-structure"},
 {"act": r(20,4), "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "Shall I build 1 and 2?", "answers": []},
]

# ---- turn-5UjoUi (turn 31) ----
LABELS["turn-5UjoUi"] = [
 {"act": f(31,1,0), "kind": "propose", "move": "propose/revive-agent",
  "quote": "we might be able to find the relevant agent and revive them"},
 {"act": f(31,1,1), "kind": "qualify", "move": "qualify/caveat",
  "quote": "it's been a few days since they did any work"},
 {"act": f(31,1,2), "kind": "propose", "move": "propose/checkpoint",
  "quote": "make a Checkpoint in the M-象-2000 mission document", "opens": "M-象-2000-checkpoint"},
 {"act": f(31,2), "kind": "propose", "move": "propose/handoff-plan",
  "quote": "pass that to an available agent when we have a specific need"},
 {"act": f(31,3), "kind": "clarify", "move": "clarify/introduce-topic",
  "quote": "As for the \"inbox from signals that are explicit\""},
 {"act": f(31,4), "kind": "clarify", "move": "clarify/weigh-deferral",
  "quote": "whether we need to wait until the M-象-2000 issue is solved"},
 {"act": f(31,5), "kind": "propose", "move": "propose/tandem-handover",
  "quote": "ask claude-16 to take over on that project"},
 {"act": r(31,0), "kind": "report", "move": "report/checkpoint-contents",
  "quote": "no agent has worked on the mission since 10-04",
  "answers": [{"act": f(31,1,2), "quote": "A new file, `holes/labs/M-象-2000/CHECKPOINT-2026-10-11-false-open-ports.md`", "effect": "closes"},
              {"act": f(31,1,0), "quote": "claude-17 is still the owner of record", "effect": "keeps"}]},
 {"act": r(31,1), "kind": "explain", "move": "explain/answer-question",
  "quote": "Mostly no. Part of the explicit-signal inbox doesn't depend on answers being linked",
  "answers": [{"act": f(31,4), "quote": "Part of the explicit-signal inbox doesn't depend on answers being linked at all", "effect": "closes"}]},
 {"act": r(31,2), "kind": "propose", "move": "propose/endorse-handover",
  "quote": "I think that fits well.",
  "answers": [{"act": f(31,5), "quote": "claude-16 already found the outage, knows the turn-service code", "effect": "closes"}]},
 {"act": r(31,3), "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "Shall I bell claude-16 asking it to take over M-象-2000?", "answers": []},
]

# ---- turn-8VoJLF (turn 29) ----
LABELS["turn-8VoJLF"] = [
 {"act": f(29,1), "kind": "constrain", "move": "constrain/for-now",
  "quote": "Sure, let's take it one step at a time.", "opens": "one-step-at-a-time"},
 {"act": r(29,0), "kind": "report", "move": "report/false-open-census",
  "quote": "the kernel counts 24 open acts, and about 20 of them were answered or done",
  "within": "one-step-at-a-time"},
 {"act": r(29,1), "kind": "report", "move": "report/genuinely-open",
  "quote": "Four items, which I'd also have named by hand"},
 {"act": r(29,2), "kind": "report", "move": "report/numbers",
  "quote": "1,857 open, split three ways:"},
 {"act": r(29,3), "kind": "explain", "move": "explain/impact-on-measures",
  "quote": "It changes step 3 of the plan."},
 {"act": r(29,4), "kind": "propose", "move": "propose/two-ways-forward",
  "quote": "Report the false-open rate to the 象/kernel side"},
 {"act": r(29,5), "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "Shall I do both", "answers": []},
 {"act": r(29,6), "kind": "report", "move": "report/done",
  "quote": "It's done and holds up under my own check.",
  "note": "kind inferred (not in the kernel's act list)"},
 {"act": r(29,7), "kind": "verify", "move": "verify/checked-live",
  "quote": "5 random analysed records per day for 10-09, 10-10 and 10-11"},
 {"act": r(29,8), "kind": "report", "move": "report/delegated-numbers",
  "quote": "243 records replayed, oldest first"},
 {"act": r(29,9), "kind": "qualify", "move": "qualify/left-unassigned",
  "quote": "It isn't urgent, so I've left it unassigned"},
 {"act": r(29,10), "kind": "explain", "move": "explain/cross-check-use",
  "quote": "the follower's computed ports can now be compared against the live service"},
 {"act": r(29,11), "kind": "continue", "move": "continue/waiting-on-decision",
  "quote": "still waiting on you, from my last message", "answers": []},
]

# ---- turn-8qn3cQ (turn 27) ----
LABELS["turn-8qn3cQ"] = [
 {"act": f(27,1), "kind": "ask-action", "move": "ask-action/commission",
  "quote": "please restore the setting & backfill via 象-sonnet or codex-70 as appropriate"},
 {"act": r(27,0), "kind": "report", "move": "report/done",
  "quote": "your daemon's `session-mode-turn-recorder` is set to `jvm`",
  "answers": [{"act": f(27,1), "quote": "appended to `~/.emacs-graph/init.el` with a comment", "effect": "keeps"}],
  "note": "restore half done and persisted; backfill half follows in the next paragraph"},
 {"act": r(27,1), "kind": "delegate", "move": "delegate/to-agent",
  "quote": "I sent it to codex-70 rather than 象-sonnet.",
  "answers": [{"act": f(27,1), "quote": "this job replays settlement inside the Agency JVM", "effect": "keeps"}]},
 {"act": r(27,2), "kind": "qualify", "move": "qualify/caveat",
  "quote": "The follower keeps mirroring into the scratch store.", "answers": []},
]

# ---- turn-CczJhL (turn 18) ----
LABELS["turn-CczJhL"] = [
 {"act": f(18,1), "kind": "qualify", "move": "qualify/depends-on-use-cases",
  "quote": "the exact design depends a bit on what the use-cases are"},
 {"act": f(18,2), "kind": "propose", "move": "propose/merge-workstream",
  "quote": "we may be at a point where could start to merge this workstream with M-象-2000",
  "opens": "merge-with-M-象-2000"},
 {"act": f(18,3), "kind": "explore", "move": "explore/search-efficiency",
  "quote": "how to search it efficiently"},
 {"act": f(18,4), "kind": "report", "move": "report/concept",
  "quote": "what I've come up with is the concept of a \"pattern cascade\""},
 {"act": f(18,5), "kind": "explain", "move": "explain/accumulator",
  "quote": "searched with an accumulator that accumulates a heuristic for solving the problem"},
 {"act": f(18,6), "kind": "explain", "move": "explain/accumulator-example",
  "quote": "a math problem, specifically an analysis problem, specifically a complex analysis problem"},
 {"act": f(18,7), "kind": "clarify", "move": "clarify/locate-in-theory",
  "quote": "much more like the 3/2 category elaboration that codex-68 is working on"},
 {"act": f(18,8), "kind": "retract", "move": "retract/conditional",
  "quote": "isn't *necessarily* a permanent thing"},
 {"act": r(18,0), "kind": "explain", "move": "explain/accumulator-mechanics",
  "quote": "A query isn't a lookup.",
  "answers": [{"act": f(18,5), "quote": "It's a walk that refines a description step by step", "effect": "closes"},
              {"act": f(18,3), "quote": "it narrows the candidate memories", "effect": "keeps"}]},
 {"act": r(18,1), "kind": "qualify", "move": "qualify/caveat",
  "quote": "I couldn't find codex-68's 3/2-category elaboration in a quick search",
  "answers": [{"act": f(18,7), "quote": "That's a guess until I've read it.", "effect": "keeps"}]},
 {"act": r(18,2), "kind": "clarify", "move": "clarify/ask-decision",
  "quote": "Is that the one you have in mind",
  "answers": [{"act": f(18,1), "quote": "The math-problem example suggests the first test case", "effect": "closes"}]},
 {"act": r(18,3), "kind": "explain", "move": "explain/accept-retraction",
  "quote": "Noted. I'll stay off the War Machine until you say otherwise",
  "answers": [{"act": f(18,8), "quote": "this is a reasonable test of whether cool memories would change that", "effect": "closes"}]},
 {"act": r(18,4), "kind": "continue", "move": "report/in-flight",
  "quote": "The entity extraction shards are still running.", "answers": []},
]

# ---- turn-GnZeXU (turn 14) ----
LABELS["turn-GnZeXU"] = [
 {"act": f(14,1,0), "kind": "approve", "move": "approve/plain", "quote": "ok"},
 {"act": f(14,1,1), "kind": "prioritize", "move": "prioritize/part-one",
  "quote": "please start with part 1"},
 {"act": r(14,0), "kind": "report", "move": "report/mechanism",
  "quote": "The 18 `restates` pairs fall into 14 groups.",
  "answers": [{"act": f(14,1,1), "quote": "Each group keeps one memory and the others become aliases", "effect": "closes"}]},
 {"act": r(14,1), "kind": "report-problem", "move": "report-problem/self-correction",
  "quote": "two problems, both fixed"},
 {"act": r(14,2), "kind": "verify", "move": "verify/checked-live",
  "quote": "I tested on `park-needs-session-id` → `emacs-park-needs-session`.",
  "answers": [{"act": f(14,1,0), "quote": "The unit tests (29 assertions), clj-kondo and the parenthesis check pass", "effect": "closes"}]},
 {"act": r(14,3), "kind": "report", "move": "report/proposals-list",
  "quote": "Most are the same lesson filed under two projects"},
 {"act": r(14,4), "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "Applying merges is your call", "answers": []},
]

# ---- turn-L7qzfD (turn 23) ----
LABELS["turn-L7qzfD"] = [
 {"act": f(23,1,0), "kind": "prioritize", "move": "prioritize/review-first",
  "quote": "Before we proceed"},
 {"act": f(23,1,1), "kind": "delegate", "move": "delegate/to-agent",
  "quote": "let's bell a description of the work so far and the plan to claude-16"},
 {"act": r(23,0), "kind": "report", "move": "report/dispatched",
  "quote": "The packet is in the repo, committed",
  "answers": [{"act": f(23,1,1), "quote": "at `~/code/minus40/docs/REVIEW-REQUEST-2026-10-11-claude-16.md`", "effect": "closes"}]},
 {"act": r(23,1), "kind": "report-problem", "move": "report-problem/self-correction",
  "quote": "My first bell went in brief mode",
  "answers": [{"act": f(23,1,1), "quote": "I retracted it with a notice and resent in work mode", "effect": "keeps"}]},
 {"act": r(23,2), "kind": "report", "move": "report/parked",
  "quote": "The park id is `park-f1a6dff2-6260-4592-8f7d-4b47ee2eee6b`",
  "answers": [{"act": f(23,1,1), "quote": "When the reply lands I'll check that claude-16 actually read files", "effect": "keeps"}]},
 {"act": r(23,3), "kind": "report", "move": "report/duplicate-resolved",
  "quote": "This is the duplicate job finishing.",
  "answers": [{"act": f(23,1,1), "quote": "same findings, same six answers", "effect": "closes"}]},
 {"act": r(23,4), "kind": "continue", "move": "continue/waiting-on-decision",
  "quote": "The same two questions: should I investigate the turn-evidence outage read-only",
  "answers": []},
]

# ---- turn-LtNoP5 (turn 36) ----
LABELS["turn-LtNoP5"] = [
 {"act": f(36,1), "kind": "approve", "move": "approve/plain", "quote": "yes to both"},
 {"act": r(36,0), "kind": "report", "move": "report/relayed",
  "quote": "I've passed both to claude-16 with your words quoted.",
  "answers": [{"act": f(36,1), "quote": "I've passed both to claude-16 with your words quoted.", "effect": "closes"}]},
 {"act": r(36,1), "kind": "report", "move": "report/next-plan",
  "quote": "claude-16 will make the two kernel changes as small packets with tests",
  "answers": [{"act": f(36,1), "quote": "recomputing only live sessions", "effect": "keeps"}]},
]

# ---- turn-MDwJCW (turn 16) ----
LABELS["turn-MDwJCW"] = [
 {"act": f(16,1), "kind": "prioritize", "move": "prioritize/hook-first",
  "quote": "Let's start on the hook first."},
 {"act": f(16,2), "kind": "propose", "move": "propose/debugger-vision",
  "quote": "we could start to run all of FUTON in a kind of debugger", "opens": "FUTON-debugger"},
 {"act": f(16,3), "kind": "explain", "move": "explain/error-to-agent",
  "quote": "any error gets raised to an agent that then has to deal with it somehow"},
 {"act": f(16,4), "kind": "qualify", "move": "qualify/sideline",
  "quote": "That's a sideline from this -40° stuff but a bit related"},
 {"act": f(16,5), "kind": "clarify", "move": "clarify/check-understanding",
  "quote": "I hope this makes sense."},
 {"act": f(16,6), "kind": "explain", "move": "explain/convergence-vision",
  "quote": "-40° would be a kind of convergence of the intent based system"},
 {"act": r(16,0), "kind": "report", "move": "report/built",
  "quote": "runs after every Write, Edit and Bash call",
  "answers": [{"act": f(16,1), "quote": "It checks the memory files' sizes and modification times", "effect": "closes"}]},
 {"act": r(16,1), "kind": "verify", "move": "verify/checked-live",
  "quote": "an appended file, a new file and a deleted file all became the right versions",
  "answers": [{"act": f(16,1), "quote": "a rerun with nothing changed wrote nothing", "effect": "closes"}]},
 {"act": r(16,2), "kind": "qualify", "move": "qualify/caveat",
  "quote": "It writes to the scratch store on :7093",
  "answers": [{"act": f(16,1), "quote": "so it only records while that bg.py job is up", "effect": "keeps"}]},
 {"act": r(16,3), "kind": "extend", "move": "extend/settings-discovery",
  "quote": "`autoMemoryEnabled: false` would switch file memory off entirely",
  "answers": [], "note": "bears on the previous turn's convince-Claude question, which is neither a fragment nor in previous_reply_acts"},
 {"act": r(16,4), "kind": "explain", "move": "explain/endorse-idea",
  "quote": "It makes sense, and it fits this store",
  "answers": [{"act": f(16,5), "quote": "It makes sense, and it fits this store", "effect": "closes"},
              {"act": f(16,3), "quote": "A failure anywhere (a JVM exception, a failed job", "effect": "closes"},
              {"act": f(16,2), "quote": "an error is an event with a time, a place and a status", "effect": "keeps"}],
  "within": "FUTON-debugger"},
 {"act": r(16,5), "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "Part 2 (entities), the MCP tool, or a design note on the FUTON debugger?", "answers": []},
]

# ---- turn-MLCYwI (turn 32) ----
LABELS["turn-MLCYwI"] = [
 {"act": f(32,1), "kind": "delegate", "move": "delegate/to-agent",
  "quote": "Yes please, bell claude-16."},
 {"act": f(32,2), "kind": "approve", "move": "approve/plain",
  "quote": "And yes, we can start the explicit-signal inbox."},
 {"act": f(32,3), "kind": "propose", "move": "propose/include-faults",
  "quote": "I'd like to include the report-futon-bug \"concept\""},
 {"act": f(32,4), "kind": "explain", "move": "explain/signal-scope",
  "quote": "explicit signals can include all of the error and warning types"},
 {"act": f(32,5), "kind": "report-problem", "move": "report-problem/observed",
  "quote": "Emacs *Warnings* and messages that I otherwise would have to notice and report"},
 {"act": f(32,6), "kind": "explain", "move": "explain/relate-debugger",
  "quote": "This is related to the concept of running FUTON in a debugger"},
 {"act": f(32,7), "kind": "propose", "move": "propose/weave-together",
  "quote": "So, this is an opportunity to weave them together.", "opens": "weave-together"},
 {"act": r(32,0), "kind": "report", "move": "report/dispatched",
  "quote": "The packet points it at the Checkpoint and the test case",
  "answers": [{"act": f(32,1), "quote": "The owner of record changes only if it accepts.", "effect": "closes"}]},
 {"act": r(32,1), "kind": "report", "move": "report/built",
  "quote": "runs every 60 s under bg.py and reuses the bug collector's secret scrubbing",
  "answers": [{"act": f(32,3), "quote": "reads:", "effect": "keeps"},
              {"act": f(32,4), "quote": "Emacs `*Warnings*` and error-like `*Messages*` lines", "effect": "closes"}]},
 {"act": r(32,2), "kind": "report", "move": "report/first-findings",
  "quote": "8 faults plus 5 low",
  "answers": [{"act": f(32,5), "quote": "象 analysis 403 \"monthly usage limit\"** (11×)", "effect": "keeps"}]},
 {"act": r(32,3), "kind": "report", "move": "report/inbox-view",
  "quote": "rewritten every minute",
  "answers": [{"act": f(32,2), "quote": "Explicit section first", "effect": "closes"}]},
 {"act": r(32,4), "kind": "explain", "move": "explain/design-fit",
  "quote": "In Elephant-2000 terms the fit is direct.",
  "answers": [{"act": f(32,7), "quote": "A warning is an assertion made by a component", "effect": "closes"},
              {"act": f(32,6), "quote": "Running FUTON in a debugger then means faults become acts with owners", "effect": "keeps"}],
  "within": "weave-together"},
 {"act": r(32,5), "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "Step 2 of the earlier plan (crystallisation at write time) is still queued", "answers": []},
 {"act": r(32,6), "kind": "verify", "move": "verify/checked-live",
  "quote": "The ownership commit:** `c6f2b068`",
  "answers": [{"act": f(32,1), "quote": "The job:** done in work mode.", "effect": "closes"}]},
 {"act": r(32,7), "kind": "report", "move": "report/findings",
  "quote": "No replies on the records."},
 {"act": r(32,8), "kind": "report-problem", "move": "report-problem/self-correction",
  "quote": "That was wrong on both counts"},
 {"act": r(32,9), "kind": "report", "move": "report/plan",
  "quote": "A lab note with the census script."},
 {"act": r(32,10), "kind": "explain", "move": "explain/endorse-proposal",
  "quote": "claude-16's proposal is the fit I'd hoped for",
  "answers": [{"act": f(32,7), "quote": "claude-16's proposal is the fit I'd hoped for", "effect": "closes"}],
  "within": "weave-together"},
 {"act": r(32,11), "kind": "continue", "move": "report/next",
  "quote": "Crystallisation at write time, as queued.",
  "answers": [{"act": f(32,2), "quote": "claude-16's steps 2 and 3 run in parallel", "effect": "keeps"}]},
]

# ---- turn-Pn04er (turn 7) ----
LABELS["turn-Pn04er"] = [
 {"act": f(7,1,0), "kind": "qualify", "move": "qualify/hesitation", "quote": "hm..."},
 {"act": f(7,2), "kind": "propose", "move": "propose/floats-idea",
  "quote": "insertion time vs record time can differ without any issue"},
 {"act": r(7,0), "kind": "verify", "move": "verify/checked-live",
  "quote": "futon1b's existing hyperedge route already takes `:hx/valid-time`",
  "answers": [{"act": f(7,2), "quote": "Querying at 08-15 returned nothing, at 09-10 returned v1", "effect": "closes"}]},
 {"act": r(7,1), "kind": "propose", "move": "propose/milestone-plan",
  "quote": "Each memory gets one stable `:memory/version` hyperedge.",
  "answers": [{"act": f(7,2), "quote": "\"What did this memory say on date X\" then becomes a single valid-time query", "effect": "closes"}],
  "opens": "milestone-2-versions"},
 {"act": r(7,2), "kind": "qualify", "move": "qualify/caveat",
  "quote": "Versions have to be replayed oldest first.", "within": "milestone-2-versions"},
 {"act": r(7,3), "kind": "report", "move": "report/done",
  "quote": "I've updated the plan in the README (`a80af71`)",
  "answers": [{"act": f(7,2), "quote": "I've updated the plan in the README (`a80af71`)", "effect": "closes"}]},
]

# ---- turn-UBmb3D (turn 3) ----
LABELS["turn-UBmb3D"] = [
 {"act": f(3,1,0), "kind": "qualify", "move": "qualify/least-change",
  "quote": "so, the least change would be to use XTDB"},
 {"act": f(3,1,1), "kind": "propose", "move": "propose/explore-alternatives",
  "quote": "We should explore the other options."},
 {"act": f(3,2), "kind": "explain", "move": "explain/pieces-in-place",
  "quote": "we have a lot of the relevant pieces in place already"},
]

# ---- turn-UHz87W (turn 5) ----
LABELS["turn-UHz87W"] = [
 {"act": f(5,1), "kind": "redirect", "move": "redirect/to-technical",
  "quote": "let's focus on the technical aspects.", "opens": "technical-aspects"},
 {"act": f(5,2), "kind": "propose", "move": "propose/data-source",
  "quote": "We could reade claude code memories rather than claude.ai memories"},
]

# ---- turn-YjPvUs (turn 39) ----
LABELS["turn-YjPvUs"] = [
 {"act": f(39,1,0), "kind": "ask-action", "move": "ask-action/commission",
  "quote": "ok, please write it"},
 {"act": f(39,1,1), "kind": "propose", "move": "propose/review-path",
  "quote": "we can pass to claude-16 next, and codex-68 for a second read after that"},
 {"act": r(39,0), "kind": "report", "move": "report/done",
  "quote": "It's written from what is built and what's been ruled today",
  "answers": [{"act": f(39,1,0), "quote": "every gap marked as one of 13 numbered holes", "effect": "closes"}]},
 {"act": r(39,1), "kind": "explain", "move": "explain/found-theory",
  "quote": "I found it: the 3/2-blend carrier"},
 {"act": r(39,2), "kind": "report", "move": "report/dispatched",
  "quote": "claude-16 has the review as job",
  "answers": [{"act": f(39,1,1), "quote": "read-only, replying with numbered comments", "effect": "closes"}]},
 {"act": r(39,3), "kind": "qualify", "move": "qualify/open-questions",
  "quote": "four questions for you, in §9, no rush", "answers": []},
]

# ---- turn-b3dxwJ (turn 9) ----
LABELS["turn-b3dxwJ"] = [
 {"act": f(9,1), "kind": "approve", "move": "approve/plain", "quote": "yes let's go this route"},
 {"act": r(9,0), "kind": "report", "move": "report/what-was-added",
  "quote": "Existence markers.",
  "answers": [{"act": f(9,1), "quote": "A version with no text means the memory existed then", "effect": "closes"}]},
 {"act": r(9,1), "kind": "report-problem", "move": "report-problem/observed",
  "quote": "The first run reported 7 contradictions and 271 markers.",
  "answers": [{"act": f(9,1), "quote": "With nanosecond birth times there are no contradictions", "effect": "keeps"}]},
 {"act": r(9,2), "kind": "verify", "move": "verify/checked-live",
  "quote": "I traced three markers back to the records they cite and they were real",
  "answers": [{"act": f(9,1), "quote": "tests, clj-kondo and the parenthesis check all pass", "effect": "closes"}]},
 {"act": r(9,3), "kind": "qualify", "move": "qualify/caveat",
  "quote": "Transcripts start on 09-01."},
 {"act": r(9,4), "kind": "propose", "move": "propose/next-step",
  "quote": "The spot-check turned up a better source: `ls -l` output in transcripts"},
]

# ---- turn-bgSPxR (turn 28) ----
LABELS["turn-bgSPxR"] = [
 {"act": f(28,1), "kind": "report-problem", "move": "report-problem/observed",
  "quote": "I'm a bit unclear about the plan going forward"},
 {"act": r(28,0), "kind": "report", "move": "report/status",
  "quote": "The core of 40ºBelowDB exists and runs live on the scratch store",
  "answers": [{"act": f(28,1), "quote": "The core of 40ºBelowDB exists and runs live", "effect": "closes"}]},
 {"act": r(28,1), "kind": "propose", "move": "propose/plan",
  "quote": "Look at it together (short).",
  "answers": [{"act": f(28,1), "quote": "That decides what the next steps optimise for.", "effect": "closes"}],
  "opens": "plan-going-forward"},
 {"act": r(28,2), "kind": "report", "move": "report/open-questions-for-operator",
  "quote": "open questions that are yours", "answers": []},
 {"act": r(28,3), "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "My lean is 1 then 2", "answers": []},
]

# ---- turn-eebQ4p (turn 35) ----
LABELS["turn-eebQ4p"] = [
 {"act": f(35,1), "kind": "propose", "move": "propose/design-forward",
  "quote": "maybe we just need to use that data to design the interface we run going forward"},
 {"act": f(35,2), "kind": "constrain", "move": "constrain/for-now",
  "quote": "the historical data won't go away, and I don't want to read an old inbox"},
 {"act": f(35,3), "kind": "propose", "move": "propose/orphan-warning-route",
  "quote": "if otphaned items raise a warning in the future I could feed them into report-futon-bug"},
 {"act": r(35,0), "kind": "report", "move": "report/done",
  "quote": "The inbox now shows ports only for live sessions",
  "answers": [{"act": f(35,2), "quote": "That's 184 ports in 13 sessions instead of 2,827.", "effect": "closes"},
              {"act": f(35,1), "quote": "only ones opened from today on", "effect": "closes"}]},
 {"act": r(35,1), "kind": "report", "move": "report/done",
  "quote": "raises one warning for that session",
  "answers": [{"act": f(35,3), "quote": "your asks are still open in the session it left", "effect": "closes"}]},
 {"act": r(35,2), "kind": "report", "move": "report/done",
  "quote": "writes a normal report-futon-bug report with the stranded asks inline",
  "answers": [{"act": f(35,3), "quote": "It works for any fault, not just orphans.", "effect": "closes"}]},
 {"act": r(35,3), "kind": "report-problem", "move": "report-problem/self-correction",
  "quote": "I accidentally killed the scratch store on :7093"},
 {"act": r(35,4), "kind": "qualify", "move": "qualify/remaining-noise",
  "quote": "Even inside the 184 there are answered asks"},
 {"act": r(35,5), "kind": "qualify", "move": "qualify/perf",
  "quote": "Rebuilding the inbox page takes about 10 minutes per pass"},
]

# ---- turn-g0WX7n (turn 4) ----
LABELS["turn-g0WX7n"] = [
 {"act": f(4,1), "kind": "ask-action", "move": "ask-action/status-check",
  "quote": "did you get any results back?"},
]

# ---- turn-jlxoP5 (turn 15) ----
LABELS["turn-jlxoP5"] = [
 {"act": f(15,1), "kind": "explore", "move": "explore/steering-idea",
  "quote": "whether we can convince Claude NOT to use files"},
 {"act": r(15,0), "kind": "explain", "move": "explain/answer-question",
  "quote": "Probably yes, partly, with that one memory alone.",
  "answers": [{"act": f(15,1), "quote": "Probably yes, partly, with that one memory alone.", "effect": "closes"}]},
 {"act": r(15,1), "kind": "explain", "move": "explain/why-insufficient",
  "quote": "Claude Code's memory has two sides, and they behave differently",
  "answers": [{"act": f(15,1), "quote": "Each miss is a memory written to the wrong place", "effect": "keeps"}]},
 {"act": r(15,2), "kind": "propose", "move": "propose/alternative",
  "quote": "Ingest on write.",
  "answers": [{"act": f(15,1), "quote": "the live, merged, non-retracted memories", "effect": "closes"}],
  "opens": "files-as-view"},
 {"act": r(15,3), "kind": "explore", "move": "explore/measurable",
  "quote": "Whether the instruction alone would work is testable."},
 {"act": r(15,4), "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "Do you want the experiment first", "answers": []},
]

# ---- turn-kC0MbI (turn 38) ----
LABELS["turn-kC0MbI"] = [
 {"act": f(38,1,0), "kind": "ask-action", "move": "ask-action/status-check",
  "quote": "is there a *specification* for the system"},
 {"act": f(38,1,1), "kind": "propose", "move": "propose/write-spec",
  "quote": "maybe we should develop a specification of \"this part\""},
 {"act": r(38,0), "kind": "report", "move": "report/answer",
  "quote": "One system with three inputs and one notion of \"open\"",
  "answers": [{"act": f(38,1,0), "quote": "One system with three inputs and one notion of \"open\"", "effect": "closes"}]},
 {"act": r(38,1), "kind": "report", "move": "report/outline",
  "quote": "The objects:** fact, version, act, port, standing, fault, sorry",
  "answers": [{"act": f(38,1,1), "quote": "each with its two times (said and referred to)", "effect": "closes"}]},
 {"act": r(38,2), "kind": "clarify", "move": "clarify/ask-decision",
  "quote": "I don't know that work well enough to place it.",
  "answers": [{"act": f(38,1,1), "quote": "Is it the work in `mfuton-notes`, or somewhere else?", "effect": "keeps"}]},
 {"act": r(38,3), "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "Shall I draft it as a mission-style spec", "answers": []},
]

# ---- turn-kuEWAB (turn 17) ----
LABELS["turn-kuEWAB"] = [
 {"act": f(17,1), "kind": "explore", "move": "explore/directory-trick",
  "quote": "Maybe we could trick Claude into thinking that the memory system *is* a directory."},
 {"act": f(17,2), "kind": "explain", "move": "explain/precedent",
  "quote": "memories are all addressed with slash-separated paths"},
 {"act": f(17,3), "kind": "propose", "move": "propose/switch-off-route",
  "quote": "simply turning off the standard memory system and instructing Claude via CLAUDE.md"},
 {"act": f(17,4), "kind": "prioritize", "move": "prioritize/part-two",
  "quote": "For now let's work on the Part 2 to get it ready to work"},
 {"act": r(17,0), "kind": "report", "move": "report/done",
  "quote": "That finds 1,179 mentions of 144 entities in 290 of the 294 memories.",
  "answers": [{"act": f(17,4), "quote": "Tests (37 assertions), clj-kondo and the parenthesis check pass", "effect": "closes"}]},
 {"act": r(17,1), "kind": "explain", "move": "explain/answer-question",
  "quote": "Both routes are open.",
  "answers": [{"act": f(17,1), "quote": "would be `autoMemoryDirectory` pointed at a directory minus40 regenerates", "effect": "keeps"},
              {"act": f(17,3), "quote": "That's cleaner, but every session then depends on remembering the tool", "effect": "keeps"}]},
 {"act": r(17,2), "kind": "continue", "move": "report/next",
  "quote": "When the shards finish I'll merge, load, and run `about` on a few entities",
  "answers": [{"act": f(17,4), "quote": "to check the results against the memories", "effect": "keeps"}]},
]

# ---- turn-mIc3Eh (turn 11) ----
LABELS["turn-mIc3Eh"] = [
 {"act": f(11,1), "kind": "approve", "move": "approve/plain", "quote": "let's go with haiku"},
 {"act": r(11,0), "kind": "report", "move": "report/early-results",
  "quote": "In the first 26 judgements every quote checked out.",
  "answers": [{"act": f(11,1), "quote": "The conflicts look like real ones", "effect": "closes"}]},
 {"act": r(11,1), "kind": "report", "move": "report/built",
  "quote": "`minus40.sorries` turns the verdicts into futon1b records",
  "answers": [{"act": f(11,1), "quote": "Only verdicts with verbatim quotes are used", "effect": "closes"}]},
 {"act": r(11,2), "kind": "continue", "move": "report/next",
  "quote": "When the shards finish, I'll load everything into the scratch store"},
]

# ---- turn-n5EIxj (turn 12) ----
LABELS["turn-n5EIxj"] = [
 {"act": f(12,1), "kind": "ask-action", "move": "ask-action/status-check",
  "quote": "are they still going?"},
]

# ---- turn-oQdwzD (turn 22) ----
LABELS["turn-oQdwzD"] = [
 {"act": f(22,1), "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "I don't really want all of the agents' turns to be read by a second agent",
  "opens": "no-second-reader"},
 {"act": f(22,2), "kind": "prioritize", "move": "prioritize/benefit-of-tooling",
  "quote": "But we should get the benefit of the 大象 tooling."},
 {"act": f(22,3), "kind": "propose", "move": "propose/hook-it-all-up",
  "quote": "I reckon we're pretty much in a position to \"hook it all up\".", "opens": "hook-it-all-up"},
 {"act": f(22,4), "kind": "report", "move": "report/coverage-status",
  "quote": "we'd still be missing the embedding and vector-based search"},
 {"act": f(22,5), "kind": "explain", "move": "explain/live-system-goal",
  "quote": "an end-to-end system hooked up that we could start to look at \"live\""},
 {"act": f(22,6), "kind": "propose", "move": "propose/spare-instance",
  "quote": "you still have an extra futon1b we could work with"},
 {"act": f(22,7), "kind": "qualify", "move": "qualify/main-store-only",
  "quote": "some cool things we could only do in the main futon1b"},
 {"act": f(22,8), "kind": "propose", "move": "propose/crime-scene-case",
  "quote": "we'd also have a live case of \"your agent chat is a crime scene\""},
 {"act": r(22,0), "kind": "report", "move": "report/architecture",
  "quote": "writes one record per turn to `~/.emacs-graph/session-turn-analysis/turn-*.json`",
  "answers": [{"act": f(22,1), "quote": "so we add no reading", "effect": "closes"}],
  "within": "no-second-reader"},
 {"act": r(22,1), "kind": "propose", "move": "propose/pipeline",
  "quote": "Follow the turn records.",
  "answers": [{"act": f(22,3), "quote": "mirrors each settled turn into the store", "effect": "closes"},
              {"act": f(22,1), "quote": "never for turns", "effect": "closes"},
              {"act": f(22,8), "quote": "First case: the claude-14 session.", "effect": "closes"}],
  "within": "hook-it-all-up"},
 {"act": r(22,2), "kind": "qualify", "move": "qualify/deferred",
  "quote": "Embeddings and vector search:** still missing from the spec",
  "answers": [{"act": f(22,4), "quote": "They'd be a rebuildable sidecar later", "effect": "keeps"}]},
 {"act": r(22,3), "kind": "qualify", "move": "qualify/caveat",
  "quote": "Switching to the main futon1b would mean re-running the loads against :7073",
  "answers": [{"act": f(22,6), "quote": "and that's your call when we get there", "effect": "keeps"},
              {"act": f(22,7), "quote": "Switching to the main futon1b would mean re-running the loads against :7073", "effect": "keeps"}]},
 {"act": r(22,4), "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "Shall I build 1–4 in that order", "answers": []},
]

# ---- turn-poV4qM (turn 34) ----
LABELS["turn-poV4qM"] = [
 {"act": f(34,1), "kind": "propose", "move": "propose/working-division",
  "quote": "we work on our tasks and ping claude-16 when we need something related from the 象 side"},
 {"act": f(34,2), "kind": "propose", "move": "propose/operator-availability",
  "quote": "I can jump over to that REPL as needed as well."},
 {"act": r(34,0), "kind": "explain", "move": "explain/adopt-division",
  "quote": "Agreed. I'll ping claude-16 only when -40° needs something",
  "answers": [{"act": f(34,1), "quote": "Agreed. I'll ping claude-16 only when", "effect": "closes"},
              {"act": f(34,2), "quote": "you can jump between the two REPLs", "effect": "closes"}]},
 {"act": r(34,1), "kind": "report", "move": "report/done",
  "quote": "Done and live. A memory now picks up its structure as it's written",
  "answers": [{"act": f(34,1), "quote": "a conflict check within about 30 seconds", "effect": "keeps"}]},
 {"act": r(34,2), "kind": "explain", "move": "explain/mechanism",
  "quote": "It reuses the existing scripts rather than adding new machinery."},
 {"act": r(34,3), "kind": "verify", "move": "verify/checked-live",
  "quote": "A planted contradiction** in a throwaway test project"},
 {"act": r(34,4), "kind": "report-problem", "move": "report-problem/observed-then-fixed",
  "quote": "The first real edit logged `No module named 'crystallise'`."},
 {"act": r(34,5), "kind": "qualify", "move": "qualify/caveat",
  "quote": "the sorry isn't closed automatically yet"},
 {"act": r(34,6), "kind": "report", "move": "report/running-jobs",
  "quote": "all on the scratch store, all under bg.py"},
 {"act": r(34,7), "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "So either I start now with the measures that don't depend on ports", "answers": []},
]

# ---- turn-tqZHsF (turn 19) ----
LABELS["turn-tqZHsF"] = [
 {"act": f(19,1), "kind": "clarify", "move": "clarify/check-understanding",
  "quote": "Is that clarifying?"},
 {"act": r(19,0), "kind": "explain", "move": "explain/answer-question",
  "quote": "Yes, more than I expected.",
  "answers": [{"act": f(19,1), "quote": "Yes, more than I expected.", "effect": "closes"}]},
 {"act": r(19,1), "kind": "explain", "move": "explain/mapping",
  "quote": "Take one refinement step"},
 {"act": r(19,2), "kind": "explain", "move": "explain/why-gaps-matter",
  "quote": "Two of them are exactly where memory search would hit trouble"},
 {"act": r(19,3), "kind": "explain", "move": "explain/fit",
  "quote": "The entity aliases I just built are candidate identifications"},
 {"act": r(19,4), "kind": "qualify", "move": "qualify/caveat",
  "quote": "This is my reading of the shapes, not a checked correspondence."},
]

# ---- turn-zb5phn (turn 13) ----
LABELS["turn-zb5phn"] = [
 {"act": f(13,1), "kind": "clarify", "move": "clarify/ask-fact",
  "quote": "what is milestone 4?"},
]

# ---- turn-zjFdHQ (turn 2) ----
LABELS["turn-zjFdHQ"] = [
 {"act": f(2,1,0), "kind": "report", "move": "report/status",
  "quote": "ok i think it is connected now"},
 {"act": f(2,1,1), "kind": "ask-action", "move": "ask-action/commission",
  "quote": "can you grab a 40ºbelowdb spec from downloads"},
]
