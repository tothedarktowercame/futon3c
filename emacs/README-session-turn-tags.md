# Live passage tags

`M-x global-session-mode-turn-tags-mode` turns on local phrase tagging in current
and future initialized agent-chat REPL buffers. To enable it at Emacs startup:

```elisp
(require 'session-mode)
(global-session-mode-turn-tags-mode 1)
```

`M-x session-mode-turn-tags-mode` toggles one buffer. Full `session-mode` also
turns on this local component; it can be used independently of mission markup
and retrieved-pattern sigils.

While composing, matched phrases receive underlines only. There is no draft or
sent-turn summary string: the previous extra display line made terminal Emacs
text move while typing. The overlays never add display characters or line breaks.

These matches are lexical cues, not an automated classification of intent.
Codex-14 has now labelled 20 recorded passage spans by intent and distilled
105 phrases across 17 categories from the 100-turn pilot. The defaults include
approve, disagree, clarify, delegate, verify, prioritize, defer and others.
Standalone “but” is no longer a cue. `C-c s i` describes the underlined phrase at
point, or the draft’s recognized intents, in the echo area only on request.
This is an agent-curated cue vocabulary. Sent turns with unmatched sentences now
request additional interpretation from the conversation agent (see below). No model
is called on each keystroke.

Phrase matching is case-insensitive, checks boundaries, and accepts straight or
curly apostrophes. Quotes and negation still require interpretation. Agreement
and objection have distinct underline colours; hovering shows the phrase and tag.

`M-x customize-variable RET session-mode-turn-vocabulary` edits the literal
vocabulary. Its source default is `session-mode-turn-intent-vocabulary`; saved
human corrections remain authoritative over the live snapshot. Customize changes are
picked up on the next edit or `M-x session-mode-turn-tags-refresh`. Marimo edits
and ratings are not automatically synchronized with this Emacs vocabulary.

Feedback updates after 150 ms idle, using only the draft. No network, embedding,
or transcript scan occurs on the draft path. Overlay annotations are not text:
they do not alter what gets sent, buffer modification state, or undo history.
Tags remain on the latest sent operator turn; older turns are not backfilled.
Only one sent turn is retained to bound overlay accumulation in long sessions.
Incoming assistant text is not classified as operator input. Mode disable/buffer
kill cancels the buffer-local timer; disabling removes this component's overlays.

Validation:

```sh
emacs -Q --batch -L emacs -l test/session-mode-test.el -f ert-run-tests-batch-and-exit
```

The tests use the real agent-chat initializer and insertion routine, including
incoming messages shifting the prompt, mixed agreement/objection, negation,
editing away an old tag, marker isolation, timer cleanup, and new-buffer enable.

## Correct sentence classifications with !c

`!c` supplies **ordered labels for preceding sentences**, not a tag/keyword pair:

```text
I agree with that plan. We could add a second example.
!c approve extend
```

Return labels sentence 1 `approve` and sentence 2 `extend`. The remaining draft
stays unsent. A standalone `!c disagree redirect explain` labels the latest
operator turn captured since enabling this version, ignoring intervening agent
responses. Existing transcripts are not guessed or backfilled.

Sentence boundaries use Emacs sentence motion with single-space endings. If
sentence/label counts differ, the command displays the detected sentences and
changes nothing. Corrections are stored as ordered sentence text/label pairs
with author and timestamp. Existing cue phrases actually found in a labelled
sentence are reassigned to that label; the same phrase in several differently
labelled sentences retains those several labels. The whole labelled sentence is
also retained as an exact-match example. This updates active drafts and the
latest captured operator passage. Labels are extensible.

The previous `!c TAG PHRASE` interpretation is removed. Explicit vocabulary
editing remains available through Customize and `session-mode-turn-add-rule`.

Human labels are authoritative for the examples. Reassigning a phrase globally
is still a generalization: a phrase can mean something different in another
context. Exact sentence matches do not discover useful novel keywords. A small
model such as Haiku could extract candidate phrase spans from the saved examples;
**automatic vocabulary refinement is not connected**; the sent-turn analysis below
adds separate interpretations without overriding human corrections.

Rules and correction records are saved atomically together as version-2 JSON in
`session-mode-turn-rules-file` (inside `user-emacs-directory`, currently
`~/.emacs-graph/session-turn-vocabulary.json`). The store now includes labelled
passage text. Version-1 rule files remain readable. Save errors preserve both the
input and live rules; the command never invokes the conversation agent. Emacs
corrections remain separate from the Marimo rating store.

## Structure and additional analysis after sending

Every ordinary operator turn sent while local tagging is enabled gets a private
JSON record in `session-mode-turn-analysis-directory` (by default
`~/.emacs-graph/session-turn-analysis/`). It preserves source text, session/turn
identity, sentence boundaries, exact Unicode character offsets and literal cue
observations. A sentence without cues is `unresolved`; one with cues is only
`cue-only`, not semantically classified. This is sentence-level gap detection:
a recognized cue can still leave other clauses in that sentence uninterpreted.

If any sentence has no cue, `session-mode-turn-analyze-gaps` (default t) appends a
bounded structural-analysis request to the existing receiving agent's prompt.
There is no second invocation. Visible operator text and before-send evidence
hooks are unchanged; the provider transcript includes the clearly delimited
machine-added request. Background bells, continuations and walkie commands are
excluded. The receiving agent can analyze the whole turn in conversation context.

The agent records multiple passage intents, their targets and rationales, and
structural roles: context, condition, contrast, action, rationale, goal or
dependency. These are inferred role annotations, not yet a graph of relations
between passages. Uninterpreted sentences require an explicit unresolved reason.
Optional flexiarg candidates require a real canonical pattern ID and a fit
rationale after reading the pattern; the validator checks the declared ID and
stores the pattern content hash. It checks structure, not semantic correctness.
This supplies inputs for later pattern fitting; it does not implement a cascading
rewrite engine or change the existing embedding retrieval.

The prompt documents `scripts/session_turn_analysis.py template REQUEST` and
`complete REQUEST ANALYSIS.json`. Completion validates exact source spans and
all sentence IDs and atomically creates a separate `.analysis.json` result.
The original request is immutable. The result is explicitly agent-labelled,
not human-approved, and cannot silently replace an existing interpretation.
Requested does not mean complete: agents may omit the task or lack filesystem
access, leaving the request pending; there is no automatic retry worker.
A storage failure warns without losing the operator's conversation turn.

When a completed result exists at reply time, inferred fragments are underlined
on the latest sent turn. Their help text identifies the interpreting agent;
`C-c s i` reads it on request. Draft layout stays unchanged. Use
`C-c s a` (`session-mode-inspect-turn-analysis`) to open the latest result (or the
uncompleted request). Only the latest sent turn gets overlays; durable records
remain available for later analysis. Disable `session-mode-turn-analyze-gaps`
to retain structural recording without asking agents for interpretations.
