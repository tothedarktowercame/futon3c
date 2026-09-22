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
This is an agent-curated cue vocabulary. Sent turns now
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
**explicit agent-proposed reusable cues are now connected** (see below);
existing phrase assignments and human corrections take precedence.

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

`session-mode-turn-analysis-policy` (default `all`) appends a
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

When a completed result exists at reply time, only explicit keyword cues are underlined
on the latest sent turn. Their help text identifies the interpreting agent;
`C-c s i` reads it on request. Draft layout stays unchanged. Use
`C-c s a` (`session-mode-inspect-turn-analysis`) to open the latest result (or the
uncompleted request). Only the latest sent turn gets overlays; durable records
remain available for later analysis. Set `session-mode-turn-analysis-policy` to `never` to retain structural recording
without requesting interpretation, or `unmatched` to request it only for turns
with unmatched sentences. The default analyzes all ordinary operator turns.

Interpretation spans and `display_cues` are separate. A whole sentence may be
interpreted, but it is never implicitly used as a display span. Each cue must
be an exact source phrase, at most eight words and 80 characters. Their union
must leave most of a long sentence unmarked; splitting it into many fragments
does not evade that check. If no explicit cue expresses the inferred intent,
the analysis records `no_surface_cue` rather than manufacturing a keyword.
Older results without display cues remain inspectable but produce no inferred
underlines. New default redirection cues include “I would want” and “I would
prefer”; these are contextual hypotheses, not proof of redirection in every use.

### Pattern hints in the minibuffer

On an analyzed keyword, `C-c s i` shows the interpreted target and any recorded
flexiarg candidate ID with its fit rationale. A cue such as “I wonder if” is a
speech-act hint; the full proposal, its object, constraints and success criterion
are the input to pattern alignment. The analysis prompt now asks for substantive
keyword spans and comparisons to each pattern's context/IF/THEN. No candidate
means “No justified flexiarg alignment recorded”, not a fabricated nearest match.
Inferred hints take precedence over overlapping provisional lexical hints.

Navigation and the describe command pick up results that were saved after the
reply callback. The file timestamp is cached; an unchanged result is not
repainted. This refresh does not run on ordinary insertion/newline commands.
Pattern matching itself still belongs to the receiving agent; the local UI
only displays saved candidates, and does not run retrieval on each keystroke.

### Flag a tagging failure with !x

End a draft with a separate `!x` token, on the same line or a final line:

```
Could this become a learning loop? !x
```

Send normally. The marker is removed from the conversational text and retained
in the structural record as `tagging_failed: true`, alongside `original_text`.
It forces a request for substantive keyword interpretation even with policy
`never` or a successful lexical match. It does not create another agent call;
results arrive through the agent answering the turn. This is explicit negative
feedback, not a correct label and not an automatic vocabulary update. Analysis
is already requested on every ordinary turn by default; `!x` distinguishes the
ones the operator says failed. A bare `!x` asks for preceding draft text and
preserves the input. Quoted or mid-sentence mentions remain ordinary text.

### Comparing with embedding retrieval: current limits

Read-only discovery on 2026-09-22: querying
`GET /api/alpha/evidence?tag=context-retrieval&session-id=01a0c489-1ec2-7553-9e83-52d0f2bea66e&limit=3`
returned three records, each with three ranked IDs/scores. In these records,
`evidence/body` is an EDN string, not a JSON object. The `query` preview is 100
characters and mostly the routing envelope. Do not join it by the first words
of a turn or equate its numeric turn with the Emacs turn ID.

`dev/futon3c/dev.clj`, `emit-context-evidence!` and `context-retrieval!`, confirms
that retrieval runs after the agent reply. Its query combines up to 200
characters of extracted user text with up to 200 of the response, and persists
only a 100-character preview. The sampled previews begin with the routing header;
that is a retrieval-input issue to investigate separately, not evidence of a
semantic match. No retrieval service was changed for the !x implementation.

A valid comparison must bind the evidence ID to the exact operator source/turn,
freeze the agent candidates before revealing the embedding top three, and name
the two inputs. Compare exact ID overlap first; record broader conceptual matches
as separately judged annotations. The embedding ranking is a comparator, not
human ground truth. Existing records inspected here lack a verified exact link,
so no recovery score is reported or implemented by this change.

### Close the loop: analyzed cues become future draft cues

A completed analysis may now include `reusable_cues`, separate from display
spans. Each proposal includes exact source `start`, `end`, `text`, an `intent`,
and a rationale for why the phrase is useful in future turns. The validator
checks the source span and short-phrase constraints. When Emacs ingests the
result it atomically saves previously unassigned phrases in the live vocabulary,
with analysis-file and labeller provenance in `learned_cues`, and refreshes active
drafts. Saved rules are loaded in subsequent sessions. Ordinary display cues are
not automatically generalized; a project name need not become an intent keyword.

Existing phrase assignments win, including human `!c` corrections. New learning
is a provisional phrase hypothesis, not proof of intent in every quotation or
negation. Agents can propose these cues through the analysis result; they need
not edit the vocabulary file themselves. Results are ingested on the current
turn's reply or navigation refresh; this is not a background sweep of all old
records. `just testing → verify` is the first live example through this path.

For that example, `apparatus/done-is-observed-running` is a contextual candidate:
Joe checked whether a saved analysis actually changed live typing behaviour.
It is not a universal synonym for testing, nor does a one-off check establish
the standing comparator required by that pattern. Pattern IDs are not copied
into global phrase rules: each turn still needs contextual alignment.
