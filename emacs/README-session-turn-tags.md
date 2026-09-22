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
This is an agent-curated vocabulary, not a background model inference service. Existing session logs and saved corrections remain available
for that processing. No model is called on each keystroke.

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
**no model refinement is connected or claimed by this implementation**.

Rules and correction records are saved atomically together as version-2 JSON in
`session-mode-turn-rules-file` (inside `user-emacs-directory`, currently
`~/.emacs-graph/session-turn-vocabulary.json`). The store now includes labelled
passage text. Version-1 rule files remain readable. Save errors preserve both the
input and live rules; the command never invokes the conversation agent. Emacs
corrections remain separate from the Marimo rating store.
