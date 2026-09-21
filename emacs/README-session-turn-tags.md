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

While composing, matched phrases receive wavy underlines and a summary appears
below the draft, for example:

    I agree with the plan about Foo but I disagree with the plan about Bar
      [draft tags: agree + qualify + object · provisional]

The summary is a multi-label classification of explicit cues, not a judgment of
the whole turn's intent. No matches means unclassified, not “new.” Phrase matching
is case-insensitive, checks boundaries, and accepts straight or curly apostrophes.
Quotes and the scope of negation still require interpretation. Agreement and
objection have distinct underline colours; hovering shows the phrase and tag.

`M-x customize-variable RET session-mode-turn-vocabulary` edits the literal
vocabulary, initially the same as the Marimo prototype. Customize changes are
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

## Extend the vocabulary from the chat input

Enter `!c TAG PHRASE` and press Return. For example:

```text
!c approve extend
!c redirect take another approach
```

The first tags the literal word “extend” as “approve”; the second tags a
multiword phrase. Tags are extensible, not restricted to the original set.
The command is handled locally; it does not send a turn to the agent.

To classify a draft you're already writing, append the directive on its own
last line:

```text
Please extend this idea.
!c approve extend
```

Return saves the rule, removes only the command line, and refreshes the remaining
**unsent** draft. Press Return again when ready to send the draft itself.
Malformed commands remain in the input with a usage message. If saving fails,
the input and live rules stay unchanged. Duplicate tag/phrase pairs are ignored
case-insensitively. Ordinary text containing `!c` within a sentence is unaffected.

The vocabulary is shared by active Emacs chat buffers. A JSON snapshot is saved
atomically to `session-mode-turn-rules-file` (default
`session-turn-vocabulary.json` inside `user-emacs-directory`; currently
`~/.emacs-graph/session-turn-vocabulary.json` on Joe's Emacs) and loaded when tagging is enabled after
a restart. This saves vocabulary, not chat text; nothing is evaluated as Lisp.
These Emacs rules are still separate from the Marimo rating store.
