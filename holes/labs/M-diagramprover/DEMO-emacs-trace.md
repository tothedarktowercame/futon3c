# Local Emacs trace demo

This demonstration exercises presentation over a frozen canonical artifact.
Matrix and the browser widget are deliberately outside its boundary.

In Emacs, with `futon3c/emacs` on `load-path`:

```elisp
(require 'proof-dialogue-demo)
(proof-dialogue-demo)
```

The `*proof dialogue demo*` buffer shows the trace head, move and receipt
counts, every typed move, its parent, and whether it has a delivery receipt.
Press `g` to reread the local JSONL file. With a prefix argument,
`M-x proof-dialogue-demo` prompts for another canonical trace.

The pilot fixture's expected header is:

```text
11 moves · 11 receipts · head f27bd65367f5
```

The viewer is intentionally not a hash-chain verifier. Before presenting an
artifact, verify it independently:

```sh
python3 scripts/proof_dialogue_trace.py \
  --trace holes/labs/M-diagramprover/proof-dialogues/t94a01-pilot/trace.jsonl \
  verify
```

This separation gives later performance work two measurable stages: canonical
verification and Emacs parsing/rendering.
