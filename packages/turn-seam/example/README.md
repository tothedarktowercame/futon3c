# One real record

`turn-example.json` is an actual operator turn, produced by the Emacs
implementation and passing `conformance.py` unmodified. Its identifiers are
real; nothing has been prettified.

Read it beside the schema. The fields worth looking at first:

- `source_text` against `original_text` — this turn quoted a previous message,
  so the quoted block is a `QUOTE` token in the first and present in the
  second. Two readers, two needs, one record.
- `offset_unit` — every span in the file indexes codepoints, not UTF-16 units.
  A JavaScript client must convert at its boundary.
- `sentences[].text` — equal to `source_text[start:end]` in every case. That
  is the test your own implementation has to pass.
