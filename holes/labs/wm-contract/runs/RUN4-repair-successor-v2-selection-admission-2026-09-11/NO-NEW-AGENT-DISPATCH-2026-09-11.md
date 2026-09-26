# Historical admission does not invoke the recorded review actors

The existing packet-specific materialized lifecycle now supplies a dispatch port that counts and throws on any agent dispatch. The historical action still completes through real runner/store/cohort/publication/readers and the queue holds. The dispatch count is zero. This is an invocation tripwire, not a substituted evidence reader.

Focused execution: 1 test / 31 assertions, zero failures/errors. clj-kondo 0/0, check-parens OK. No runtime source changed. Test and retained output accompany this note.

Correction to the coordination decision: changing the immutable actors requires new evidence, but saving Codex invocation costs does NOT require changing those historical actors. The historical path consumes their already-executed review and explicitly records no author dispatch before closing awaiting-validation. A fresh Zai qualification campaign was therefore an unnecessary prerequisite for the cost objective. It remains cancelled.

Next implementation work can reuse the accepted verification and runtime packet for the historical admission, then use Zai for actual future worker invocations. This does not relabel existing authorship, resolve the repair, enact U88, or permit reset/retry of the consumed initialization queue. Normal live installation and exact existing packet checks still apply; no live action was taken by this test.
