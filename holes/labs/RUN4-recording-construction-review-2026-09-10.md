# RUN4 recording construction review — 2026-09-10

Independent tests at ec5008eb: realized recording 2 tests / 7 assertions,
zero failures/errors. The bundle reader exposes its already checked projection;
the adapter uses the existing envelope validator and keeps prior-step/channel
observations explicitly unknown. Those boundaries are correct.

Not accepted as a final semantic recording: running from-terminal-bundle on the
committed test bundle produces :policy "zai-2", :tick :t1, :decision/ref :t1,
and an observed execution policy with domain :actor-id. Casting author is a
worker identity, not evidence of a selected policy; trial identity is not a
tick identifier. The schema validator accepts these values but does not prove
their meaning. Preserve actors and series/trial identities as those identities;
derive policy/action/decision/tick from the actual joined producer fields, with
explicit supported absence/refusal where a required carrier is unavailable.
A changed author must not change the recorded selected policy. Test this with
a bundle containing distinct worker, policy/action, decision, tick and trial IDs.

The report still sets recording-completeness from successful construction/count
of in-memory envelopes. This is schema-construction evidence, not a persisted
journal readback. The queued df40b60a integration must distinguish these states,
verify actual immutable recording identity/digest/readback, and retain missing
persisted evidence before reporting operator-decision-required. No live run,
record store or observer invocation occurred in this review.
