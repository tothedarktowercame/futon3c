# Saying “no” to a disclosed choice

When an agent reports work and names a disclosed choice, you can say that you
do not want that choice. Nothing is silently deleted or changed merely because
象 reads your words.

1. **象 records a reading.** Your reply is tied to the job behind the report.
   象 writes an `interpretation/negation` record containing its reading of your
   words. The reading is not authority and does not withdraw anything.

2. **The target is chosen without guessing from prose.** If you name a
   disclosure's `act:…` id, that id is used only when it belongs to the job you
   are answering. Otherwise, 象 selects a target only when exactly one
   disclosure still stands for that job. Zero candidates gives
   `target-unresolved`; two or more gives `target-ambiguous`.

3. **The dispatcher decides the effect.** A resolved reading is sent to the
   agent that dispatched the job. That orchestrator can withdraw the disclosed
   choice:

   ```sh
   curl -sS -X POST http://localhost:7070/api/alpha/disclosure/withdraw \
     -H 'content-type: application/json' \
     -d '{"caller":"ORCHESTRATOR","target":"act:DISCLOSURE","reason":"WHY"}'
   ```

   Or it can decline the reading and give a reason:

   ```sh
   curl -sS -X POST http://localhost:7070/api/alpha/interpretation/negation/decline \
     -H 'content-type: application/json' \
     -d '{"caller":"ORCHESTRATOR","interpretation-id":"interpretation-negation:ID","reason":"WHY"}'
   ```

4. **The audit shows what happened.** Read the job with:

   ```sh
   curl -sS 'http://localhost:7070/api/alpha/disclosure/audit?job=invoke-JOB'
   ```

   Its disclosures are marked standing or withdrawn. Declines appear in
   `declined`. A resolved negation with neither a withdrawal nor a decline
   remains visible as `negation-without-effect`.
