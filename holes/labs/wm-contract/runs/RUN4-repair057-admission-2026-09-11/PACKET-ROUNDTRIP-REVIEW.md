# Actual frozen historical packet roundtrip

The packet-specific disposable async gate now passes: 1 test / 25 assertions.
It consumes this packet's actual template, source authority, config, series,
task pin, casting and cohort bytes. It does not substitute the old U88 packet.

Real paths exercised: deployment materializer, trusted preparation, admission,
historical candidate/execute ports, immutable repair store, full runner core,
cohort checkpoints/close, routed run-record writer, projection/binding writer,
strict historical reader, observation, controller wait and visibility.

Observed: exact controller attempt historical-verification-admission-001;
verification repair-057-revalidation-20260911-v1; cohort
:run4-repair057-admission-20260911-v1; casting codex-10/codex-12/codex-12.
Historical execution closes awaiting-validation. Requested pin remains
authenticated-not-enacted. Observation task verdict is unknown, visibility
pending, no task terminal. Duplicate series-step leaves one attempt and one close.

Fixture boundaries:
- The reusable U88 helper relocates this new template's authority into temporary
  storage and recomputes file references/digests there; its old template is not used.
- The cohort is copied byte-for-byte and activated only in disposable storage.
  The actual cohort validator/activate API accepts these bytes; no production
  status change, capacity or root is needed for this test.
- The canonical finding is copied byte-for-byte. The actual verifier produces
  a new test artifact binding that copied finding using the retained executed
  review job and actual qualification/source/ancestry checks. This is explicit
  path relocation, not a production verification/admission receipt.
- The full runner uses existing isolated substrate/options ports, fixture roster
  availability and effective-environment reads. The historical core and durable
  evidence readers are not replaced. No worker job or live JVM is invoked.
- All temporary stores are removed on exit. Nothing from the fixture can supply
  a production successor identity.

Review correction: the frozen cohort inherited Zai/U88 task prose. Its purpose,
epoch description, authority and claim now say historical admission, distinguish
requested U88 from enactment, and retain mandatory successor validation.
Regenerated hashes:
- cohort: 2c4f3004c32f5d9f978dcb8cc8aa7d2fee118933ebed41cb940e63c1147113ef
- task: 57bc2b984f7c331e139087d9ecf9cd69c7e078e7cb98abea38a50707e9a3422c
- series: 25c8a48fc30cff11e69bd402ffff4632a474e1cf8992387b474bf46ca9798dd9
- template: 397960a19a0b1909ef89e03cf7bb9607461cff76bbda930150e5acf4b284d6f9
Run-config and qualified Futon2 HEAD 72d9beba remain unchanged.

Reproduce from futon3c (explicit slow test, sibling test fixture classpath):

```sh
clojure -Sdeps '{:aliases {:packet-review {:extra-paths ["test" "dev" "../futon2/test"]}}}' -M:packet-review -e "(require 'futon3c.wm.run4-historical-packet-roundtrip-test) (let [r (clojure.test/run-tests 'futon3c.wm.run4-historical-packet-roundtrip-test)] (shutdown-agents) (System/exit (+ (:fail r) (:error r))))"
```

This closes the missing composed preparation test, not production activation.
Independent review of this new test and metadata repin remains next. The actual
historical execution receipt remains unavailable until a separately permitted
live admission. No live capacity, reset, attempt, restart or repair change.
