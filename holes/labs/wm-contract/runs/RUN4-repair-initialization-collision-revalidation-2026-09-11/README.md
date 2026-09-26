# Initialization collision qualification target

This disabled packet is specific to
`repair-initialization-b076e0f8-dbc2-4368-80a0-073d243951a0-initialization-failed`.
The retained finding records `FileAlreadyExistsException` at the unqualified
auxiliary path `findings/repair-attempt-001-agent-unavailable.edn`.

Current Futon2 already contains the bounded repair. An explicit execution cohort
keeps its local `attempt-001`, while durable repair and Morning Brief identities
use `<cohort-id>--attempt-001`. T3 receives and checks that same external identity.
The runner regression creates two isolated cohort roots whose local attempts are
both `attempt-001` and proves their durable identities differ. The tripwire
regression uses the real repair writer and proves T3 finds the qualified record.

The plan pins the exact live finding bytes and current runner, tripwire, and
producer-test bytes at Futon2 HEAD `16c215c5bf615741715dc92bce47817fbd96e9e1`.
Its checks intentionally run one namespace per process. Qualification production,
independent review, admission, repair transition, capacity, and a production
successor have not occurred.

Next gate: execute the disabled plan with the canonical historical qualification
producer into a new immutable output directory, then independently review the
receipt before preparing verifier input.
