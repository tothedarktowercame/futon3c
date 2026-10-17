minus40 memories injected before round 1 for this task:

--- memory invoke-ledger-test-clobber (cc:-home-joe) ---
The live Agency's invoke ledger uses the default path `/tmp/futon3c-invoke-jobs.edn`
(FUTON3C_INVOKE_JOBS_FILE unset), and so do test JVMs. On 2026-10-12 05:02 one of
my suite runs left a 102-byte empty ledger in place of the live 99 MB snapshot.
Under the journal writer that persisted: a restart would have loaded an empty
ledger and refused it. I repaired it from the live JVM (forced snapshot), and
futon3c f7f8dff6 makes both journal writers re-snapshot when the snapshot's
[file-key size mtime] changes underneath them.

**Why:** with whole-file rewrites the clobber healed itself in 1.5 s; with
append-only journals it doesn't, so a stray writer now matters.

**How to apply:** since futon3c cd33eebd the :test aliases redirect these defaults themselves (-Dfuton3c.test-run=true); for other runners, still run tests with
`FUTON3C_INVOKE_JOBS_FILE=$(mktemp -d)/i.edn FUTON3C_DURABLE_QUEUE_PATH=$(mktemp -d)/q.edn`
(pass them with `systemd-run -E`). The lasting fix, moving the live ledger out of
/tmp via the unit's environment, is Joe's call at a restart.
Related: [[proc-io-includes-reaped-children]], [[slow-tagged-tests-hide]].
--- end of injected memories ---

Write the exact command you would run, from /home/joe/code/futon3c on this box, to run the test namespace futon3c.agency.invoke-activity-test, given that the live Agency server is running on this same machine. Do not run it. Reply with the command in one code block and one sentence on anything about it that matters on this box.
