# Creator integration review

Reviewed 4138319f/7e99b6da/b884120f. All five source/dependency pins match current bytes. Retained focused 6 tests/21 assertions, kondo (zero warnings/errors, one info), parens and deliberate failure inspected without passing-suite rerun.

NOT accepted as drain accounting: independent real temporary-file persistence control injects failure at :renamed. The ledger reports :committed? true / :durability :unconfirmed and retains the job on disk and in memory. HTTP finally releases the creation ticket with nil acceptance; authenticated snapshot then reports :drained? true, all counts zero. Final explicit-path control exited 0 and reproduced this defect. Two missing-classpath attempts and one incorrect raw-state/count assertion are retained separately; none is a product failure claim.

Repair must preserve committed or uncertain work and prevent false drain, including failure after persistence before normal return. Do not swallow errors or roll back committed evidence. Execution/delivery callbacks and duplicate lifecycle handling remain to be implemented. Inactive default and closed-intake pre-write refusal are source machinery only, not deployment readiness. No live changes.
