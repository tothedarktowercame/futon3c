# Worker lifetime review

Reviewed 8c06e0ed/289bb323/122b18c9 and retained final6a2d4637 gates: five historical source pins match; current HTTP differs only by row26 run-id forwarding51269db9. HTTP11tests72/controller11tests38, kondo/parens and prior sensitivity receipts inspected; no passing-suite rerun. Initial current-pin assertion refused correctly on concurrent row26 change; an attempted missing control file exited1 before control construction, not a product finding.

Worker versus terminal/delivery separation, duplicate creation accounting and ordered callbacks accepted narrowly by source review. Changes required before serving: executed lead-reused-worker.clj exits0 and proves actual run-invoke-job! of an already-running ID changes original ledger to failed and calls unregister-job-worker!. Execution-reuse exception is caught by the ordinary failure path; finally runs non-owner cleanup. The same pattern exists in the direct wrapper. Refusal must not finalize/unregister/mark-idle another worker. Terminal skip cleanup also needs ownership review. Retain failed-publication accounting.

Next packet is this concrete wrapper ownership repair only, within Row19 timebox; startup/recovery/fence remain unproved. No deployment. Scoreboard zero rows closed, zero new claims admitted.
