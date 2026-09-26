# Historical RUN4 visibility metadata

The producer retains schema `wm/run-visibility-v1` and adds the following
fields only after `run4-historical-projection/read-bundle!` has validated the
admission, click binding, run record, projection, repair transition, and closed
cohort attempt.

At the series and trial levels, `assigned_roles` is:

```json
{"author":"codex-10","reviewer":"codex-12","repair_reviewer":"codex-12","active_workers":[]}
```

The historical trial additionally carries:

- `historical_execution`: `kind`, completed `status`,
  `resolution_status`, exact `click_id`, `run_id`, controller and runner attempt
  IDs, cohort ID, repair ID, and verification ID.
- `requested_task`: trial ID and status `authenticated-not-enacted`.
- `actual_action`: type `revalidate-historical-repair` and exact repair ID.

For this state, `stage` is `review` and `result` remains `pending`. The ordinary
`worker` and `reviewer` display fields are omitted because casting assignments
are not active workers. No task success, task terminal, repair resolution,
acceptance, or refreshed activity is inferred.

Voxterm's current reader safely ignores these additive keys while retaining the
existing schema validation. To display the distinction, it may copy these
three trial maps and `assigned_roles` through verbatim after validating their
types; it must not reinterpret `historical_execution.status=completed` as
`result=passed`.

The retained live repair-058 composed test rereads the actual immutable bundle
for click `wm-click-53f7d985-489f-48eb-a8fc-607c9cbab779` and run
`92a46da7-c02c-4177-a535-ac9d91e933e0`. A mutation from task verdict `unknown`
to `succeeded` is rejected.

## Continuous queue prerequisite

`run4-series-controller/step!` only advances one ordinal in one already frozen
manifest, and `run4-series-service/step!` only runs after an explicit
authenticated request. The recurring WM scheduler refreshes snapshots; it does
not discover, authenticate, rank, or enqueue RUN4 series. Therefore a continuous
bounded queue still requires a server-owned frozen manifest queue plus an
explicit start/stop lifecycle that selects the next independently admitted
eligible manifest and invokes the existing single-flight series boundary one
transition at a time. No such queue authority or lifecycle exists today.
