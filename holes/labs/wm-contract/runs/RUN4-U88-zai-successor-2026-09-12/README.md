# U88 production successor — zai casting (task B)

Third commissioned U88 execution, and the distinct production successor the
ea1 admission requires. Casting: author zai-2, reviewer zai-1,
repair-reviewer zai-1. Fresh target-1 cohort
:run4-u88-zai-successor-20260912-v1 (activated at
/home/joe/run4/U88-zai-successor-20260912). No historical-action port: this
is an ordinary production click that dispatches a real author turn continuing
from commit ee5ab864 (contextual_preferences.clj + tests, preserved on
futon2 master).

Deployment installed in the serving JVM 2026-09-12 via reconfigure-handler!
with :historical-successor linking:
- repair-ea1-6a0de60a…--attempt-001-artifact-binding-mismatch
- verification repair-ea1-artifact-binding-revalidation-20260912-v1
- verification attempt ea1-4dce4ed5…--attempt-001
- verification cohort :run4-ea1-artifact-binding-admission-20260912-v2
- successor trial u88-zai-successor-20260912-attempt-001

A grounded close on this series resolves the repair via
linked-successor!/resolve-from-durable! (and un-holds the repair
discharge). A failure close consumes the attempt like any cohort.

## Live trigger (cast actors zai-2 AND zai-1 must be idle)

```sh
TOKEN=$(cat /home/joe/.config/futon3c/run4/u88.bearer)
curl -s --max-time 280 -X POST http://127.0.0.1:7070/api/alpha/wm/run4/series/step \
  -H 'Content-Type: application/json' -H "Authorization: Bearer $TOKEN" \
  -d '{"run4-series-ref":"holes/labs/wm-contract/runs/RUN4-U88-zai-successor-2026-09-12/series-pin.edn"}'
```

Expect trial-started; the click then runs selection → author dispatch to
zai-2 (real mission work, may take 10-30 min) → review → build →
adjudication. Poll GET /api/alpha/wm/click every 60s until running? false,
then POST the step once more for the terminal result. Do not retry a failed
click (attempt-each-once).
