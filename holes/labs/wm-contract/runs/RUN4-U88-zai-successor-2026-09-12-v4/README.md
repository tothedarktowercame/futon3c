# U88 production successor v4 — first packet on the repaired publication path

v3 (RUN4-U88-zai-successor-2026-09-12) achieved a GROUNDED change — commit
5d595dc9 (pure event derivation for contextual preferences, zai-2 author /
zai-1 reviewer, gates green) — but its publication refused with the
run-record-route defect: operator-selected RUN4 packets emit no
selection-judgment :wm/route, so no run-record was written and the terminal
projection refused :missing-run-record after the work was banked. Fixed in
futon2 92e5fbf2 (packet-run-route; 134 tests / 648 assertions green) and
reloaded into the serving JVM. v3's attempt is consumed and its series held;
v4 is the same packet with fresh ids (cohort
:run4-u88-zai-successor-20260912-v4, attempt -004) on the repaired path,
with the same historical-successor link to the ea1 admission.

## Trigger (zai-2 and zai-1 idle), then poll + re-step — same runbook as v3:
series-step POST with ref
holes/labs/wm-contract/runs/RUN4-U88-zai-successor-2026-09-12-v4/series-pin.edn
