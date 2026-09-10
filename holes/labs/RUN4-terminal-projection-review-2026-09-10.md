# RUN4 terminal projection independent review

Codex-17, 2026-09-10. Accepted the bounded evidence-recording slice a65cc03d
with correction 991f1993769a54bc9869567deab1cfbc7b59faa6.

Independent execution: terminal-projection namespace 8 tests / 26 assertions;
runner-service namespace 13 tests / 103 assertions; zero failures or errors.
The original /tmp/terminal-source-race-repro.clj now reports projection returned,
validated run run-1, and source-digest-matches-validated-file true. Inspection
confirms parsing and hashing share the captured source text, and binding
publication compares its separately captured source digest against that snapshot.
Present malformed pin values refuse; absence alone retains legacy opt-out.

The producer-boundary test exercises the actual run-opportunity! wrapper and
run-record writer with a stubbed core. It does not establish live end-to-end
execution. This acceptance concerns recording and identity binding, not an
outcome classifier, scheduler, complete RUN4 conformance, deployment, or launch.
An orphan projection cannot authorize advancement; a subsequent consumer must
verify the binding, source digest and complete attempt/series identity joins.
Service failures without returned evidence remain unknown.

Next required slice is a strict terminal-evidence reader with explicit outcome
classification rules. Missing evidence, worker completion, idle status and click
acceptance must never be substituted for a qualifying terminal outcome.
