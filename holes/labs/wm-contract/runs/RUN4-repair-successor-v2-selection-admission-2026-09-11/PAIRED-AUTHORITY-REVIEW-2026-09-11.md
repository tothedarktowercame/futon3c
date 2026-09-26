# Independent paired authority review

Reviewed 544214c5, 4da3b83c and 8ee0a89b with Futon2 810be2a9. Terminal records require both versioned fields or genuine absence. Successor resolution compares recorded provenance to canonical closed-execution, including root authority. Foreign-root identity cannot establish distinctness or authorize resolution.

Independent terminal namespace: 11 tests / 62 assertions, zero failures/errors. Corrected paired namespace: 2 tests / 42 assertions, zero failures/errors. Both legacy-history/new-successor and fresh-history/fresh-successor paths ran. Historical core, stores, cohort, async publishers and strict readers are real; successor task/environment ports remain fixtures. These tests do not establish live worker success.

The first paired invocation was stopped in its isolated JVM (PID 310764) after a jstack showed tripwire serialization into the canonical default path. Calling another test namespace's helper does not apply that namespace's fixture. This test now explicitly installs hermetic/with-hermetic-stores. The successful rerun includes fixture assertions that canonical repair/trip file populations were unchanged. No evaluator invariant was disabled.

Three recent canonical trip files were observed during investigation: trip-20f7bff3-18c7-4fbf-b493-30c972fb575f.edn, trip-74b8aa19-86c2-4e75-9fe3-845b63d86a2d.edn, trip-58b7381b-6ac1-4d4d-b00b-af5cf6998c80.edn. They remain untouched. Their time proximity alone does not establish exclusive attribution; the first run is not admitted as a hermetic gate. Retained logs and stack capture document the incident.

Terminal-reader SHA remained a28a43c4640594eadc836be587af34ece228985e94834013adf0620be584788c before/after. Successor-reader SHA remained 71104d450b5996eb3a2d3b62023d484c0321d9d38e97175bac5cd5514e120e92. Corrected paired test SHA: 2ba689394838e7b9efb12032963a6277dbc8f9ae6c91ce3ec9e33f81574d629f.

Command: clojure -Sdeps '{:aliases {:futon2-test-support {:extra-paths ["../futon2/test"]}}}' -M:test:test-all:futon2-test-support -i :slow -n futon3c.wm.run4-real-paired-test

clj-kondo: zero errors/warnings. check-parens: OK. This review does not accept installation of stale packet 6b464ad5. Explicit current runtime pins, packet-specific before/after audits and its actual lifecycle gate remain required. Verifier 99b0fed1 and historical receipts are unchanged; the held queue was not resumed and no production capacity was allocated.
