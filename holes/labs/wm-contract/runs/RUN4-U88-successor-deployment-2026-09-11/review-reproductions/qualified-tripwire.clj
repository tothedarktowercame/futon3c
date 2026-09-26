(require '[futon2.aif.repair-obligation :as repair]
         '[futon2.aif.tripwire :as trip])
(let [root (str (java.nio.file.Files/createTempDirectory "run4-tripwire-review" (make-array java.nio.file.attribute.FileAttribute 0)))
      _ (repair/record-system-failure! root {:attempt-id "run4-a--attempt-001"
                                            :repair-class :environmental-hold
                                            :failure-stage :author-readiness
                                            :failure-kind :agent-unavailable
                                            :outcome :agent-unavailable :error "fixture"})
      obs {:phase :opportunity :transition :end :outcome :agent-unavailable
           :cohort? true :repair-root root}]
  (println {:local-id-result (trip/evaluate-wire :T3 (assoc obs :attempt-id "attempt-001"))
            :qualified-id-result (trip/evaluate-wire :T3 (assoc obs :attempt-id "run4-a--attempt-001"))}))
