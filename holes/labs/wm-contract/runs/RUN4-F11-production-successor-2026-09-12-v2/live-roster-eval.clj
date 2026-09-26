(do
  (require '[futon3c.agency.registry :as reg])
  (into {}
        (for [id ["codex-20" "codex-18" "codex-19" "codex-21"]
              :let [a (reg/get-agent id)]]
          [id (select-keys a [:agent/status :agent/invoke-ready?
                              :agent/current-job :agent/active-job-id
                              :agent/invoke-job-id :agent/session-id])])))
