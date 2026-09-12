(do
  (require '[futon3c.transport.http :as http]
           '[futon3c.wm.run4-trusted-entry :as trusted])
  (let [cfg @(var-get (ns-resolve 'futon3c.transport.http '!handler-config))
        token (get-in cfg [:run4 :bearer-token])]
    (dissoc
     (trusted/prepare cfg {"authorization" (str "Bearer " token)}
                      {:run4-pin-ref
                       "holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12/task-pin.edn"
                       :run4-attempt-id "f11-production-successor-20260912-attempt-001"})
     :opts :admission-request)))
