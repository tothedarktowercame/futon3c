(require '[futon3c.transport.invoke-ingress-integration-test :as fixture]
         '[futon3c.transport.http :as http]
         '[futon3c.agency.invoke-ingress-controller :as ingress]
         '[clojure.edn :as edn])
(let [c (#'fixture/durable-controller)
      dir (java.nio.file.Files/createTempDirectory "lead-http-commit-" (make-array java.nio.file.attribute.FileAttribute 0))
      path (str (.resolve dir "ledger.edn"))
      persist (var-get #'http/persist-invoke-jobs-ledger!)]
 (try
  (#'fixture/with-http-fixture c
   (fn [{:keys [ledger]}]
    (with-redefs-fn {#'http/persist-invoke-jobs-ledger! persist
                    #'http/invoke-jobs-store-path (constantly path)}
     (fn []
      (binding [http/*invoke-jobs-persist-stage-hook*
                (fn [stage _] (when (= :renamed stage) (throw (ex-info "post-rename-control" {}))))]
       (let [error (try (#'http/create-invoke-job! (#'fixture/invoke-request "lead-committed")) nil
                        (catch clojure.lang.ExceptionInfo e (ex-data e)))
             disk (edn/read-string (slurp path))
             _ (ingress/close-intake! c)
             snap (#'fixture/snapshot c)
             result {:error error :disk-has-job? (contains? (:jobs disk) "lead-committed")
                     :memory-has-job? (contains? (:jobs @ledger) "lead-committed")
                     :snapshot snap}]
        (prn result)
        (assert (:disk-has-job? result))
        (assert (= 0 (:accepted-queued snap)))
        (assert (:drained? snap))))))))
  (finally (ingress/release-controller! c))))
(shutdown-agents)
