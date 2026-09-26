(ns futon3c.wm.run4-attempt-producer-contract-test
  "Contract test: consume return values obtained from the actual click! producer."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.registry :as reg]
            [futon3c.wm.run4-attempt-admission :as admission]
            [futon3c.wm.runner-service :as service]))

(def attempt-identity
  {:series-id "RUN4-2026-09-10" :trial-id :producer-contract
   :pin-sha256 (apply str (repeat 64 "c"))
   :casting {:author "codex-10" :reviewer "codex-17"
             :repair-reviewer "codex-1"}})

(defn delete-tree! [root]
  (doseq [f (reverse (file-seq root))] (io/delete-file f true)))

(defn resolver [run!]
  (fn [symbol]
    (case symbol
      futon2.aif.full-loop-runner/config identity
      futon2.aif.full-loop-runner/run-opportunity! run!
      futon3c.wm.scheduler/ensure-war-machine-agent! (fn [] nil)
      futon3c.peripheral.live-wm-selection/validated-selection identity
      nil)))

(deftest actual-click-return-shapes-are-recordable
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-producer-contract"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        release (promise)
        entered (promise)
        run! (fn [_] (deliver entered true) @release
               {:attempt-id "worker" :outcome :no-grounded-change})]
    (try
      (reset! service/!status service/initial-status)
      (binding [service/*resolve-var* (resolver run!)
                service/*click-run-binding-dir* (.getPath root)]
        (with-redefs [reg/get-agent (constantly {:agent/id "war-machine"})
                      reg/update-agent! (fn [& _] nil)
                      reg/mark-agent-idle! (fn [& _] nil)
                      reg/clear-external-invoke! (fn [& _] nil)]
          (admission/reserve! (.getPath root)
                              {:attempt-id "success-shape"
                               :identity attempt-identity})
          (let [accepted (service/click! {})]
            (is (true? (deref entered 2000 false)))
            (is (= #{:click-id :started-at} (set (keys accepted))))
            (is (= :click-recorded
                   (:state (admission/record-click! (.getPath root)
                                                    "success-shape" accepted))))
            (admission/reserve! (.getPath root)
                                {:attempt-id "busy-shape"
                                 :identity (assoc attempt-identity
                                                  :trial-id :busy-contract)})
            (let [busy (service/click! {})]
              (is (= {:rejected :already-running :click-id (:click-id accepted)}
                     busy))
              (is (= :busy-rejected
                     (:state (admission/record-click! (.getPath root)
                                                      "busy-shape" busy)))))
            (deliver release true)
            (is (= :completed (:status (service/await-click! (:click-id accepted))))))))
      (finally
        (deliver release true)
        (delete-tree! root)))))
