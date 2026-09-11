(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[futon2.aif.c-fold-config :as digest]
         '[futon3c.wm.run4-historical-action :as action])

(def base
  "holes/labs/wm-contract/runs/RUN4-repair-successor-v2-selection-revalidation-2026-09-11/")

(defn one [path]
  (with-open [r (java.io.PushbackReader. (io/reader path))]
    (let [value (edn/read {:eof ::empty} r)]
      (assert (not= ::empty value))
      (assert (= ::end (edn/read {:eof ::end} r)))
      value)))

(let [admission (one (str base "admission-input.disabled.edn"))
      action-config (one (str base "historical-action.proposed.disabled.edn"))
      verification (one (get-in admission [:verification :path]))
      candidate ((:historical-verification-candidate-fn
                  (action/runner-ports (:authority action-config)))
                 {:repair/id (:repair-id admission)})]
  (assert (and (false? (:enabled? admission))
               (false? (:enabled? action-config))))
  (assert (= (:repair-id admission) (:repair-id action-config)
             (:repair-id verification) (:repair/id candidate)))
  (assert (= (get-in admission [:verification :id])
             (:verification-id action-config)
             (:verification-id verification)
             (:verification-id candidate)))
  (assert (= (get-in admission [:verification :sha256])
             (get-in action-config [:authority :verification-sha256])
             (digest/sha256 (slurp (get-in admission [:verification :path])))))
  (assert (= (:actors verification)
             (select-keys (:casting action-config) [:author :reviewer])))
  (assert (= {:author "codex-10" :reviewer "codex-12"
              :repair-reviewer "codex-12"}
             (:casting action-config)))
  (assert (= :awaiting-validation (:state verification)
             (:repair/status candidate)))
  (assert (false? (:repair-resolved? verification)))
  (assert (nil? (:verification-attempt candidate)))
  (assert (= {:status :not-performed :identity nil}
             (:execution admission) (:execution action-config)))
  (assert (= :not-allocated (:capacity admission)))
  (println "PASS: exact artifact reader, actor-correct disabled inputs, no execution or allocated capacity"))
