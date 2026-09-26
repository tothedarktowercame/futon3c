(require '[clojure.edn :as edn]
         '[futon2.aif.c-fold-config :as digest]
         '[futon3c.wm.run4-historical-qualification :as qualification])

(def base
  "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-successor-v2-selection-revalidation-2026-09-11/")

(defn one [path]
  (with-open [r (java.io.PushbackReader. (java.io.FileReader. path))]
    (let [value (edn/read {:eof ::empty} r)]
      (assert (not= ::empty value))
      (assert (= ::end (edn/read {:eof ::end} r)))
      value)))

(let [plan-path (str base "qualification-plan.disabled.edn")
      receipt-path (str base "offline-evidence/repair-successor-v2-selection-revalidation-20260911-v1.qualification.edn")
      plan (qualification/validate-plan! (one plan-path))
      receipt (one receipt-path)]
  (assert (= "dff0fe4c54e37013eedddee48189d6d9d10c7f66b860ab666217c78fc92b4c3b"
             (digest/sha256 (slurp plan-path))
             (get-in receipt [:manifest :sha256])))
  (assert (= "92a51fb83e2de7ebe992bde58faf7efdb535b375053b1c8bc6e1b4d0f0300545"
             (digest/sha256 (slurp receipt-path))))
  (assert (= (:verification-id plan) (:verification-id receipt)))
  (assert (= (:repair-id plan) (:repair-id receipt)))
  (assert (= (:sources plan) (:sources receipt)))
  (assert (= (:checks plan)
             (mapv #(select-keys % [:id :argv :timeout-ms]) (:checks receipt))))
  (doseq [{:keys [path sha256]} (:sources plan)]
    (assert (= sha256 (digest/sha256 (slurp path))) path))
  (doseq [row (:checks receipt)]
    (assert (zero? (:exit row)))
    (assert (false? (:timed-out? row)))
    (doseq [stream [:stdout :stderr]]
      (assert (= (get-in row [stream :sha256])
                 (digest/sha256 (get-in row [stream :utf8]))))))
  (assert (true? (:qualification-passed? receipt)))
  (assert (= :not-performed (:independent-review receipt)))
  (assert (false? (:repair-admitted? receipt)))
  (prn {:receipt-sha256 (digest/sha256 (slurp receipt-path))
        :strict-forms true :source-pins (count (:sources plan))
        :ordered-checks (mapv :id (:checks receipt))
        :all-exits-zero true :output-digests-current true
        :independent-review :not-performed :repair-admitted? false}))
