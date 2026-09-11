(require '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[futon2.aif.c-fold-config :as digest]
         '[futon3c.wm.run4-historical-verification :as verifier])

(def base
  "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair058-revalidation-2026-09-11")
(def input-path (str base "/verifier-input.reviewed.disabled.edn"))
(def input-sha "65d6539462e32a6f58b9d43e702e4447bcfad2948725e156ca830953d2c121ca")
(def job-sha "05d605686d8064bee50bd6b852b9a2f776b714228978e6c2fff5d39e4733e5a1")

(defn one-edn [text]
  (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
    (let [v (edn/read {:eof ::empty} r)]
      (assert (not= ::empty v))
      (assert (= ::end (edn/read {:eof ::end} r)))
      v)))

(let [input-text (slurp input-path)
      _ (assert (= input-sha (digest/sha256 input-text)))
      input (one-edn input-text)
      job-path (get-in input [:review-source :path])
      job-text (slurp job-path)
      _ (assert (= job-sha (get-in input [:review-source :sha256])
                   (digest/sha256 job-text)))
      response (json/parse-string job-text true)
      job (:job response)
      expected-job (get-in input [:options :review-job-id])]
  (assert (= :wm/historical-verifier-preparation-v1 (:schema input)))
  (assert (false? (:enabled? input)))
  (assert (true? (:ok response)))
  (assert (= expected-job (:job-id job)
             (get-in input [:review-admission :job-id])))
  (assert (= "codex-12" (:agent-id job)))
  (assert (= "done" (:state job)))
  (let [record (verifier/admit!
                (assoc (:options input)
                       :review-job-reader
                       (fn [requested]
                         (assert (= expected-job requested))
                         job)))]
    (prn {:schema (:schema record)
          :verification-id (:verification-id record)
          :repair-id (:repair-id record)
          :state (:state record)
          :repair-resolved? (:repair-resolved? record)
          :actors (:actors record)
          :review-job-id (get-in record [:review :job-id])
          :check-ids (get-in record [:qualification :check-ids])})))
(shutdown-agents)
