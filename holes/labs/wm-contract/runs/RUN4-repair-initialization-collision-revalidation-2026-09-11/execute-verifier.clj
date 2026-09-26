(require '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[futon2.aif.c-fold-config :as digest]
         '[futon3c.wm.run4-historical-verification :as verifier])

(def base
  "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-initialization-collision-revalidation-2026-09-11")
(def input-path (str base "/verifier-input.reviewed.disabled.edn"))
(def input-sha "c91e023c5ef3f497728b1ae2775ff7396f8c3be18ad67872fa46ae57205ffe57")
(def job-sha "4094a32a2a2d455db0d77d0c29c1c7593eb634d1ba4fa2f0f5d9d3f3139111c9")

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
