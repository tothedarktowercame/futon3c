(require '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[futon2.aif.c-fold-config :as digest]
         '[futon2.aif.full-loop-runner :as runner])
(def base "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-initialization-collision-revalidation-2026-09-11")
(defn one [s]
  (with-open [r (java.io.PushbackReader. (java.io.StringReader. s))]
    (let [v (edn/read r)] (assert (= ::end (edn/read {:eof ::end} r))) v)))
(let [plan-text (slurp (str base "/qualification-plan.disabled.edn"))
      plan (one plan-text)
      candidates (one (slurp (str base "/IMPLEMENTATION-CANDIDATES.edn")))
      qualification-path (str base "/offline-evidence/" (:verification-id plan) ".qualification.edn")
      qualification-text (slurp qualification-path)
      qualification (one qualification-text)
      job-path (str base "/independent-review-job.json")
      job-text (slurp job-path)
      response (json/parse-string job-text true)
      job (:job response)
      finding (first (:sources plan))
      review (runner/independent-review-evidence [(:path finding) qualification-path] job)
      receipt-sha "02821ef4033146884d9ea4993d9bfd5c3a26170762aad0fcfd1d2e8c4babbbbe"]
  (assert (= "4094a32a2a2d455db0d77d0c29c1c7593eb634d1ba4fa2f0f5d9d3f3139111c9" (digest/sha256 job-text)))
  (assert (true? (:ok response)))
  (assert (= "invoke-1789137433179-20168-eb5ae2db" (:job-id job) (:job-id review)))
  (assert (= "codex-12" (:agent-id job)))
  (assert (= "done" (:state job)))
  (assert (true? (:valid? review)))
  (assert (= :approve (:verdict review)))
  (assert (= [receipt-sha] (mapv second (re-seq #"(?m)^HISTORICAL_VERIFICATION_SHA256: ([0-9a-f]{64})$" (:result job)))))
  (assert (= receipt-sha (digest/sha256 qualification-text)))
  (assert (= "c598fa60498610eca03e2bea29ab968f197ed28534c0afca7ffae135e8270b7b" (digest/sha256 plan-text) (get-in qualification [:manifest :sha256])))
  (assert (= (:repair-id plan) (:repair-id qualification) (:repair-id candidates)))
  (doseq [{:keys [path sha256]} (:sources plan)] (assert (= sha256 (digest/sha256 (slurp path))) path))
  (let [input {:schema :wm/historical-verifier-preparation-v1 :enabled? false
               :options {:finding-root (.getParent (io/file (:path finding)))
                         :finding-path (:path finding) :finding-sha256 (:sha256 finding)
                         :qualification-root (str base "/offline-evidence")
                         :qualification-source-root "/home/joe"
                         :qualification-path qualification-path :qualification-sha256 receipt-sha
                         :output-root (str base "/offline-verification")
                         :source-repo "/home/joe/code/futon2" :source-head (:source-head candidates)
                         :first-commit (:first-commit candidates) :last-commit (:last-commit candidates)
                         :verification-id (:verification-id plan)
                         :expected-check-ids (mapv :id (:checks plan))
                         :author "codex-17" :reviewer "codex-12" :review-job-id (:job-id job)}
               :review-source {:path job-path :sha256 (digest/sha256 job-text)}
               :review-admission (select-keys review [:job-id :state :verdict :execution :execution-source :valid?])
               :verification-output :not-performed :repair-store-admission :not-performed
               :production-successor :not-performed}
        target (.toPath (io/file base "verifier-input.reviewed.disabled.edn"))
        bytes (.getBytes (str (pr-str input) "\n") "UTF-8")]
    (java.nio.file.Files/write target bytes
      (into-array java.nio.file.OpenOption [java.nio.file.StandardOpenOption/CREATE_NEW java.nio.file.StandardOpenOption/WRITE]))
    (prn {:input-sha256 (digest/sha256 (String. bytes "UTF-8"))
          :review-admission (:review-admission input)
          :repair-store-admission :not-performed})))
(shutdown-agents)
