(require '[clojure.edn :as edn] '[clojure.data.json :as json]
         '[futon2.aif.c-fold-config :as digest]
         '[futon2.aif.full-loop-runner :as runner]
         '[futon3c.wm.run4-historical-action :as action]
         '[futon3c.wm.run4-historical-verification :as verifier])
(defn one [text]
  (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
    (let [v (edn/read r)] (assert (= ::eof (edn/read {:eof ::eof} r))) v)))
(let [base "holes/labs/wm-contract/runs/RUN4-U88-successor-deployment-2026-09-11/"
      config (one (slurp (str base "historical-action.reviewed.disabled.edn")))
      authority (:authority config)
      text (slurp (:verification-path authority)) v (one text)
      q (:qualification v) finding (:finding v) impl (:implementation v)
      job (:job (json/read-str
                 (slurp (str "http://localhost:7070/api/alpha/invoke/jobs/" (get-in v [:review :job-id])))
                 :key-fn keyword))
      output (str (java.nio.file.Files/createTempDirectory "run4-independent-verifier-"
                   (make-array java.nio.file.attribute.FileAttribute 0)))
      replay (verifier/admit!
              {:finding-root (str (:repair-root authority) "/findings")
               :qualification-root (.getCanonicalPath (java.io.File. base))
               :qualification-source-root "/home/joe/code/futon2"
               :output-root output :source-repo "/home/joe/code/futon2"
               :finding-path (:path finding) :finding-sha256 (:sha256 finding)
               :qualification-path (:path q) :qualification-sha256 (:sha256 q)
               :expected-check-ids (:check-ids q)
               :first-commit (:first impl) :last-commit (:last impl) :source-head (:source-head impl)
               :verification-id (:verification-id v)
               :author (get-in v [:actors :author]) :reviewer (get-in v [:actors :reviewer])
               :review-job-id (get-in v [:review :job-id])
               :review-job-reader (fn [id] (assert (= id (:job-id job))) job)})
      ports (action/runner-ports authority)
      obligation (one (slurp (:path finding)))
      candidate ((:historical-verification-candidate-fn ports) obligation)
      entry (runner/historical-revalidation-entry obligation candidate (:casting config))]
  (assert (= "6ca6f397b1f7531c91a0f4b9ab6cbc3d2712eb4b347c3c897b91a62c72958829"
             (:verification-sha256 authority) (digest/sha256 text)))
  (assert (= v replay))
  (assert (= text (slurp (str output "/" (:verification-id v) ".verification.edn"))))
  (assert (= :revalidate-historical-repair (get-in entry [:action :type])))
  (assert (nil? (runner/historical-revalidation-entry obligation candidate
                   (dissoc (:casting config) :repair-reviewer))))
  (assert (nil? (runner/historical-revalidation-entry obligation candidate
                   (assoc (:casting config) :repair-reviewer "codex-10"))))
  (assert (false? (:enabled? config)))
  (assert (= :not-performed (get-in config [:repair-admission :status])))
  (assert (nil? (get-in config [:execution :identity])))
  (prn {:verifier-replay :byte-identical :output-root output
        :actual-selector :historical-revalidation :casting-controls :passed
        :repair-store-execution :not-performed :enabled? false}))
(shutdown-agents)
