(require '[cheshire.core :as json] '[clojure.pprint :as pp]
         '[futon2.aif.c-fold-config :as digest]
         '[futon3c.wm.run4-historical-verification :as verifier])
(def base "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-ea1-artifact-binding-revalidation-2026-09-12")
(def finding "/home/joe/code/futon2/data/wm-repair-obligations/findings/repair-ea1-6a0de60a159e907dc51bb6b6b564330ec2a7f330b5bdfbf0f3dd06145fc44a12--attempt-001-artifact-binding-mismatch.edn")
(def qual (str base "/offline-evidence/repair-ea1-artifact-binding-revalidation-20260912-v1.qualification.edn"))
(def jid "invoke-1789215401163-20343-3449a99f")
(let [job (first (filter #(= jid (:job-id %))
                         (:jobs (json/parse-string
                                 (slurp "http://127.0.0.1:7070/api/alpha/invoke/jobs?limit=5") true))))
      _ (assert (some? job) "review job not found")
      _ (assert (= "done" (:state job)))
      opts {:finding-root "/home/joe/code/futon2/data/wm-repair-obligations/findings"
            :qualification-root (str base "/offline-evidence")
            :qualification-source-root "/home/joe/code"
            :output-root (str base "/offline-verification")
            :source-repo "/home/joe/code/futon2"
            :finding-path finding
            :finding-sha256 (digest/sha256 (slurp finding))
            :qualification-path qual
            :qualification-sha256 (digest/sha256 (slurp qual))
            :expected-check-ids [:commissioned-repository-resolution-regression]
            :first-commit "38de06f5806f9b1f1c850c10cd5343e7ba03d470"
            :last-commit "38de06f5806f9b1f1c850c10cd5343e7ba03d470"
            :source-head "38de06f5806f9b1f1c850c10cd5343e7ba03d470"
            :verification-id "repair-ea1-artifact-binding-revalidation-20260912-v1"
            :author "zai-5" :reviewer "zai-1"
            :review-job-id jid
            :review-job-reader (fn [requested] (assert (= jid requested)) job)}
      r (verifier/admit! opts)]
  (spit (str base "/verifier-input.edn")
        (with-out-str (pp/pprint {:options (dissoc opts :review-job-reader)
                                  :review-sha256 (digest/sha256 (:result job))})))
  (prn (select-keys r [:schema :verification-id :repair-id :state :repair-resolved? :actors])))
(shutdown-agents)
