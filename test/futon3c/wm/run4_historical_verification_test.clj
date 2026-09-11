(ns futon3c.wm.run4-historical-verification-test
  (:require [clojure.test :refer [deftest is]] [clojure.java.io :as io]
            [clojure.string :as str] [clojure.java.shell :as shell]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-historical-qualification :as qualification]
            [futon3c.wm.run4-historical-verification :as v]))
(defn- tmp [] (.toFile (java.nio.file.Files/createTempDirectory "hist-v" (make-array java.nio.file.attribute.FileAttribute 0))))
(defn- write! [f x] (spit f (str (pr-str x) "\n")) f)
(deftest qualification-to-reviewed-awaiting-validation
  (let [root (tmp) findings (doto (io/file root "f") .mkdir) quals (doto (io/file root "q") .mkdir)
        out (doto (io/file root "o") .mkdir)
        source (write! (io/file root "source.edn") {:source :pinned})
        finding (write! (io/file findings "057.edn")
                        {:repair/id "repair-057" :repair/status :open
                         :repair/class :machine-failure :repair/schema-version 3})
        plan-file (io/file root "plan.edn")
        plan {:schema :wm/historical-qualification-plan-v1 :verification-id "verify-1"
              :repair-id "repair-057"
              :sources [{:path (.getPath source) :sha256 (digest/sha256 (slurp source))}]
              :checks [{:id :recovery :argv ["/bin/true"] :timeout-ms 2000}
                       {:id :exhaustion :argv ["/bin/true"] :timeout-ms 2000}]}
        _ (write! plan-file plan)
        _ (qualification/produce! {:source-root (.getPath root) :output-root (.getPath quals)
                                   :manifest-path (.getPath plan-file)
                                   :manifest-sha256 (digest/sha256 (slurp plan-file))})
        q (io/file quals "verify-1.qualification.edn")
        qsha (digest/sha256 (slurp q))
        job {:job-id "review-1" :state "done"
             :agent-id "codex-10"
             :result-summary "FULL_LOOP_REVIEW: APPROVE"
             :result (str "HISTORICAL_VERIFICATION_SHA256: " qsha)
             :execution {:executed true :tool-events 1 :command-events 1}}
        opts {:finding-root (.getPath findings) :qualification-root (.getPath quals)
              :qualification-source-root (.getPath root)
              :output-root (.getPath out) :source-repo "/home/joe/code/futon2"
              :finding-path (.getPath finding) :finding-sha256 (digest/sha256 (slurp finding))
              :qualification-path (.getPath q) :qualification-sha256 qsha
              :expected-check-ids [:recovery :exhaustion]
              :first-commit "9ab503bd61be1d63e7a24731e8e8aa285a9e44da"
              :last-commit "3bdc381e76518e69f90077397fa46495da98e61c"
              :source-head (str/trim (:out (shell/sh "git" "-C" "/home/joe/code/futon2"
                                                     "rev-parse" "HEAD")))
              :verification-id "verify-1" :author "zai-2" :reviewer "codex-10"
              :review-job-id "review-1" :review-job-reader (fn [_] job)}]
    (is (= :awaiting-validation (:state (v/admit! opts))))
    (is (false? (:repair-resolved? (v/admit! opts))))
    (doseq [bad [(assoc opts :expected-check-ids [:recovery])
                 (assoc opts :review-job-reader (fn [_] (assoc job :state "running")))
                 (assoc opts :review-job-reader (fn [_] (assoc job :agent-id "zai-2")))
                 (assoc opts :reviewer "zai-2")]]
      (is (thrown? clojure.lang.ExceptionInfo (v/admit! bad))))
    (spit q "nil")
    (is (thrown? clojure.lang.ExceptionInfo (v/admit! opts)))))
