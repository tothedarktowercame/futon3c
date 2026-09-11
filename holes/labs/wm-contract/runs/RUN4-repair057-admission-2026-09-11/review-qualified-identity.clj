(require '[clojure.java.io :as io]
         '[futon2.aif.c-fold-config :as digest]
         '[futon2.aif.repair-obligation :as repair]
         '[futon3c.wm.run4-historical-successor :as successor]
         '[futon3c.wm.run4-terminal-evidence-test :as fixture])
(defn write! [file value]
  (io/make-parents file) (spit file (pr-str value)))
(def request fixture/request)
(def started fixture/started)
(#'fixture/fixture
 (fn [{:keys [roots]}]
     (let [store (.toFile (java.nio.file.Files/createTempDirectory
                           "historical-successor"
                           (make-array java.nio.file.attribute.FileAttribute 0)))
           finding-file (io/file store "findings/repair-057.edn")
           obligation {:repair/id "repair-057" :repair/status :open
                       :repair/class :machine-failure :attempt-id "failed-057"}
           _ (write! finding-file obligation)
           verification-root (doto (io/file store "incoming") .mkdir)
           verification-file (io/file verification-root "verification.edn")
           verification {:schema :wm/historical-repair-verification-v1
                         :verification-id "verification-057" :repair-id "repair-057"
                         :state :awaiting-validation :repair-resolved? false
                         :actors {:author "zai-2" :reviewer "codex-10"}
                         :review {:job-id "review-job-057" :verdict :approve
                                  :execution {:executed true :tool-events 1}}
                         :qualification {:path "/qualified/output" :sha256 (apply str (repeat 64 "a"))
                                         :check-ids [:timeout-recovery :timeout-exhaustion]}
                         :finding {:path (.getCanonicalPath finding-file)
                                   :sha256 (digest/sha256 (slurp finding-file))}
                         :implementation {:first "9ab503bd" :last "3bdc381e"
                                          :source-head "8bf149c5"}}
           _ (write! verification-file verification)
           _ (repair/commit-historical-verification!
              (.getPath store) {:kind :runner-execution :id "internal-1"}
              {:verification-root (.getPath verification-root)
               :path (.getPath verification-file)
               :sha256 (digest/sha256 (slurp verification-file))})
           config {:repair-root (.getPath store) :evidence-roots roots
                   :admission-request request :started started :repair-id "repair-057"
                   :verification-id "verification-057"
                   :verification-attempt {:kind :runner-execution
                                          :id "internal-1"}}]

       (try
         (let [resolution (successor/resolve-from-durable! config)]
           (prn {:resolved? (= :resolved (:repair/status resolution))
                 :verification-execution (:verification-attempt resolution)
                 :successor-execution (:validation-attempt resolution)
                 :cohort-claim (:validation-execution resolution)
                 :cohort-authority-roots-present?
                 (boolean (some #(contains? roots %)
                                [:cohort-data-root :cohort-preregistration]))})
           (assert (not= :resolved (:repair/status resolution))
                   "Resolved without historical/successor cohort authority or distinct-execution proof"))
         (finally
           (doseq [file (reverse (file-seq store))] (io/delete-file file true)))))))
(shutdown-agents)
