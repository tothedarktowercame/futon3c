(ns futon3c.test-registry-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.test-registry :as registry])
  (:import [java.nio.file Files] [java.nio.file.attribute FileAttribute]))

(def summary "Ran 3 tests containing 7 assertions.\n0 failures, 0 errors.\n")
(def opts {:repo-root "/fixture" :code-paths ["src"] :test-paths ["test"]
           :command ["clojure" "-M:test" "-n" "demo-test"] :author "author"})
(def code {:code-sha "code" :test-sha "tests" :git-head "head"
           :code-files {"src/demo.clj" "blob"} :test-files {"test/demo_test.clj" "test-blob"}})

(defn fixture [f]
  (let [dir (.toFile (Files/createTempDirectory "test-registry-" (make-array FileAttribute 0)))
        backend (atom {:entries {} :order []}) current (atom code) env (atom {:sha256 "env"}) calls (atom [])]
    (try
      (with-redefs [registry/capture-code (fn [_] @current)
                    registry/fingerprint (fn [_] @env)
                    registry/run-process! (fn [_ command log]
                                            (swap! calls conj command)
                                            (let [text (if (some #{"-v"} command)
                                                         "Ran 1 tests containing 1 assertions.\n0 failures, 0 errors.\n"
                                                         summary)]
                                              (spit log text)
                                              (registry/parse-results 0 text 40)))]
        (f {:backend backend :current current :env env :calls calls
            :options (assoc opts :artifact-dir (str dir))}))
      (finally (doseq [file (reverse (file-seq dir))] (io/delete-file file))))))

(defn check-options [run] {:entry-id (:evidence/id run) :repo-root "/fixture" :changed-paths ["src/demo.clj"]})

(deftest author-review-roundtrip-uses-one-sample
  (fixture
   (fn [{:keys [backend calls options]}]
     (let [run (registry/register-run! backend options)
           check (registry/check-record! backend (check-options run))
           review (registry/review! backend (merge (check-options run)
                                                   {:reviewer "reviewer" :lane :routine :first-run? false
                                                    :spot-test "demo-test/catches-regression"
                                                    :adequacy "Pins deterministic rendering, including missing fields."
                                                    :artifact-dir (:artifact-dir options)}))
           chain (registry/read-chain! backend (:evidence/id review))]
       (is (:warrant? check))
       (is (= [:intent :run :intent :run :review] (mapv #(get-in % [:payload :kind]) chain)))
       (is (= :spot-check (get-in review [:payload :policy :mode])))
       (is (= 2 (count @calls)))
       (is (= ["-v" "demo-test/catches-regression"] (take-last 2 (last @calls))))
       (is (get-in review [:payload :warrant?]))))))

(deftest warrant-failures-are-typed-and-do-not-execute
  (doseq [mode [:stale-code :stale-tests :env :diff :log :missing :tamper :truncated-chain]]
    (fixture
     (fn [{:keys [backend current env calls options]}]
       (let [run (registry/register-run! backend options)
             id (:evidence/id run) check (check-options run)
             expected (case mode
                        :stale-code (do (swap! current assoc :code-sha "new") :stale-sha)
                        :stale-tests (do (swap! current assoc :test-sha "new") :stale-sha)
                        :env (do (reset! env {:sha256 "other"}) :environment-mismatch)
                        :diff :diff-outside-tested-scope
                        :log (do (spit (get-in run [:payload :log-artifact :path]) "tampered") :log-mismatch)
                        :missing :missing-entry
                        :tamper (do (swap! backend assoc-in [:entries id :evidence/body :payload-edn] "{}") :record-digest-mismatch)
                        :truncated-chain (do (swap! backend update :entries dissoc (get-in run [:payload :previous :evidence/id]))
                                             :chain-link-mismatch))
             check (cond-> check (= mode :diff) (assoc :changed-paths ["uncovered.clj"])
                           (= mode :missing) (assoc :entry-id "absent"))
             result (registry/check-record! backend check)]
         (is (= expected (:reason result)) (pr-str [mode result]))
         (is (= :test-registry/refusal (:record/type result)))
         (is (= 1 (count @calls))))))))

(deftest results-envelope-does-not-invent-absent-counts
  (is (= (registry/none :unparsed) (:tests (registry/parse-results 0 "process exited" 5))))
  (is (false? (registry/successful? (registry/parse-results 0 "process exited" 5)))))

(deftest declared-policy-preserves-required-full-runs
  (is (= {:mode :spot-check :count 1} (registry/execution-policy {:warrant? true} :routine false)))
  (doseq [[check lane first?] [[{:warrant? false} :routine false]
                             [{:warrant? true} :routine true]
                             [{:warrant? true} :invariant false]
                             [{:warrant? true} :pre-push false]]]
    (is (= :full-scope (:mode (registry/execution-policy check lane first?))))))

(deftest pre-push-executes-full-declared-namespace
  (fixture
   (fn [{:keys [backend calls options]}]
     (let [run (registry/register-run! backend options)
           r (registry/review! backend (merge (check-options run)
                                              {:reviewer "reviewer" :lane :pre-push :first-run? false
                                               :adequacy "Pre-push requires all declared namespace tests."
                                               :artifact-dir (:artifact-dir options)}))]
       (is (= :full-scope (get-in r [:payload :policy :mode])))
       (is (= (:command options) (last @calls)))))))

(deftest author-cannot-sign-their-own-review
  (fixture
   (fn [{:keys [backend options]}]
     (let [run (registry/register-run! backend options)]
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"independent-review-required"
                            (registry/review! backend (merge (check-options run)
                                                           {:reviewer "author" :lane :routine :first-run? false
                                                            :adequacy "self-review"}))))))))

(deftest namespace-boundary-rejects-accidental-full-suite
  (doseq [command [["clojure" "-M:test"] ["clojure" "-M:test" "-n"]
                   ["clojure" "-M:test" "-n" "one" "-n" "two"]
                   ["clojure" "-M:test" "-r" ".*"]]]
    (is (thrown? clojure.lang.ExceptionInfo (registry/validate-command! command))))
  (is (nil? (registry/validate-command! (:command opts)))))

(deftest racing-inputs-leave-results-but-no-warrant
  (fixture
   (fn [{:keys [backend current options]}]
     (let [execute registry/run-process!
           r (with-redefs [registry/run-process! (fn [& args]
                                                   (let [result (apply execute args)]
                                                     (swap! current assoc :code-sha "edited") result))]
               (registry/register-run! backend options))]
       (is (false? (get-in r [:payload :warrant?])))
       (is (= :inputs-changed-during-run (get-in r [:payload :postcheck :reason])))
       (is (= 3 (get-in r [:payload :results :tests])))))))

(deftest unknown-or-failed-results-have-no-warrant
  (doseq [text ["no envelope" "Ran 1 tests containing 1 assertions.\n1 failures, 0 errors.\n"]]
    (fixture
     (fn [{:keys [backend options]}]
       (let [r (with-redefs [registry/run-process! (fn [_ _ log]
                                                   (spit log text) (registry/parse-results 0 text 3))]
                 (registry/register-run! backend options))]
         (is (false? (get-in r [:payload :warrant?]))))))))

(deftest forged-results-and-intent-bindings-have-no-warrant
  (doseq [mode [:results :intent]]
    (fixture
     (fn [{:keys [backend options]}]
       (let [run (registry/register-run! backend options)
             payload (case mode
                       :results (assoc-in (:payload run) [:results :assertions] 999)
                       :intent (assoc (:payload run) :author "other-author"))
             forged (registry/append-record! backend payload (get-in run [:payload :previous :evidence/id]))
             check (registry/check-record! backend (check-options forged))]
         (is (= (if (= mode :results) :results-log-mismatch :missing-or-mismatched-run-intent)
                (:reason check))))))))

(deftest selected-test-must-actually-execute-once
  (fixture
   (fn [{:keys [backend options]}]
     (let [run (registry/register-run! backend options)
           review (with-redefs [registry/run-process!
                                (fn [_ _ log]
                                  (let [text "Ran 0 tests containing 0 assertions.\n0 failures, 0 errors.\n"]
                                    (spit log text) (registry/parse-results 0 text 3)))]
                    (registry/review! backend (merge (check-options run)
                                                     {:reviewer "reviewer" :lane :routine :first-run? false
                                                      :spot-test "demo-test/typo" :adequacy "Negative control."
                                                      :artifact-dir (:artifact-dir options)})))]
       (is (false? (get-in review [:payload :warrant?])))
       (is (= :selective-execution-failed (get-in review [:payload :outcome])))))))

(deftest directory-budget-is-a-refusal-not-an-exclusion
  (let [dir (.toFile (Files/createTempDirectory "registry-budget-" (make-array FileAttribute 0)))
        path (io/file dir "dependency.clj")]
    (try
      (spit path "actual source")
      (with-redefs [registry/max-dependency-directory-bytes 1]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"dependency-directory-too-large"
                             (registry/directory-sha dir))))
      (is (string? (registry/directory-sha dir)))
      (finally (io/delete-file path) (io/delete-file dir)))))

(deftest canonical-record-hash-is-order-independent
  (is (= (registry/sha {:a [1 {:b 2}] :c 3}) (registry/sha (array-map :c 3 :a [1 {:b 2}])))))
