(ns futon3c.test-registry-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.evidence.backend]
            [futon3c.evidence.store :as store]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.test-registry.ledger :as registry-ledger]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.test-registry :as registry])
  (:import [java.nio.file Files] [java.nio.file.attribute FileAttribute]))

(def summary "Ran 3 tests containing 7 assertions.\n0 failures, 0 errors.\n")
(def opts {:repo-root "/fixture" :code-paths ["src"] :test-paths ["test"]
           :command ["clojure" "-M:test" "-n" "demo-test"] :author "author"})
(def code {:code-sha "code" :test-sha "tests" :git-head "head"
           :code-files {"src/demo.clj" "blob"} :test-files {"test/demo_test.clj" "test-blob"}})

(defn fixture [f]
  (let [dir (.toFile (Files/createTempDirectory "test-registry-" (make-array FileAttribute 0)))
        backend (atom {:entries {} :order []}) current (atom code) env (atom {:sha256 "env"})
        closure (atom [{:ns "demo" :path "src/demo.clj" :sha256 "blob"}
                       {:ns "demo-test" :path "test/demo_test.clj" :sha256 "test-blob"}])
        calls (atom [])]
    (try
      (with-redefs [registry/capture-code (fn [_] @current)
                    registry/fingerprint (fn [_] @env)
                    registry/compute-closure (fn [_ _] @closure)
                    registry/current-closure-shas (fn [_ _] (registry/closure-shas @closure))
                    registry/run-process! (fn [_ command log]
                                            (swap! calls conj command)
                                            (let [text (if (some #{"-v"} command)
                                                         "Ran 1 tests containing 1 assertions.\n0 failures, 0 errors.\n"
                                                         summary)]
                                              (spit log text)
                                              (registry/parse-results 0 text 40)))]
        (f {:backend backend :current current :env env :closure closure :calls calls
            :options (assoc opts :artifact-dir (str dir)
                            :ledger-root (str (io/file dir "ledger")))}))
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
  (doseq [mode [:stale-code :stale-tests :env :log :missing :tamper :truncated-chain]]
    (fixture
     (fn [{:keys [backend current env calls options]}]
       (let [run (registry/register-run! backend options)
             id (:evidence/id run) check (check-options run)
             expected (case mode
                        :stale-code (do (swap! current assoc :code-sha "new") :stale-sha)
                        :stale-tests (do (swap! current assoc :test-sha "new") :stale-sha)
                        :env (do (reset! env {:sha256 "other"}) :environment-mismatch)
                        ;; Tampering with the copy at the recorded path is not
                        ;; enough: the ledger holds the object under its own
                        ;; sha and is what the check reads. Both must be gone.
                        ;; The ledger object is the evidence; the copy at the
                        ;; recorded path carries no guarantee. Losing the
                        ;; object is a typed refusal naming the sha, not a
                        ;; silent read of whatever drifted at the path.
                        :log (let [artifact (get-in run [:payload :log-artifact])
                                   object (registry-ledger/resolve-file
                                           (:ledger artifact) (:sha256 artifact))]
                               (spit (:path artifact) "tampered")
                               (.setWritable ^java.io.File object true)
                               (io/delete-file object)
                               :log-object-missing)
                        :missing :missing-entry
                        :tamper (do (swap! backend assoc-in [:entries id :evidence/body :payload-edn] "{}") :record-digest-mismatch)
                        :truncated-chain (do (swap! backend update :entries dissoc (get-in run [:payload :previous :evidence/id]))
                                             :chain-link-mismatch))
             check (cond-> check (= mode :missing) (assoc :entry-id "absent"))
             result (registry/check-record! backend check)]
         (is (= expected (:reason result)) (pr-str [mode result]))
         (is (= :test-registry/refusal (:record/type result)))
         (is (= 1 (count @calls))))))))

(deftest results-envelope-does-not-invent-absent-counts
  (is (= (registry/none :unparsed) (:tests (registry/parse-results 0 "process exited" 5))))
  (is (false? (registry/successful? (registry/parse-results 0 "process exited" 5)))))

(deftest declared-policy-preserves-required-full-runs
  (let [spot (registry/execution-policy {:warrant? true} :routine false)]
    (is (= :spot-check (:mode spot)))
    (is (= 1 (:count spot))))
  (doseq [[check lane first?] [[{:warrant? false} :routine false]
                             [{:warrant? true} :routine true]
                             [{:warrant? true} :invariant false]
                             [{:warrant? true} :pre-push false]
                             ;; undeclared first-run status routes to full scope
                             ;; (the resolved old-policy/reviewer-lane disagreement)
                             [{:warrant? true} :routine nil]]]
    (is (= :full-scope (:mode (registry/execution-policy check lane first?))))))

(deftest unrelated-commit-does-not-stale-the-warrant
  (testing "a changed path outside manifests and closure is :outside-closure, not a refusal"
    (fixture
     (fn [{:keys [backend options]}]
       (let [run (registry/register-run! backend options)
             check (registry/check-record! backend
                                           {:entry-id (:evidence/id run)
                                            :repo-root "/fixture"
                                            :changed-paths ["src/totally-unrelated.clj"
                                                            "docs/other.md"]})]
         (is (true? (:warrant? check)))
         (is (= ["docs/other.md" "src/totally-unrelated.clj"] (:outside-closure check))))))))

(deftest changed-closure-file-refuses-naming-the-file
  (testing "a changed loaded dependency refuses :environment-mismatch with the file named"
    (fixture
     (fn [{:keys [backend options closure]}]
       (let [run (registry/register-run! backend options)]
         (swap! closure (fn [c] (mapv #(if (= "demo.clj" (last (clojure.string/split (:path %) #"/")))
                                         (assoc % :sha256 "changed-sha")
                                         %)
                                      c)))
         (let [result (registry/check-record! backend (check-options run))]
           (is (= :environment-mismatch (:reason result)))
           (is (= ["src/demo.clj"] (get-in result [:details :changed-files])))))))))

(deftest closure-diff-is-pure-and-names-added-removed-changed
  (let [recorded {"a.clj" "1" "b.clj" "2" "c.clj" "3"}]
    (is (= [] (registry/closure-diff recorded recorded)))
    (is (= ["a.clj"] (registry/closure-diff recorded (assoc recorded "a.clj" "9"))))
    (is (= ["new.clj"] (registry/closure-diff recorded (assoc recorded "new.clj" "4"))))
    (is (= ["c.clj"] (registry/closure-diff recorded (dissoc recorded "c.clj"))))))

(deftest test-namespace-of-reads-the-single-declared-namespace
  (is (= "demo-test" (registry/test-namespace-of (:command opts)))))

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
                   ["clojure" "-M:test" "-r" ".*"]
                   ["clojure" "-M:test" "-n" "one" "-v" "not-a-var"]
                   ["clojure" "-M:test" "-m" "futon3c.test-registry.runner" "-n" "one"]
                   ["lake" "build"] ["lake" "build" "A.B" "C.D"] ["lake" "build" "A B"]
                   ["lake" "exe" "cache"] ["lake" "build" "--rehash"]]]
    (is (thrown? clojure.lang.ExceptionInfo (registry/validate-command! command)) (pr-str command)))
  (is (nil? (registry/validate-command! (:command opts))))
  (is (nil? (registry/validate-command! ["clojure" "-M:test-pure" "-n" "a-test" "-v" "a-test/x"])))
  (is (nil? (registry/validate-command! ["lake" "build" "DarkTower.WarMachine.EpistemicValue"]))))

(deftest clojure-execution-injects-the-registry-runner
  (let [argv (registry/execution-command ["clojure" "-M:test-pure" "-n" "a-test" "-v" "a-test/x"]
                                         (io/file "/tmp/r.closure.edn"))]
    (is (= ["clojure" "-Sdeps"] (take 2 argv)))
    (is (str/includes? (nth argv 2) "futon3c.test-registry.runner"))
    (is (str/includes? (nth argv 2) "test-registry-runner"))
    (is (= "-M:test-pure:futon3c.test-registry/runner" (nth argv 3)))
    (is (= ["-n" "a-test" "-v" "a-test/x" "/tmp/r.closure.edn"] (drop 4 argv))))
  (is (= ["lake" "build" "M"] (registry/execution-command ["lake" "build" "M"] (io/file "x")))))

(deftest lean-header-parser-handles-module-system-syntax
  (is (= ["A.B" "C" "D.E" "F" "G"]
         (registry/lean-header-imports
          "/- copyright\n  /- nested -/ still comment -/\n-- line\nmodule\n\npublic import A.B\nimport C -- trailing\nmeta import D.E\npublic meta import F\nimport all G\n\n/-! # Doc -/\npublic section\nimport NotHeader\n")))
  (is (= ["Init.Core"] (registry/lean-header-imports "prelude\nimport Init.Core\ndef x := 1")))
  (is (= [] (registry/lean-header-imports "/-- doc -/\ntheorem t : True := trivial"))))

(deftest lean-results-record-sorries-per-file-and-require-a-clean-build
  (let [ok "✔ [12/12] Built X\nBuild completed successfully (12 jobs).\n"
        sorry (str "warning: DarkTower/WarMachine/Holes.lean:157:4: declaration uses `sorry`\n"
                   "warning: DarkTower/WarMachine/Holes.lean:999:4: declaration uses `sorry`\n"
                   "warning: A/B.lean:3:8: declaration uses 'sorry'\n"
                   "info: A/B.lean:9:0: the string declaration uses `sorry` in a message\n"
                   "Build completed successfully (12 jobs).\n")
        failed "error: A/B.lean:1:0: unknown identifier 'x'\nerror: build failed\n"
        r (registry/parse-lean-results 0 sorry 10)]
    (is (registry/lean-successful? (registry/parse-lean-results 0 ok 10)))
    (is (= 12 (:jobs (registry/parse-lean-results 0 ok 10))))
    (is (= {"A/B.lean" 1 "DarkTower/WarMachine/Holes.lean" 2} (:sorry-files r)))
    (is (= 3 (:sorry-count r)))
    (is (registry/lean-successful? r) "sorries are recorded, not a build failure")
    (is (= 2 (:error-count (registry/parse-lean-results 1 failed 10))))
    (is (false? (registry/lean-successful? (registry/parse-lean-results 1 failed 10))))
    (is (false? (registry/lean-successful? (registry/parse-lean-results 0 "no completion line" 10))))))

(deftest lean-closure-is-transitive-and-repo-bounded
  (let [dir (.toFile (Files/createTempDirectory "lean-closure-" (make-array FileAttribute 0)))
        write (fn [path text] (let [f (io/file dir path)] (io/make-parents f) (spit f text)))]
    (try
      (write "Top.lean" "import Mid\nimport Mathlib.NotHere\ndef t := 1")
      (write "Mid.lean" "module\npublic import Leaf\nimport Init.Data")
      (write "Leaf.lean" "def leaf := 0")
      (write "Unrelated.lean" "def u := 0")
      (let [closure (registry/lean-closure {:repo-root (str dir)} "Top")]
        (is (= ["Leaf.lean" "Mid.lean" "Top.lean"] (mapv :path closure)))
        (write "Unrelated.lean" "def u := 1")
        (is (= [] (registry/closure-diff (registry/closure-shas closure)
                                         (registry/current-closure-shas (str dir) closure))))
        (write "Leaf.lean" "def leaf := 2")
        (is (= ["Leaf.lean"] (registry/closure-diff (registry/closure-shas closure)
                                                    (registry/current-closure-shas (str dir) closure)))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"lean-module-source-missing"
                            (registry/lean-closure {:repo-root (str dir)} "Absent")))
      (finally (doseq [f (reverse (file-seq dir))] (io/delete-file f))))))

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

(deftest environment-refusal-names-the-differing-fields
  (fixture
   (fn [{:keys [backend env options]}]
     (let [run (registry/register-run! backend options)]
       (reset! env {:sha256 "different" :jvm {:version "other"}})
       (let [r (registry/check-record! backend (check-options run))]
         (is (= "env" (get-in r [:details :expected-only :sha256])))
         (is (= "other" (get-in r [:details :observed-only :jvm :version])))
         (is (= :reconcile-test-environment (get-in r [:details :next-action]))))))))

(deftest locale-configuration-is-retired
  (fixture
   (fn [{:keys [backend options]}]
     (try
       (registry/register-run! backend (assoc options :test-environment {}))
       (is false "Retired spec key must refuse, including an empty map")
       (catch clojure.lang.ExceptionInfo e
         (is (= :environment-not-configurable (:reason (ex-data e))))
         (is (= registry/canonical-environment
                (get-in (ex-data e) [:details :canonical-environment]))))))))

(deftest ^:slow clojure-closure-covers-dynamic-loads-and-resources
  ;; Real processes in a temp project: the test body loads `dyn` only through
  ;; requiring-resolve and reads resources/data.txt and a symlinked
  ;; resources/page.txt, so a fresh-JVM require of the test namespace would
  ;; record none of them.
  (let [dir (.toFile (Files/createTempDirectory "registry-closure-" (make-array FileAttribute 0)))
        root (str dir)
        write (fn [path text] (let [f (io/file dir path)] (io/make-parents f) (spit f text)))
        sh (fn [& argv] (let [r (apply shell/sh (concat argv [:dir root]))]
                          (assert (zero? (:exit r)) (pr-str r))))
        backend (atom {:entries {} :order []})
        check (fn [run changed] (registry/check-record! backend {:entry-id (:evidence/id run) :repo-root root
                                                                 :changed-paths changed}))]
    (try
      ;; "." on the classpath, as in futon2's :test alias.
      (write "deps.edn" "{:paths [\"src\" \"test\" \"resources\" \".\"] :deps {org.clojure/clojure {:mvn/version \"1.12.0\"}} :aliases {:t {}}}")
      (write "src/demo.clj" "(ns demo)\n(defn one [] 1)\n")
      (write "src/dyn.clj" "(ns dyn)\n(defn value [] 1)\n")
      (write "src/unrelated.clj" "(ns unrelated)\n")
      (write "resources/data.txt" "v1")
      (write "resources/unused.txt" "u1")
      (write "generated/page.txt" "p1")
      (Files/createSymbolicLink (.toPath (io/file dir "resources/page.txt"))
                                (.toPath (io/file dir "generated/page.txt"))
                                (make-array FileAttribute 0))
      (write "test/demo_test.clj"
             (str "(ns demo-test (:require [clojure.test :refer [deftest is]] [clojure.java.io :as io] [demo]))\n"
                  "(deftest dynamic (is (= (demo/one) ((requiring-resolve 'dyn/value))))"
                  " (is (= \"v1\" (slurp (io/resource \"data.txt\"))))"
                  " (is (= \"p1\" (slurp (io/resource \"page.txt\")))))\n"))
      (sh "git" "init" "-q") (sh "git" "add" ".")
      (sh "git" "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-qm" "fixture")
      (let [run (registry/register-run! backend {:repo-root root :command ["clojure" "-M:t" "-n" "demo-test"]
                                                 :code-paths ["src/demo.clj"] :test-paths ["test/demo_test.clj"]
                                                 :author "author" :artifact-dir (str root "/.artifacts")})
            paths (set (map :path (get-in run [:payload :load-closure])))]
        (is (true? (get-in run [:payload :warrant?])) (pr-str (:payload run)))
        (is (= 3 (get-in run [:payload :results :assertions])))
        (is (contains? paths "resources/data.txt") (pr-str paths))
        (is (contains? paths "resources/page.txt") (pr-str paths))
        (is (contains? paths "src/dyn.clj") (pr-str paths))
        (is (contains? paths "test/demo_test.clj") (pr-str paths))
        (write "src/unrelated.clj" "(ns unrelated)\n;; edited\n")
        (let [r (check run ["src/unrelated.clj"])]
          (is (true? (:warrant? r)) (pr-str r))
          (is (= ["src/unrelated.clj"] (:outside-closure r))))
        (write "src/dyn.clj" "(ns dyn)\n;; edited\n(defn value [] 1)\n")
        (let [r (check run ["src/dyn.clj"])]
          (is (= :environment-mismatch (:reason r)))
          (is (= ["src/dyn.clj"] (get-in r [:details :changed-files]))))
        (write "src/dyn.clj" "(ns dyn)\n(defn value [] 1)\n")
        (is (true? (:warrant? (check run []))))
        (write "resources/unused.txt" "u2")
        (write "notes.md" "a new file at the repo root")
        (is (true? (:warrant? (check run ["resources/unused.txt" "notes.md"]))))
        (write "generated/page.txt" "p2")
        (let [r (check run [])]
          (is (= :environment-mismatch (:reason r)))
          (is (= ["resources/page.txt"] (get-in r [:details :changed-files]))))
        (write "generated/page.txt" "p1")
        (write "resources/data.txt" "v2")
        (let [r (check run ["resources/data.txt"])]
          (is (= :environment-mismatch (:reason r)))
          (is (= ["resources/data.txt"] (get-in r [:details :changed-files])))))
      (finally (doseq [f (reverse (file-seq dir))] (io/delete-file f true))))))

(defn reseal!
  "Rewrite a stored record's payload and re-derive its id the way
  append-record! does, so the digest check passes and the check under test is
  the one that fires."
  [backend id f]
  (let [entry (get-in @backend [:entries id])
        payload (f (edn/read-string (get-in entry [:evidence/body :payload-edn])))
        text (pr-str (#'registry/canonical payload))
        hash (digest/sha256 text)
        new-id (str "test-registry-" hash)
        resealed (assoc entry :evidence/id new-id
                        :evidence/body {:payload-edn text :sha256 hash})]
    (swap! backend (fn [b]
                     (-> b
                         (update :entries dissoc id)
                         (assoc-in [:entries new-id] resealed)
                         (update :order (fn [order] (mapv #(if (= id %) new-id %) order))))))
    new-id))

(deftest a-record-from-an-older-reader-refuses-as-superseded-not-as-tampered
  (fixture
   (fn [{:keys [backend options]}]
     (let [run (registry/register-run! backend options)]
       (is (= registry/reader-version (get-in run [:payload :reader-version]))
           "a run records the reader that read it")
       ;; Strip the key, as every record written before 2026-09-17 has it
       ;; stripped, and break exactly what a newer predicate would ask for.
       (let [id (reseal! backend (:evidence/id run)
                         #(-> % (dissoc :reader-version)
                              (assoc-in [:results :tests] (registry/none :unparsed))))
             result (registry/check-record! backend
                                            (assoc (check-options run) :entry-id id))]
         (is (= :parser-superseded (:reason result)) (pr-str result))
         (is (= 0 (get-in result [:details :recorded-reader-version])))
         (is (= registry/reader-version (get-in result [:details :reader-version])))
         (is (= :re-register-the-run (get-in result [:details :next-action]))))))))

(deftest a-current-reader-record-still-refuses-as-unsupported
  (fixture
   (fn [{:keys [backend options]}]
     (let [run (registry/register-run! backend options)
           id (reseal! backend (:evidence/id run)
                       #(assoc-in % [:results :tests] (registry/none :unparsed)))
           result (registry/check-record! backend
                                          (assoc (check-options run) :entry-id id))]
       (is (= :unsupported-results (:reason result))
           "a record its own reader could have satisfied is not superseded")))))

;; --- scope guard (zai-1's ruling, 2026-09-17) -------------------------------

(defn- git! [root & args]
  (apply shell/sh (concat args [:dir root])))

(defn- temp-repo []
  (let [dir (str (Files/createTempDirectory "scope-repo-" (make-array FileAttribute 0)))]
    (git! dir "git" "init" "-q")
    (git! dir "git" "config" "user.email" "t@example.com")
    (git! dir "git" "config" "user.name" "t")
    dir))

(defn- write! [root path content]
  (let [f (io/file root path)]
    (io/make-parents f)
    (spit f content)
    f))

(deftest committed-and-clean-scope-passes
  (let [root (temp-repo)]
    (write! root "src/a.clj" "(ns a)")
    (git! root "git" "add" "-A") (git! root "git" "commit" "-qm" "a")
    (is (= [] (registry/uncommitted-scope root ["src/a.clj"])))
    (is (= [] (registry/uncommitted-scope root ["src"])))))

(deftest a-modified-scope-file-is-refused
  (let [root (temp-repo)]
    (write! root "src/a.clj" "(ns a)")
    (git! root "git" "add" "-A") (git! root "git" "commit" "-qm" "a")
    (write! root "src/a.clj" "(ns a) ;; edited, not committed")
    (is (= [{:path "src/a.clj" :reason :dirty :repo root}]
           (registry/uncommitted-scope root ["src/a.clj"])))
    (is (= [:dirty] (map :reason (registry/uncommitted-scope root ["src"])))
        "a directory scope is dirty when anything under it is")))

(deftest an-untracked-scope-file-is-refused
  (let [root (temp-repo)]
    (write! root "src/a.clj" "(ns a)")
    (git! root "git" "add" "-A") (git! root "git" "commit" "-qm" "a")
    (write! root "src/never-committed.clj" "(ns b)")
    (is (= [:dirty] (map :reason (registry/uncommitted-scope root ["src/never-committed.clj"])))
        "git reports an untracked file as unclean, which is the same refusal")))

(deftest a-dependency-artifact-is-exempt
  (let [jar (str (System/getProperty "user.home")
                 "/.m2/repository/org/clojure/clojure/1.12.0/clojure-1.12.0.jar")]
    (is (= [] (registry/uncommitted-scope "/tmp" [jar]))
        "a .m2 jar is immutable and already pinned by sha; requiring it tracked
         would refuse every Clojure warrant")))

(deftest a-stray-parent-repository-does-not-exempt-ordinary-paths
  (let [scratch (str (Files/createTempDirectory "notarepo-" (make-array FileAttribute 0))
                     "/scratch.clj")]
    (spit scratch "(ns scratch)")
    (is (= [:untracked] (map :reason (registry/uncommitted-scope "/tmp" [scratch])))
        "/tmp has a stray .git on this box; a scope file under it is still
         unreproducible and must refuse")))

(deftest the-guard-names-what-to-do
  (let [root (temp-repo)]
    (write! root "src/a.clj" "(ns a)")
    (let [result (try (#'registry/require-committed-scope! root ["src/a.clj"] :scope)
                      (catch Exception e (ex-data e)))]
      (is (= :scope-not-committed (:reason result)))
      (is (= :commit-before-registering (get-in result [:details :next-action])))
      (is (= :scope (get-in result [:details :stage]))))))

(deftest a-ledger-that-cannot-take-the-log-refuses-the-registration
  (fixture
   (fn [{:keys [backend options]}]
     (let [blocked (str (Files/createTempFile "not-a-dir-" ".txt"
                                              (make-array FileAttribute 0)))
           result (try (registry/register-run! backend (assoc options :ledger-root blocked))
                       (catch Exception e (ex-data e)))]
       (is (= :ledger-write-failed (:reason result))
           "a path-pinned warrant is the failure the ledger exists to prevent")
       (is (= :fix-the-ledger-and-re-register
              (get-in result [:details :next-action])))))))

(deftest a-run-that-never-warranted-anything-is-not-a-reader-question
  (fixture
   (fn [{:keys [backend options]}]
     ;; v0 record (no :reader-version) whose tests failed at run time. The
     ;; reader-version branch must not claim re-registering will help: the
     ;; tests will fail again (zai-1 review, 2026-09-17).
     (let [run (registry/register-run! backend options)
           id (reseal! backend (:evidence/id run)
                       #(-> % (dissoc :reader-version) (assoc :warrant? false)))
           result (registry/check-record! backend
                                          (assoc (check-options run) :entry-id id))]
       (is (= :not-a-warrant (:reason result)) (pr-str result))
       (is (= :fix-the-run-not-the-record (get-in result [:details :next-action])))))))

(deftest an-unstable-run-is-not-a-reader-question-either
  (fixture
   (fn [{:keys [backend options]}]
     (let [run (registry/register-run! backend options)
           id (reseal! backend (:evidence/id run)
                       #(-> % (dissoc :reader-version) (assoc :execution/stable? false)))
           result (registry/check-record! backend
                                          (assoc (check-options run) :entry-id id))]
       (is (= :not-a-warrant (:reason result)))
       (is (false? (get-in result [:details :execution/stable?])))))))

(deftest a-results-mismatch-says-what-differs
  (fixture
   (fn [{:keys [backend options]}]
     (let [run (registry/register-run! backend options)
           id (reseal! backend (:evidence/id run)
                       #(assoc-in % [:results :assertions] 999))
           result (registry/check-record! backend
                                          (assoc (check-options run) :entry-id id))]
       (is (= :results-log-mismatch (:reason result)))
       (is (= 999 (get-in result [:details :recorded-only :assertions])))
       (is (some? (get-in result [:details :observed-only :assertions]))
           "a human adjudicating needs both sides, as :environment-mismatch gives")))))

(deftest a-lost-ledger-object-refuses-by-name
  (fixture
   (fn [{:keys [backend options]}]
     (let [run (registry/register-run! backend options)
           artifact (get-in run [:payload :log-artifact])
           object (registry-ledger/resolve-file (:ledger artifact) (:sha256 artifact))]
       (.setWritable ^java.io.File object true)
       (io/delete-file object)
       (let [result (registry/check-record! backend (check-options run))]
         (is (= :log-object-missing (:reason result)) (pr-str result))
         (is (= (:sha256 artifact) (get-in result [:details :sha256]))
             "name the sha, so a lost object is debuggable rather than an NPE")
         (is (= :re-register-the-run (get-in result [:details :next-action]))))))))

(deftest a-ledger-object-edited-in-place-still-fails-the-hash
  (fixture
   (fn [{:keys [backend options]}]
     (let [run (registry/register-run! backend options)
           artifact (get-in run [:payload :log-artifact])
           object (registry-ledger/resolve-file (:ledger artifact) (:sha256 artifact))]
       (.setWritable ^java.io.File object true)
       (spit object "someone edited the ledger itself")
       (let [result (registry/check-record! backend (check-options run))]
         (is (= :log-mismatch (:reason result)))
         (is (= (:sha256 artifact) (get-in result [:details :sha256]))))))))

(def ^:private NUL (str (char 0)))

(deftest an-uncommitted-closure-keeps-the-run-record-and-refuses-the-warrant
  ;; A 30-minute build must not be thrown away to report a condition the
  ;; record can state (zai-1 review, 2026-09-17).
  (let [repo (temp-repo)
        _ (write! repo "src/loaded.clj" "(ns loaded)")
        dirty (.getCanonicalPath (io/file repo "src/loaded.clj"))
        ran (atom false)]
    (fixture
     (fn [{:keys [backend options closure]}]
       (reset! closure [{:ns "loaded" :path dirty :sha256 "blob"}])
       (let [run (registry/register-run! backend options)
             payload (:payload run)]
         (is (= :run (:kind payload)) "the run record still lands")
         (is (false? (:warrant? payload)))
         (is (= :scope-not-committed (get-in payload [:postcheck :reason])))
         (is (= :load-closure (get-in payload [:postcheck :details :stage])))
         (is (= :commit-before-registering
                (get-in payload [:postcheck :details :next-action])))
         (is (some? (get-in payload [:log-artifact :ledger]))
             "and its log is anchored in the ledger, not left in scratch")
         (reset! ran true))))
    (is (true? @ran))))

(deftest a-rename-in-flight-does-not-corrupt-the-unclean-set
  (let [out (str "R  src/new.clj" NUL "src/old.clj" NUL " M src/other.clj" NUL)]
    (is (= #{"src/new.clj" "src/old.clj" "src/other.clj"}
           (registry/unclean-paths out))
        "the second field of a rename carries no XY prefix and must be
         consumed, not sliced; the old path is kept because a scope may pin it")))

(deftest an-ordinary-status-still-parses
  (is (= #{"src/a.clj" "test/b.clj"}
         (registry/unclean-paths (str " M src/a.clj" NUL "?? test/b.clj" NUL)))))

;; --- the runner is the instrument, not the specimen (zai-1 ruling) ----------

(deftest the-runner-is-dropped-from-a-recorded-closure
  (let [entries [{:ns "demo" :url (str "file://" (System/getProperty "user.dir") "/deps.edn")}
                 {:ns registry/runner-namespace
                  :url (str "file://" (System/getProperty "user.dir")
                            "/test-registry-runner/src/futon3c/test_registry/runner.clj")}]
        closure (registry/closure-from-entries (vec entries) (System/getProperty "user.dir"))]
    (is (= 1 (count closure)))
    (is (not-any? registry/instrument? closure))
    (is (some? (registry/runner-sha (vec entries) (System/getProperty "user.dir")))
        "its sha is still recorded for the audit trail")))

(deftest an-old-record-pinning-the-runner-is-not-stale-when-only-the-runner-moved
  ;; 38 of 39 Clojure warrants pinned it before 2026-09-17. Excluding it only
  ;; at registration would leave every one of them stale on a comment.
  (let [recorded {"src/a.clj" "aaa"
                  "/home/joe/code/futon3c/test-registry-runner/src/futon3c/test_registry/runner.clj" "old"}
        observed {"src/a.clj" "aaa"
                  "/home/joe/code/futon3c/test-registry-runner/src/futon3c/test_registry/runner.clj" "new"}]
    (is (= [] (registry/closure-diff recorded observed)))))

(deftest a-real-closure-change-still-refuses
  (let [recorded {"src/a.clj" "aaa"
                  "/home/joe/code/futon3c/test-registry-runner/src/futon3c/test_registry/runner.clj" "old"}
        observed {"src/a.clj" "CHANGED"
                  "/home/joe/code/futon3c/test-registry-runner/src/futon3c/test_registry/runner.clj" "new"}]
    (is (= ["src/a.clj"] (registry/closure-diff recorded observed))
        "only the instrument is exempt; the specimen is not")))

(deftest instrument-recognises-both-the-namespace-and-the-source-path
  (is (registry/instrument? {:ns registry/runner-namespace :path "wherever.clj"}))
  (is (registry/instrument? {:ns "resource:x"
                             :path "/somewhere/futon3c/test_registry/runner.clj"}))
  (is (not (registry/instrument? {:ns "futon2.aif.trace" :path "src/futon2/aif/trace.clj"}))))

;; ---------------------------------------------------------------------------
;; AR-42: lookup by namespace. The registry could be asked "is THIS record
;; still good" but not "which record covers this namespace", so futon2's :C8
;; had to be handed an entry id. These cover the rule the lookup states.

(defn- append-run!
  "A synthetic :run record through the registry's own append, so it decodes
  exactly as a minted one does. Used where a real run cannot produce the
  condition under test (two records sharing a :ran-at)."
  [backend {:keys [namespace ran-at finished-at failures warrant?]
            :or {failures 0 warrant? true}}]
  (registry/append-record!
   backend
   {:kind :run :author "author" :run/id (str (java.util.UUID/randomUUID))
    :ran-at ran-at :finished-at (or finished-at ran-at)
    :command ["clojure" "-M:test" "-n" namespace]
    :code-files {"src/demo.clj" "blob"} :test-files {"test/demo_test.clj" "test-blob"}
    :results {:tests 1 :assertions 1 :failures failures :errors 0 :exit 0}
    :postcheck {:status :matched} :warrant? warrant?}
   nil))

(defn- failing-run!
  "Register a real run whose command reports a failure. The record is still
  appended — a failed run is a fact about the code, not a registry error."
  [backend options]
  (with-redefs [registry/run-process!
                (fn [_ _ log]
                  (let [text "Ran 3 tests containing 7 assertions.\n1 failures, 0 errors.\n"]
                    (spit log text)
                    (registry/parse-results 1 text 40)))]
    (registry/register-run! backend options)))

(deftest latest-run-for-namespace-resolves-to-the-newest-not-the-warranted
  (fixture
   (fn [{:keys [backend options]}]
     (let [older (registry/register-run! backend options)
           newer (failing-run! backend options)
           found (registry/latest-run-for-namespace backend {:namespace "demo-test"})]
       ;; the older run really is the one a warrant-filtering lookup would pick
       (is (true? (get-in older [:payload :warrant?])))
       (is (= :matched (get-in older [:payload :postcheck :status])))
       (is (false? (get-in newer [:payload :warrant?])))
       ;; ... and the lookup picks the newer one anyway, which is the point
       (is (= (:evidence/id newer) (:evidence/id found))
           (str "picked " (:evidence/id found) " ran-at " (get-in found [:payload :ran-at])))
       (is (= 1 (get-in found [:payload :results :failures])))
       (is (= :run (get-in found [:payload :kind])))
       ;; the row shape is the one read-chain! returns, plus what was scanned
       (is (string? (:sha256 found)))
       (is (pos? (:scanned found)))))))

(deftest latest-run-for-namespace-types-its-absence
  (fixture
   (fn [{:keys [backend options]}]
     (registry/register-run! backend options)
     (let [none (registry/latest-run-for-namespace backend {:namespace "futon3c.not-registered-test"})]
       (is (some? none) "absence is a value, never nil")
       (is (= :none (:status none)))
       (is (= :no-run-for-namespace (:reason none)))
       (is (nil? (:payload none)))
       ;; absence is only claimed because the scan saw every registry entry
       (is (= (:scanned none) (:registry-entries none))))
     ;; a scan that saw less than the registry holds did NOT establish absence
     (let [capped (registry/latest-run-for-namespace
                   backend {:namespace "futon3c.not-registered-test" :limit 1})]
       (is (= :scan-window-exhausted (:reason capped)))
       (is (= 1 (:limit capped)))
       (is (= 1 (:scanned capped)))
       (is (< (:scanned capped) (:registry-entries capped))))
     ;; a namespace is required: without one there is nothing to look up
     (is (= :namespace-required (:reason (registry/latest-run-for-namespace backend {}))))
     (is (= :test-registry/refusal
            (:record/type (registry/latest-run-for-namespace backend {:namespace "  "})))))))

(deftest latest-run-for-namespace-breaks-ties-as-documented
  (fixture
   (fn [{:keys [backend]}]
     (let [stamp "2026-09-25T01:00:00Z"
           first-finished (append-run! backend {:namespace "tie-test" :ran-at stamp
                                                :finished-at "2026-09-25T01:00:05Z"})
           last-finished (append-run! backend {:namespace "tie-test" :ran-at stamp
                                               :finished-at "2026-09-25T01:00:09Z"})
           found (registry/latest-run-for-namespace backend {:namespace "tie-test"})]
       ;; same :ran-at, so :finished-at decides
       (is (= (:evidence/id last-finished) (:evidence/id found)))
       (is (not= (:evidence/id first-finished) (:evidence/id found)))))))

(deftest latest-run-for-namespace-orders-by-instant-not-by-string
  (fixture
   (fn [{:keys [backend]}]
     ;; "…:40Z" sorts AFTER "…:40.387Z" as a string while being 387ms earlier
     ;; in fact. The newest here is the fractional one.
     (let [whole (append-run! backend {:namespace "order-test" :ran-at "2026-09-25T01:00:40Z"})
           fractional (append-run! backend {:namespace "order-test" :ran-at "2026-09-25T01:00:40.387Z"})
           found (registry/latest-run-for-namespace backend {:namespace "order-test"})]
       (is (= (:evidence/id fractional) (:evidence/id found)))
       (is (not= (:evidence/id whole) (:evidence/id found)))))))

(deftest latest-run-for-namespace-names-what-it-could-not-read
  (fixture
   (fn [{:keys [backend]}]
     (let [good (append-run! backend {:namespace "undecodable-test"
                                      :ran-at "2026-09-25T01:00:00Z"})]
       ;; corrupt one entry's body in place: its text no longer hashes to its id
       (swap! backend update :entries
              (fn [entries]
                (reduce-kv (fn [m id entry]
                             (assoc m id (if (= id (:evidence/id good))
                                           entry
                                           (assoc-in entry [:evidence/body :payload-edn] "{:kind :run}"))))
                           {} entries)))
       (let [found (registry/latest-run-for-namespace backend {:namespace "undecodable-test"})]
         (is (= (:evidence/id good) (:evidence/id found)))
         ;; if anything was unreadable it is named, not dropped where no
         ;; reader can see it
         (is (every? :evidence/id (:undecodable found))))))))

;; ---------------------------------------------------------------------------
;; AR-42 finding i: the namespace ledger. Registration maintains a
;; namespace -> newest-run index, the lookup consults it first, and absence
;; from it is only as good as the recorded completeness of its build.

(deftest namespace-ledger-holds-the-newer-run-not-the-warranted
  ;; two runs for one namespace, older warranted and newer failing: the
  ;; ledger holds the newer, by the same rule the scan applies
  (fixture
   (fn [{:keys [backend options]}]
     (let [ledger (str (io/file (:artifact-dir options) "namespaces.ednlog"))
           options (assoc options :namespace-ledger-file ledger)
           older (registry/register-run! backend options)
           newer (failing-run! backend options)
           found (registry/latest-run-for-namespace
                  backend {:namespace "demo-test" :namespace-ledger-file ledger})]
       (is (true? (get-in older [:payload :warrant?])))
       (is (false? (get-in newer [:payload :warrant?])))
       (is (= (:evidence/id newer) (:evidence/id found)))
       (is (= :namespace-ledger (:resolved-by found)))
       (is (= 1 (get-in found [:payload :results :failures])))))))

(deftest namespace-ledger-fills-forward-from-registration
  ;; a run registered after the ledger was built appears without a rescan:
  ;; presence never waits on the build marker's completeness
  (fixture
   (fn [{:keys [backend options]}]
     (let [ledger (str (io/file (:artifact-dir options) "namespaces.ednlog"))
           built (registry/build-namespace-ledger! backend {:namespace-ledger-file ledger})]
       ;; an empty registry is never a complete scan (a failed read looks
       ;; exactly like one), but that only gates absence, not presence
       (is (false? (:complete? built)))
       (let [run (registry/register-run! backend (assoc options :namespace-ledger-file ledger))
             found (registry/latest-run-for-namespace
                    backend {:namespace "demo-test" :namespace-ledger-file ledger})]
         (is (= (:evidence/id run) (:evidence/id found)))
         (is (= :namespace-ledger (:resolved-by found))))))))

(deftest an-incomplete-namespace-ledger-refuses-absence
  ;; a ledger built from a scan that saw fewer entries than the store holds
  ;; cannot conclude absence: it reports the window it recorded
  (fixture
   (fn [{:keys [backend options]}]
     (let [ledger (str (io/file (:artifact-dir options) "namespaces.ednlog"))]
       (registry/register-run! backend options)
       (registry/register-run! backend options)
       (let [built (registry/build-namespace-ledger!
                    backend {:namespace-ledger-file ledger :limit 1})]
         (is (false? (:complete? built)))
         (is (= 1 (:scanned built)))
         (is (= 4 (:registry-entries built)))) ;; intent + run, twice
       (let [none (registry/latest-run-for-namespace
                   backend {:namespace "futon3c.not-registered-test"
                            :namespace-ledger-file ledger})]
         (is (= :none (:status none)))
         (is (= :scan-window-exhausted (:reason none)))
         (is (= :namespace-ledger-incomplete (:resolved-by none)))
         (is (= 1 (:scanned none)))
         (is (= 4 (:registry-entries none))))))))

(deftest a-ledger-hit-never-fetches-an-evidence-page
  ;; the lookup answers within the client's timeout because a ledger hit is
  ;; one targeted store read -- no evidence page is ever fetched (2026-09-25,
  ;; live: the endpoint answers a ledger hit in ~4s, while the default page
  ;; times the 5s client out)
  (fixture
   (fn [{:keys [backend options]}]
     (let [ledger (str (io/file (:artifact-dir options) "namespaces.ednlog"))
           run (registry/register-run! backend (assoc options :namespace-ledger-file ledger))
           real-get-entry* store/get-entry*
           pages (atom 0) reads (atom 0)]
       (with-redefs [store/query* (fn [_ _] (swap! pages inc) [])
                     store/count* (fn [_ _] (swap! pages inc) 0)
                     store/get-entry* (fn [b id] (swap! reads inc) (real-get-entry* b id))]
         (let [found (registry/latest-run-for-namespace
                      backend {:namespace "demo-test" :namespace-ledger-file ledger})]
           (is (= (:evidence/id run) (:evidence/id found)))
           (is (= :namespace-ledger (:resolved-by found)))
           (is (zero? @pages) "no evidence page was fetched for a ledger hit")
           (is (= 3 @reads) "targeted reads only: the chain's links and parent, never a page")))))))

(deftest a-complete-namespace-ledger-concludes-absence
  ;; first use builds the ledger from one full scan; a complete build plus
  ;; forward fills is the positive sign absence needs
  (fixture
   (fn [{:keys [backend options]}]
     (let [ledger (str (io/file (:artifact-dir options) "namespaces.ednlog"))]
       (registry/register-run! backend options)
       (let [none (registry/latest-run-for-namespace
                   backend {:namespace "futon3c.not-registered-test"
                            :namespace-ledger-file ledger})]
         (is (= :no-run-for-namespace (:reason none)))
         (is (= :namespace-ledger (:resolved-by none)))
         (is (= (:scanned none) (:registry-entries none))))
       ;; and the build did not disturb the scan path for the registered one
       (let [found (registry/latest-run-for-namespace
                    backend {:namespace "demo-test" :namespace-ledger-file ledger})]
         (is (= :namespace-ledger (:resolved-by found))))))))

;; ---------------------------------------------------------------------------
;; AR-42 follow-up: the ledger reaches the live lookup and the CLI. Both entry
;; points thread namespace-ledger-path, whose documented default sits under
;; the checkout's data directory, so a configured server and the CLI agree on
;; one append-only ledger with no env var.

(deftest namespace-ledger-path-resolves-option-then-default
  ;; the explicit option overrides the documented default; unset, the default
  ;; is <root>/data/test-registry/namespace-ledger.edn under the given root
  (is (= "/tmp/explicit/namespaces.ednlog"
         (registry/namespace-ledger-path
          {:namespace-ledger-file "/tmp/explicit/namespaces.ednlog"
           :futon3c-root "/tmp/some-checkout"})))
  (is (= (str (io/file "/tmp/some-checkout" "data" "test-registry"
                       "namespace-ledger.edn"))
         (registry/namespace-ledger-path {:futon3c-root "/tmp/some-checkout"}))))

(deftest a-cli-registration-is-found-through-the-handler-path
  ;; the register subcommand and the /latest handler both thread
  ;; namespace-ledger-path over the same root: a registration ledgered by the
  ;; CLI path resolves live by the handler path with no option and no env var
  (fixture
   (fn [{:keys [backend options]}]
     (let [root (str (io/file (:artifact-dir options) "checkout"))
           cli-options (assoc options :namespace-ledger-file
                              (registry/namespace-ledger-path {:futon3c-root root}))
           run (registry/register-run! backend cli-options)
           found (registry/latest-run-for-namespace
                  backend {:namespace "demo-test"
                           :namespace-ledger-file
                           (registry/namespace-ledger-path {:futon3c-root root})})]
       (is (= (:evidence/id run) (:evidence/id found)))
       (is (= :namespace-ledger (:resolved-by found)))))))

(deftest an-unset-option-builds-the-default-ledger-once
  ;; with the option unset and the default file absent, the first lookup
  ;; performs the one allowed build and records its scan in the marker
  (fixture
   (fn [{:keys [backend options]}]
     (registry/register-run! backend options)
     (let [root (str (io/file (:artifact-dir options) "checkout"))
           default-file (registry/namespace-ledger-path {:futon3c-root root})]
       (is (not (.isFile (io/file default-file)))
           "no ledger exists before first use")
       (let [found (registry/latest-run-for-namespace
                    backend {:namespace "demo-test"
                             :namespace-ledger-file default-file})]
         (is (= :namespace-ledger (:resolved-by found)))
         (is (.isFile (io/file default-file)) "first use built the ledger")
         (let [{:keys [built]} (registry/namespace-ledger default-file)]
           (is (some? built) "the build recorded its marker")
           (is (true? (:complete? built)))
           (is (pos? (:scanned built)))
           (is (= (:scanned built) (:registry-entries built)))))))))

(deftest latest-run-for-namespace-will-not-call-a-failed-read-absence
  ;; http-backend substitutes [] for a failed -query and 0 for a failed -count,
  ;; so "nothing scanned, nothing held" is what a timed out read looks like.
  ;; It must refuse, not report an unregistered namespace.
  (let [dead (reify futon3c.evidence.backend/EvidenceBackend
               (-append [_ _] nil)
               (-get [_ _] nil)
               (-exists? [_ _] false)
               (-query [_ _] [])
               (-count [_ _] 0)
               (-forks-of [_ _] [])
               (-delete! [_ _] {:compacted 0})
               (-all [_] []))
        answer (registry/latest-run-for-namespace dead {:namespace "demo-test"})]
    (is (= :scan-window-exhausted (:reason answer)))
    (is (= 0 (:scanned answer)))
    (is (not= :no-run-for-namespace (:reason answer)))))

;; ---------------------------------------------------------------------------
;; AR-43: a failed or silently windowed evidence read never reads as a
;; complete empty result. The lookup must refuse, typed, on a backend
;; read-failed map or on a page the server windowed by default.

(defn- failing-backend
  "An EvidenceBackend whose -query and -count return the AR-43 typed failure."
  [query-result count-result]
  (reify futon3c.evidence.backend/EvidenceBackend
    (-append [_ _] nil)
    (-get [_ _] nil)
    (-exists? [_ _] false)
    (-query [_ _] query-result)
    (-count [_ _] count-result)
    (-forks-of [_ _] [])
    (-delete! [_ _] {:compacted 0})
    (-all [_] [])))

(deftest latest-run-for-namespace-refuses-a-failed-query
  ;; the read itself failed: the typed failure surfaces as
  ;; :registry-read-failed, never :no-run-for-namespace
  (let [failure {:error/component :E-store :error/code :read-failed
                 :error/kind :timeout :status nil}
        answer (registry/latest-run-for-namespace
                (failing-backend failure 0) {:namespace "demo-test"})]
    (is (= :none (:status answer)))
    (is (= :registry-read-failed (:reason answer)))
    (is (= :timeout (get-in answer [:read-error :error/kind])))))

(deftest latest-run-for-namespace-refuses-a-failed-count
  ;; the page read fine but named no namespace, and the completeness count
  ;; failed: absence is not established
  (fixture
   (fn [{:keys [backend]}]
     (append-run! backend {:namespace "other-test" :ran-at "2026-09-25T01:00:00Z"})
     (with-redefs [store/count* (fn [_ _]
                                  {:error/component :E-store :error/code :read-failed
                                   :error/kind :http :status 500})]
       (let [answer (registry/latest-run-for-namespace backend {:namespace "demo-test"})]
         (is (= :registry-read-failed (:reason answer)))
         (is (= 500 (get-in answer [:read-error :status]))))))))

(deftest latest-run-for-namespace-refuses-a-defaulted-window
  ;; a clean 200 whose window the server applied BY DEFAULT is an incomplete
  ;; scan, even though the read succeeded
  (fixture
   (fn [{:keys [backend]}]
     (append-run! backend {:namespace "other-test" :ran-at "2026-09-25T01:00:00Z"})
     (let [real-query* store/query*]
       (with-redefs [store/query* (fn [b q]
                                    (with-meta (real-query* b q)
                                               {:window {:since "2026-09-23T00:00:00Z"
                                                         :before nil :defaulted? true}}))]
         (let [answer (registry/latest-run-for-namespace backend {:namespace "demo-test"})]
           (is (= :scan-window-exhausted (:reason answer)))
           (is (true? (get-in answer [:window :defaulted?])))))))))
