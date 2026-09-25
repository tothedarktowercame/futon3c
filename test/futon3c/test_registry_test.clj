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
  [backend {:keys [namespace command ran-at finished-at failures warrant?]
            :or {failures 0 warrant? true}}]
  (registry/append-record!
   backend
   {:kind :run :author "author" :run/id (str (java.util.UUID/randomUUID))
    :ran-at ran-at :finished-at (or finished-at ran-at)
    :command (or command ["clojure" "-M:test" "-n" namespace])
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

(defn- fail-on-paged-backend
  "Wraps backend: the first page reads fine, any :query/before page returns
  the AR-43 typed read-failed map — a store that dies mid-scan."
  [backend]
  (reify futon3c.evidence.backend/EvidenceBackend
    (-append [_ _] nil)
    (-get [_ _] nil)
    (-exists? [_ _] false)
    (-query [_ q] (if (:query/before q)
                    {:error/component :E-store :error/code :read-failed
                     :error/kind :timeout :status nil}
                    (store/query* backend q)))
    (-count [_ q] (store/count* backend q))
    (-forks-of [_ _] [])
    (-delete! [_ _] {:compacted 0})
    (-all [_] [])))

(deftest a-ledger-build-pages-until-the-scan-is-whole
  ;; more entries than one page: the build pages on :query/before cursors
  ;; until a short page, so :scanned reaches the store's count with no fixed
  ;; cap (the capped one-page build recorded scanned 2000 of 3166 and could
  ;; never complete)
  (fixture
   (fn [{:keys [backend options]}]
     (dotimes [i 12]
       (append-run! backend {:namespace (str "paged-" i "-test")
                             :ran-at (format "2026-09-25T01:00:%02dZ" i)}))
     (let [ledger (str (io/file (:artifact-dir options) "namespaces.ednlog"))
           built (registry/build-namespace-ledger!
                  backend {:namespace-ledger-file ledger :page-size 5})]
       (is (true? (:complete? built)))
       (is (= 12 (:scanned built)))
       (is (= 12 (:registry-entries built)))
       ;; and a paged-in run resolves through the ledger, not a rescan
       (let [found (registry/latest-run-for-namespace
                    backend {:namespace "paged-3-test" :namespace-ledger-file ledger})]
         (is (= :namespace-ledger (:resolved-by found))))))))

(deftest an-undecodable-entry-is-named-on-the-marker-not-a-mute-incomplete
  ;; review bad case (claude-8, 2026-09-25): the live rebuild over the whole
  ;; store read scanned 3196 of 3196 and :complete? false with NO reason on
  ;; the marker. An entry the build cannot decode must be named, with its
  ;; reason, on the marker and on the absence refusal that cites it.
  (fixture
   (fn [{:keys [backend options]}]
     (append-run! backend {:namespace "decodable-test" :ran-at "2026-09-25T01:00:00Z"})
     (let [text "{:kind :run :schema \"test-registry/v1\" :author \"author\"}"
           bad-id (str "test-registry-" (digest/sha256 "not the text"))
           receipt ((requiring-resolve 'futon3c.evidence.boundary/append!)
                    backend
                    {:evidence/id bad-id
                     :evidence/subject {:ref/type :session :ref/id "corrupt"}
                     :evidence/type :coordination :evidence/claim-type :observation
                     :evidence/author "author" :evidence/at "2026-09-25T01:00:01Z"
                     :evidence/tags [:test-registry]
                     :evidence/body {:payload-edn text :sha256 "0000"}})
           _ (is (:ok receipt) (pr-str receipt))
           ledger (str (io/file (:artifact-dir options) "namespaces.ednlog"))
           built (registry/build-namespace-ledger!
                  backend {:namespace-ledger-file ledger :page-size 5})]
       (is (false? (:complete? built)))
       (is (= 2 (:scanned built)))
       (is (= [{:evidence/id bad-id :reason :record-digest-mismatch}] (:undecodable built)))
       (let [none (registry/latest-run-for-namespace
                   backend {:namespace "futon3c.not-registered-test"
                            :namespace-ledger-file ledger})]
         (is (= :namespace-ledger-incomplete (:resolved-by none)))
         (is (= [{:evidence/id bad-id :reason :record-digest-mismatch}] (:undecodable none))))
       (let [found (registry/latest-run-for-namespace
                    backend {:namespace "decodable-test" :namespace-ledger-file ledger})]
         (is (= :namespace-ledger (:resolved-by found)) "the decodable run still resolves"))))))

(deftest a-foreign-entry-under-the-tag-is-listed-and-does-not-block-completeness
  ;; live, 2026-09-25: nine :memory notes by zai-1 carry the :test-registry
  ;; tag with e-<uuid> ids. They are not registry records (append-record!
  ;; mints test-registry-<sha> ids) and cannot have named a command, so the
  ;; build lists them as :foreign and still completes; an id of the
  ;; registry's own form that will not decode stays :undecodable (previous
  ;; test).
  (fixture
   (fn [{:keys [backend options]}]
     (append-run! backend {:namespace "decodable-test" :ran-at "2026-09-25T01:00:00Z"})
     (let [receipt ((requiring-resolve 'futon3c.evidence.boundary/append!)
                    backend
                    {:evidence/id "e-00000000-0000-4000-8000-000000000001"
                     :evidence/subject {:ref/type :component :ref/id "futon3c"}
                     :evidence/type :memory :evidence/claim-type :observation
                     :evidence/author "zai-1" :evidence/at "2026-09-25T01:00:01Z"
                     :evidence/tags [:memory :test-registry]
                     :evidence/body {:kind "project" :name "a note" :body "text"}})
           _ (is (:ok receipt) (pr-str receipt))
           ledger (str (io/file (:artifact-dir options) "namespaces.ednlog"))
           built (registry/build-namespace-ledger!
                  backend {:namespace-ledger-file ledger :page-size 5})]
       (is (true? (:complete? built)))
       (is (= 2 (:scanned built)))
       (is (= [{:evidence/id "e-00000000-0000-4000-8000-000000000001"
                :evidence/type :memory :evidence/author "zai-1"}]
              (:foreign built)))
       (is (nil? (:undecodable built)))
       (let [none (registry/latest-run-for-namespace
                   backend {:namespace "futon3c.not-registered-test"
                            :namespace-ledger-file ledger})]
         (is (= :no-run-for-namespace (:reason none)))
         (is (= :namespace-ledger (:resolved-by none))))))))

(deftest an-incomplete-namespace-ledger-refuses-absence
  ;; a build whose second page fails records :complete? false carrying the
  ;; typed failure — never a silent short scan — and absence keeps refusing
  ;; with that failure named
  (fixture
   (fn [{:keys [backend options]}]
     (dotimes [i 12]
       (append-run! backend {:namespace (str "paged-" i "-test")
                             :ran-at (format "2026-09-25T01:00:%02dZ" i)}))
     (let [ledger (str (io/file (:artifact-dir options) "namespaces.ednlog"))
           paged (fail-on-paged-backend backend)
           built (registry/build-namespace-ledger!
                  paged {:namespace-ledger-file ledger :page-size 5})]
       (is (false? (:complete? built)))
       (is (= 5 (:scanned built)) "only the first page was read")
       (is (= :timeout (get-in built [:read-error :error/kind])))
       (let [none (registry/latest-run-for-namespace
                   paged {:namespace "futon3c.not-registered-test"
                          :namespace-ledger-file ledger})]
         (is (= :none (:status none)))
         (is (= :scan-window-exhausted (:reason none)))
         (is (= :namespace-ledger-incomplete (:resolved-by none)))
         (is (= 5 (:scanned none)))
         (is (= :timeout (get-in none [:read-error :error/kind]))))))))

(deftest a-ledger-hit-fetches-one-bounded-fill-page-never-a-scan
  ;; the lookup answers within the client's timeout: one SMALL fill-forward
  ;; read bounded by :query/since <the ledger's newest :ran-at> plus one
  ;; targeted read for the record itself — never an unbounded evidence page
  ;; (2026-09-25, live: a full page times the 5s client out, and the
  ;; endpoint answers a ledger hit in ~4s)
  (fixture
   (fn [{:keys [backend options]}]
     (let [ledger (str (io/file (:artifact-dir options) "namespaces.ednlog"))
           run (registry/register-run! backend (assoc options :namespace-ledger-file ledger))
           real-get-entry* store/get-entry*
           queries (atom []) reads (atom 0)]
       (with-redefs [store/query* (fn [_ q] (swap! queries conj q) [])
                     store/count* (fn [_ _] (swap! queries conj :count) 0)
                     store/get-entry* (fn [b id] (swap! reads inc) (real-get-entry* b id))]
         (let [found (registry/latest-run-for-namespace
                      backend {:namespace "demo-test" :namespace-ledger-file ledger})]
           (is (= (:evidence/id run) (:evidence/id found)))
           (is (= :namespace-ledger (:resolved-by found)))
           (is (= 1 (count @queries)) "one bounded fill read, no scan")
           (is (= (get-in run [:payload :ran-at]) (:query/since (first @queries)))
               "the fill reads only what registered after the ledger's newest :ran-at")
           (is (= registry/default-namespace-fill-limit (:query/limit (first @queries))))
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

;; ---------------------------------------------------------------------------
;; E-kimi-task-19: lookup by COMMAND. A mission's VERIFY exit is a gate command
;; (bb, sh, lake build) naming no -n namespace, so latest-run-for-namespace can
;; never observe it; latest-run-for-command keys on the exact recorded logical
;; command vector.

(deftest latest-run-for-command-finds-a-gate-run-no-namespace-can-see
  (fixture
   (fn [{:keys [backend]}]
     (let [run (append-run! backend {:command ["bb" "scripts/gates.clj"]
                                     :ran-at "2026-09-25T01:00:00Z"})
           found (registry/latest-run-for-command
                  backend {:command ["bb" "scripts/gates.clj"]})]
       (is (= (:evidence/id run) (:evidence/id found)))
       (is (= ["bb" "scripts/gates.clj"] (get-in found [:payload :command])))
       ;; the ledger reduce indexes it under its command key, not a namespace
       (is (= (registry/command-key ["bb" "scripts/gates.clj"])
              (registry/command-key (get-in found [:payload :command]))))
       ;; ...and no namespace lookup can ever find it
       (doseq [ns ["bb" "scripts/gates.clj" "gates" "demo-test"]]
         (let [none (registry/latest-run-for-namespace backend {:namespace ns})]
           (is (= :none (:status none)))
           (is (= :no-run-for-namespace (:reason none)))))))))

(deftest latest-run-for-command-resolves-to-the-newer-of-two-runs
  (fixture
   (fn [{:keys [backend]}]
     (let [older (append-run! backend {:command ["sh" "scripts/verify.sh"]
                                       :ran-at "2026-09-25T01:00:00Z"})
           newer (append-run! backend {:command ["sh" "scripts/verify.sh"]
                                       :ran-at "2026-09-25T02:00:00Z"
                                       :failures 1 :warrant? false})
           found (registry/latest-run-for-command
                  backend {:command ["sh" "scripts/verify.sh"]})]
       (is (true? (get-in older [:payload :warrant?])))
       (is (false? (get-in newer [:payload :warrant?])))
       (is (= (:evidence/id newer) (:evidence/id found)))
       (is (= 1 (get-in found [:payload :results :failures])))))))

(deftest a-command-differing-by-one-argument-is-a-different-key
  (fixture
   (fn [{:keys [backend]}]
     (append-run! backend {:command ["bb" "scripts/gates.clj" "--fast"]
                           :ran-at "2026-09-25T01:00:00Z"})
     (let [none (registry/latest-run-for-command
                 backend {:command ["bb" "scripts/gates.clj"]})]
       (is (= :none (:status none)))
       (is (= :no-run-for-command (:reason none)))
       ;; absence is only claimed because the scan saw every registry entry
       (is (= (:scanned none) (:registry-entries none))))
     ;; and the exact command IS found
     (let [found (registry/latest-run-for-command
                  backend {:command ["bb" "scripts/gates.clj" "--fast"]})]
       (is (some? (:evidence/id found))))
     ;; a command is required: without one there is nothing to look up
     (is (= :command-required (:reason (registry/latest-run-for-command backend {}))))
     (is (= :command-required (:reason (registry/latest-run-for-command backend {:command []})))))))

(deftest command-ledger-fills-forward-and-answers-gate-runs
  ;; a registered gate run is ledgered under its command key at registration,
  ;; and the lookup answers from the ledger without a store scan
  (fixture
   (fn [{:keys [backend options]}]
     (let [ledger (str (io/file (:artifact-dir options) "commands.ednlog"))
           run (append-run! backend {:command ["lake" "build" "Demo.Gates"]
                                     :ran-at "2026-09-25T01:00:00Z"})
           _ (registry/record-namespace-run! {:namespace-ledger-file ledger} run)
           found (registry/latest-run-for-command
                  backend {:command ["lake" "build" "Demo.Gates"]
                           :namespace-ledger-file ledger})]
       (is (= (:evidence/id run) (:evidence/id found)))
       (is (= :namespace-ledger (:resolved-by found)))))))

(deftest command-ledger-absence-needs-a-command-keyed-build
  ;; a ledger built whole by THIS build concludes command absence; a legacy
  ;; build (no :keying :command marker) indexed only namespaces and must
  ;; refuse to conclude command absence
  (fixture
   (fn [{:keys [backend options]}]
     (append-run! backend {:command ["bb" "scripts/gates.clj"]
                           :ran-at "2026-09-25T01:00:00Z"})
     (let [ledger (str (io/file (:artifact-dir options) "commands.ednlog"))
           built (registry/build-namespace-ledger!
                  backend {:namespace-ledger-file ledger})]
       (is (:complete? built))
       (is (= :command (:keying built)))
       (let [none (registry/latest-run-for-command
                   backend {:command ["bb" "scripts/other.clj"]
                            :namespace-ledger-file ledger})]
         (is (= :no-run-for-command (:reason none)))
         (is (= :namespace-ledger (:resolved-by none)))))
     ;; a legacy ledger: namespace-keyed entries, a complete marker with no
     ;; :keying — command absence is NOT established. The fill-forward still
     ;; brings a PRESENT gate run into the ledger: presence never depends on
     ;; the marker's keying.
     (let [legacy (str (io/file (:artifact-dir options) "legacy.ednlog"))]
       (spit legacy
             (str "{:entry/type :namespace-run :namespace \"demo-test\" "
                  ":entry-id \"e-old\" :ran-at \"2026-09-25T01:00:00Z\"}\n"
                  "{:entry/type :namespace-ledger-built :scanned 2 "
                  ":registry-entries 2 :complete? true :at \"2026-09-25T01:00:00Z\"}\n"))
       (let [found (registry/latest-run-for-command
                    backend {:command ["bb" "scripts/gates.clj"]
                             :namespace-ledger-file legacy})]
         (is (= :namespace-ledger (:resolved-by found))
             "the fill-forward ledgered the gate run the legacy build could not key"))
       (let [none (registry/latest-run-for-command
                   backend {:command ["bb" "scripts/other.clj"]
                            :namespace-ledger-file legacy})]
         (is (= :scan-window-exhausted (:reason none)))
         (is (= :namespace-ledger-incomplete (:resolved-by none))))
       ;; ...while the legacy ledger still answers namespace lookups as before
       (let [none (registry/latest-run-for-namespace
                   backend {:namespace "unregistered-test"
                            :namespace-ledger-file legacy})]
         (is (= :no-run-for-namespace (:reason none))))))))

(deftest latest-run-for-namespace-still-wraps-the-same-ledger
  ;; the namespace lookup is a thin wrapper over the same command-keyed
  ;; ledger: one build serves both lookups
  (fixture
   (fn [{:keys [backend options]}]
     (registry/register-run! backend options)
     (let [ledger (str (io/file (:artifact-dir options) "shared.ednlog"))
           _ (registry/build-namespace-ledger!
              backend {:namespace-ledger-file ledger})
           by-ns (registry/latest-run-for-namespace
                  backend {:namespace "demo-test" :namespace-ledger-file ledger})
           by-cmd (registry/latest-run-for-command
                   backend {:command ["clojure" "-M:test" "-n" "demo-test"]
                            :namespace-ledger-file ledger})]
       (is (= (:evidence/id by-ns) (:evidence/id by-cmd)))
       (is (= :namespace-ledger (:resolved-by by-ns)))
       (is (= :namespace-ledger (:resolved-by by-cmd)))))))

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

;; ---------------------------------------------------------------------------
;; E-kimi-task-30: the live lookup fills the ledger forward from the store
;; before answering, so a registration that never reached the ledger at
;; registration (a --pinned worktree's discarded data dir; the mis-resolved
;; src/ default of 2026-09-25) still reaches the live lookup -- and the CLI
;; default resolves to the canonical checkout, never a worktree.

(defn- recording-backend
  "Wraps backend: every -query passes through while the query map is
  recorded, so a test can assert the fill-forward's :query/since."
  [backend queries]
  (reify futon3c.evidence.backend/EvidenceBackend
    (-append [_ _] nil)
    (-get [_ id] (store/get-entry* backend id))
    (-exists? [_ id] (some? (store/get-entry* backend id)))
    (-query [_ q] (swap! queries conj q) (store/query* backend q))
    (-count [_ q] (store/count* backend q))
    (-forks-of [_ _] [])
    (-delete! [_ _] {:compacted 0})
    (-all [_] [])))

(defn- fail-on-fill-backend
  "Wraps backend: the build's full scan (a :query/since of epoch) reads fine;
  the fill's since-bounded read returns the AR-43 typed read-failed map."
  [backend]
  (reify futon3c.evidence.backend/EvidenceBackend
    (-append [_ _] nil)
    (-get [_ _] nil)
    (-exists? [_ _] false)
    (-query [_ q] (if (= "1970-01-01T00:00:00Z" (:query/since q))
                    (store/query* backend q)
                    {:error/component :E-store :error/code :read-failed
                     :error/kind :timeout :status nil}))
    (-count [_ q] (store/count* backend q))
    (-forks-of [_ _] [])
    (-delete! [_ _] {:compacted 0})
    (-all [_] [])))

(deftest a-lookup-fills-the-ledger-forward-from-the-store
  ;; a run registered AFTER the ledger build, straight into the store, is
  ;; found by command: the lookup fills the ledger forward before answering,
  ;; the fill's read is bounded by the ledger's newest :ran-at, and the
  ;; ledger file records both the run and the fill itself
  (fixture
   (fn [{:keys [backend options]}]
     (append-run! backend {:namespace "base-test"
                           :command ["clojure" "-M:test" "-n" "base-test"]
                           :ran-at "2026-09-25T01:00:00Z"})
     (let [ledger (str (io/file (:artifact-dir options) "namespaces.ednlog"))
           built (registry/build-namespace-ledger! backend {:namespace-ledger-file ledger})
           _ (is (true? (:complete? built)))
           late (append-run! backend {:command ["lake" "build" "Demo.Module"]
                                      :ran-at "2026-09-25T02:00:00Z"})
           queries (atom [])
           found (registry/latest-run-for-command
                  (recording-backend backend queries)
                  {:command ["lake" "build" "Demo.Module"]
                   :namespace-ledger-file ledger})]
       (is (= (:evidence/id late) (:evidence/id found)))
       (is (= :namespace-ledger (:resolved-by found)))
       (is (= ["2026-09-25T01:00:00Z"] (mapv :query/since @queries))
           "the fill read only what registered after the ledger's newest :ran-at")
       (let [{:keys [runs fills]} (registry/namespace-ledger ledger)]
         (is (= (:evidence/id late)
                (get-in runs [(registry/command-key ["lake" "build" "Demo.Module"]) :entry-id]))
             "the ledger file now contains the late run")
         (is (= 1 (count fills)))
         (is (= "2026-09-25T01:00:00Z" (:since (first fills))))
         (is (= 1 (:appended (first fills)))))
       ;; a second lookup fills nothing further: dedupe by entry-id
       (let [again (registry/latest-run-for-command
                    backend {:command ["lake" "build" "Demo.Module"]
                             :namespace-ledger-file ledger})]
         (is (= (:evidence/id late) (:evidence/id again)))
         (is (= 1 (count (:fills (registry/namespace-ledger ledger))))))))))

(deftest fill-forward-reads-from-the-ledger-watermark-not-its-newest-run
  ;; the case the live ledger presented on 2026-09-25: after the build, a
  ;; registration ledgers a NEWER run directly (record-namespace-run!) while
  ;; an OLDER run registered in between reached only the store (ledgered
  ;; into a mis-resolved file). A fill reading forward from the ledger's
  ;; newest run entry starts past the stranded run and never finds it; the
  ;; fill must read from the point the ledger is known whole through — the
  ;; build's :whole-through — and the stranded run is found
  (fixture
   (fn [{:keys [backend options]}]
     (append-run! backend {:namespace "base-test"
                           :command ["clojure" "-M:test" "-n" "base-test"]
                           :ran-at "2026-09-25T01:00:00Z"})
     (let [ledger (str (io/file (:artifact-dir options) "namespaces.ednlog"))
           built (registry/build-namespace-ledger! backend {:namespace-ledger-file ledger})
           _ (is (true? (:complete? built)))
           _ (is (= "2026-09-25T01:00:00Z" (:whole-through built))
                 "the build marker records the newest :ran-at the scan saw")
           stranded (append-run! backend {:command ["lake" "build" "Demo.Module"]
                                          :ran-at "2026-09-25T02:00:00Z"})
           direct (append-run! backend {:namespace "later-test"
                                        :command ["clojure" "-M:test" "-n" "later-test"]
                                        :ran-at "2026-09-25T03:00:00Z"})
           _ (registry/record-namespace-run! {:namespace-ledger-file ledger} direct)
           queries (atom [])
           found (registry/latest-run-for-command
                  (recording-backend backend queries)
                  {:command ["lake" "build" "Demo.Module"]
                   :namespace-ledger-file ledger})]
       (is (= ["2026-09-25T01:00:00Z"] (mapv :query/since @queries))
           "the fill reads from the build's watermark, not the directly ledgered 03:00 run")
       (is (= (:evidence/id stranded) (:evidence/id found)))
       (is (= :namespace-ledger (:resolved-by found)))
       (let [{:keys [fills]} (registry/namespace-ledger ledger)
             fill (first fills)]
         (is (= 1 (count fills)))
         (is (= 1 (:appended fill)) "the stranded run, not the already-ledgered direct one")
         (is (= [(:evidence/id stranded)] (:entry-ids fill)))
         (is (= "2026-09-25T03:00:00Z" (:whole-through fill))
             "the next fill reads forward from the newest :ran-at this read saw"))
       ;; and the next lookup reads forward from the fill's watermark
       (let [queries2 (atom [])
             _ (registry/latest-run-for-command
                (recording-backend backend queries2)
                {:command ["lake" "build" "Demo.Module"]
                 :namespace-ledger-file ledger})]
         (is (= ["2026-09-25T03:00:00Z"] (mapv :query/since @queries2))))))))

(deftest fill-forward-over-a-failed-read-refuses-and-appends-nothing
  ;; AR-43 in the fill: the typed read-failed map surfaces as
  ;; :registry-read-failed -- never as "nothing new" -- and the ledger file
  ;; is untouched
  (fixture
   (fn [{:keys [backend options]}]
     (append-run! backend {:namespace "base-test"
                           :command ["clojure" "-M:test" "-n" "base-test"]
                           :ran-at "2026-09-25T01:00:00Z"})
     (let [ledger (str (io/file (:artifact-dir options) "namespaces.ednlog"))
           built (registry/build-namespace-ledger! backend {:namespace-ledger-file ledger})
           _ (is (true? (:complete? built)))
           before (slurp ledger)
           none (registry/latest-run-for-command
                 (fail-on-fill-backend backend)
                 {:command ["lake" "build" "Demo.Module"]
                  :namespace-ledger-file ledger})]
       (is (= :none (:status none)))
       (is (= :registry-read-failed (:reason none)))
       (is (= :timeout (get-in none [:read-error :error/kind])))
       (is (= before (slurp ledger)) "a failed fill appends nothing")))))

(deftest a-worktree-cli-resolves-the-canonical-ledger-path
  ;; a --pinned registration runs the CLI from a sibling worktree; with the
  ;; local :7070 agency-url the ledger resolves to the CANONICAL checkout's
  ;; data dir -- a worktree is never the canonical checkout -- so the live
  ;; lookup reads what the worktree registered
  (with-redefs-fn {#'registry/futon3c-checkout-root (fn [] "/tmp/wt-warrant-abcd1234")}
    (fn []
      (is (= (str (io/file "/tmp/canonical" "data" "test-registry" "namespace-ledger.edn"))
             (registry/namespace-ledger-path
              {:agency-url "http://localhost:7070"
               :canonical-futon3c-root "/tmp/canonical"})))))
  ;; the explicit option still wins over the canonical default
  (is (= "/tmp/explicit/namespaces.ednlog"
         (registry/namespace-ledger-path
          {:namespace-ledger-file "/tmp/explicit/namespaces.ednlog"
           :agency-url "http://localhost:7070"
           :canonical-futon3c-root "/tmp/canonical"})))
  ;; a non-local agency-url keeps the checkout-relative default
  (is (= (str (io/file "/tmp/some-checkout" "data" "test-registry" "namespace-ledger.edn"))
         (registry/namespace-ledger-path
          {:futon3c-root "/tmp/some-checkout"
           :agency-url "http://elsewhere:9999"}))))

;; WARRANT-PREFIX-I: classpath entries under repo-root are recorded repo-relative,
;; so a warrant registered from a --pinned worktree checks from the canonical
;; checkout of the same commit.

(defn- temp-dir! [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- fingerprint-repo!
  "A minimal deps.edn repo with src/ and test/ on the :test classpath, plus
  OUTSIDE (an absolute directory outside the repo)."
  [outside src-text]
  (let [root (temp-dir! "fingerprint-repo-")]
    (io/make-parents (io/file root "src/demo.clj"))
    (spit (io/file root "src/demo.clj") src-text)
    (io/make-parents (io/file root "test/demo_test.clj"))
    (spit (io/file root "test/demo_test.clj") "(ns demo-test)")
    (spit (io/file root "deps.edn")
          (pr-str {:paths ["src"]
                   :aliases {:test {:extra-paths ["test" (str outside)]}}}))
    root))

(deftest classpath-entries-under-repo-root-are-relative
  (let [outside (temp-dir! "fingerprint-outside-")
        a (fingerprint-repo! outside "(ns demo)")
        b (fingerprint-repo! outside "(ns demo)")
        fp (fn [root] (#'registry/fingerprint* {:repo-root (str root) :command ["clojure" "-M:test"]}))
        fa (fp a) fb (fp b)
        paths (set (map :path (:dependencies fa)))]
    (testing "rel-1: src and test are recorded as repo-relative paths"
      (is (contains? paths "src"))
      (is (contains? paths "test"))
      (is (not-any? #(str/starts-with? % (str (.getCanonicalFile a))) paths)))
    (testing "rel-2: the same content at two roots fingerprints equal"
      (is (= (:sha256 fa) (:sha256 fb)))
      (is (= fa fb)))
    (testing "abs-1: an entry outside repo-root stays absolute"
      (is (contains? paths (str (.getCanonicalFile outside))))
      (is (= :load-closure-only
             (:sha256 (first (filter #(= (str (.getCanonicalFile outside)) (:path %))
                                     (:dependencies fa)))))))))

(deftest relative-classpath-does-not-weaken-the-closure-pin
  (testing "bad case: a worktree whose src CONTENT differs still refuses via the closure"
    (let [outside (temp-dir! "fingerprint-outside-")
          worktree (fingerprint-repo! outside "(ns demo)")
          canonical (fingerprint-repo! outside "(ns demo) (def changed 1)")
          url (str (.toURL (.toURI (io/file worktree "src/demo.clj"))))
          recorded (registry/closure-from-entries [{:ns "demo" :url url}] (str worktree))]
      (is (= ["src/demo.clj"] (mapv :path recorded)))
      (is (= ["src/demo.clj"]
             (registry/closure-diff (registry/closure-shas recorded)
                                    (registry/current-closure-shas (str canonical) recorded)))))))
