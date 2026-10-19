(ns futon3c.wm.meta-self-heal-adapters-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell]
            [clojure.string :as str]
            [futon2.aif.meta-repair-coordinator :as coordinator]
            [futon2.aif.meta-self-heal-harness :as harness]
            [futon3c.agency.registry :as reg]
            [futon3c.wm.meta-self-heal-adapters :as sut]))

(def ready-agent
  {:agent/status :idle :agent/invoke-ready? true :agent/session-id nil})

(deftest readiness-is-read-only-and-names-concrete-authorities
  (let [root (.toString (java.nio.file.Files/createTempDirectory
                         "meta-adapters" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (clojure.java.shell/sh "git" "-C" root "init" "-q")
    (clojure.java.shell/sh "git" "-C" root "config" "user.email" "wm@test")
    (clojure.java.shell/sh "git" "-C" root "config" "user.name" "WM")
    (spit (str root "/seed") "seed")
    (clojure.java.shell/sh "git" "-C" root "add" "seed")
    (clojure.java.shell/sh "git" "-C" root "commit" "-qm" "seed")
    (with-redefs [reg/get-agent (fn [_] ready-agent)]
      (let [queued (atom [])
            result (sut/readiness
                    {:target-repository root
                     :meta-injury-ledger-path "/tmp/ledger.edn"
                     :meta-repair-transaction-path "/tmp/tx.edn"
                     :queue-retry-fn #(swap! queued conj %)})]
        (is (= :ready (:status result)))
        (is (= :futon3c.agency.registry/invoke-agent! (:dispatch result)))
        (is (= :queued-after-parent-release (:retry result)))
        (is (empty? @queued))))))

(deftest restored-string-status-is-ready-after-serving-jvm-restart
  (let [result (with-redefs [reg/get-agent
                             (fn [_] (assoc ready-agent :agent/status "restored"))
                             clojure.java.shell/sh
                             (fn [& args]
                               {:exit 0
                                :out (if (some #{"status"} args) "" "clean-head\n")
                                :err ""})]
                 (sut/readiness
                  {:target-repository "/home/joe/code/futon2"
                   :meta-injury-ledger-path "/tmp/ledger.edn"
                   :meta-repair-transaction-path "/tmp/tx.edn"
                   :queue-retry-fn identity}))]
    (is (= :ready (:status result)))
    (is (empty? (:defects result)))))

(deftest restored-stored-agent-uses-registered-local-invoke-authority
  (let [result (with-redefs [reg/get-agent
                             (fn [_] {:agent/status :restored
                                      :agent/invoke-fn (fn [& _])})
                             clojure.java.shell/sh
                             (fn [& args]
                               {:exit 0
                                :out (if (some #{"status"} args) "" "clean-head\n")
                                :err ""})]
                 (sut/readiness
                  {:target-repository "/home/joe/code/futon2"
                   :meta-injury-ledger-path "/tmp/ledger.edn"
                   :meta-repair-transaction-path "/tmp/tx.edn"
                   :queue-retry-fn identity}))]
    (is (= :ready (:status result)))
    (is (empty? (:defects result)))))

(deftest missing-queue-or-unready-seat-refuses-without-effects
  (with-redefs [reg/get-agent (fn [seat]
                                (assoc ready-agent :agent/status
                                       (if (= seat "wm-reviewer") :invoking :idle)))]
    (let [result (sut/readiness
                  {:target-repository "/home/joe/code/futon2"
                   :meta-injury-ledger-path "/tmp/ledger.edn"
                   :meta-repair-transaction-path "/tmp/tx.edn"})]
      (is (= :refused (:status result)))
      (is (some #{:cast-not-ready} (:defects result)))
      (is (some #{:queued-retry-authority} (:defects result))))))

(deftest agency-adapter-requires-one-machine-readable-map
  (testing "valid EDN is carried with the actual session"
    (with-redefs [reg/reset-session! (fn [_] {:ok true :old-session-id "old-session"})
                  reg/invoke-agent!
                  (fn [_ _ _]
                    {:ok true :result "{:status :succeeded :payload {:patch-sha \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"}}"
                     :session-id "fresh-session" :invoke-meta {:job-id "job-1"}})]
      (is (= {:status :succeeded
              :payload {:patch-sha "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}
              :session-id "fresh-session" :job-id "job-1"}
             (sut/agency-invoke! {:seat "wm-author" :session-id "declared"
                                  :request {:phase :author}})))))
  (testing "prose cannot become repair evidence"
    (with-redefs [reg/reset-session! (fn [_] {:ok true})
                  reg/invoke-agent! (fn [& _] {:ok true :result "done" :session-id "fresh"})]
      (is (= :agency-response-not-one-edn-map
             (get-in (sut/agency-invoke! {:seat "wm-author" :request {}})
                     [:payload :reason]))))))

(deftest acceptance-run-is-bounded-pinned-and-shell-free
  "The exit, output, and pin assertions exercise a real systemd transient unit
   through bounded_test_job.py. Grammar refusals are pure and launch no unit."
  (let [root (.toString (java.nio.file.Files/createTempDirectory
                         "acceptance-repo" (make-array java.nio.file.attribute.FileAttribute 0)))
        logs (.toString (java.nio.file.Files/createTempDirectory
                         "acceptance-logs" (make-array java.nio.file.attribute.FileAttribute 0)))
        port (fn [timeout-ms request]
               (#'sut/run-acceptance! timeout-ms request))]
    (spit (io/file root ".gitignore") ".cpcache/\n")
    (spit (io/file root "deps.edn")
          "{:paths [\"test\"] :aliases {:test {:extra-deps {io.github.cognitect-labs/test-runner {:git/url \"https://github.com/cognitect-labs/test-runner.git\" :sha \"5e91ee0989d93115cfb5e15b6f242f5ec7ab762e\"}}}}}\n")
    (doseq [args [["init" "-q"] ["config" "user.email" "wm@test"]
                  ["config" "user.name" "WM"] ["add" "."]
                  ["commit" "-qm" "fixture"]]]
      (apply clojure.java.shell/sh "git" "-C" root args))
    (binding [sut/*acceptance-repository* root
              sut/*acceptance-runs-root* logs]
      (testing "nonzero exit and full-output pin"
        (let [source-text "(ns fixture.acceptance-test (:require [clojure.test :refer [deftest is]]))\n(deftest fails (println \"ok\") (is false))\n"
              source {:namespace "fixture.acceptance-test"
                      :path "test/fixture/acceptance_test.clj"
                      :text source-text}
              materialized (io/file root (:path source))
              result (port 60000 {:id "fails" :command ["clojure" "-M:test" "-n"
                                                          "fixture.acceptance-test"]
                                  :source source})
              path (get-in result [:run-pin :path])
              bytes (when path (.getBytes (slurp path) "UTF-8"))]
          (is (= [:ran :failed 1] ((juxt :status :outcome :exit) result)))
          (is (true? (:source-clean? result)))
          (is (not (.exists materialized)))
          (is (str/includes? (:summary result) "1 failures, 0 errors"))
          ;; The production root is futon2/data/wm-meta-injuries/acceptance-runs;
          ;; this binding proves the hermetic test redirects every written log.
          (is (.startsWith path logs))
          (is (= (#'sut/hex-sha256 bytes) (get-in result [:run-pin :sha256])))
          ;; The failed transient unit must not be retained: a pinning test
          ;; exits nonzero on every real admission, and a retained unit per
          ;; attempt is a leak (codex-2, review of 21057475f).
          (let [units (:out (clojure.java.shell/sh "systemctl" "--user" "list-units"
                                                   "--all" "--plain" "--no-pager"
                                                   "wm-acceptance-fails-*"))]
            (is (not (str/includes? units "wm-acceptance-fails-"))
                (str "retained unit(s):\n" units)))))
      (testing "last nonblank output line is intact"
        (is (= "ok" (#'sut/acceptance-summary "\n ok\n\n"))))
      (testing "a compile error is the summary, not the runner's correlation line (F14)"
        (is (= "Syntax error compiling at (futon2/aif/x_test.clj:40:19)."
               (#'sut/acceptance-summary
                (str "Syntax error compiling at (futon2/aif/x_test.clj:40:19).\n"
                     "Unable to resolve namespace: x-test\n\n"
                     "bounded-test failure/resource correlation: command-exit=1 resource-status=:clean\n"))))
        (is (= "ok" (#'sut/acceptance-summary
                     "ok\nbounded-test failure/resource correlation: command-exit=1\n"))))
      (testing "only clojure.test namespace/optional-var commands are admitted"
        (doseq [command [["clojure" "-M:test" "-e" "(println 1)"]
                         ["clojure" "-M:test" "-n" "bad namespace"]]]
          (is (= {:status :failed
                  :reason :meta-self-heal/acceptance-command-not-admissible
                  :command command}
                 (port 1000 {:id "unsafe" :command command
                             :source {:namespace "fixture.acceptance-test"
                                      :path "test/fixture/acceptance_test.clj"
                                      :text "source"}})))))
      (testing "an existing source is refused and untouched"
        (let [file (io/file root "test/fixture/existing_test.clj")
              original (.getBytes "original bytes\n" "UTF-8")
              _ (.mkdirs (.getParentFile file))
              _ (with-open [out (io/output-stream file)] (.write out original))
              result (port 1000 {:id "existing"
                                 :command ["clojure" "-M:test" "-n" "fixture.existing-test"]
                                 :source {:namespace "fixture.existing-test"
                                          :path "test/fixture/existing_test.clj"
                                          :text "replacement"}})]
          (is (= :meta-self-heal/acceptance-source-path-refused (:reason result)))
          (is (= (seq original) (seq (.getBytes (slurp file) "UTF-8"))))))
      (testing "a namespace-derived path outside test is refused"
        (is (= :meta-self-heal/acceptance-source-path-refused
               (:reason (port 1000 {:id "escape"
                                    :command ["clojure" "-M:test" "-n" "../x"]
                                    :source {:namespace "../x"
                                             :path "../x.clj" :text "bad"}})))))
      (testing "timeout removes materialized source"
        (let [file (io/file root "test/fixture/slow_test.clj")
              result (port 200 {:id "slow"
                                :command ["clojure" "-M:test" "-n" "fixture.slow-test"]
                                :source {:namespace "fixture.slow-test"
                                         :path "test/fixture/slow_test.clj"
                                         :text "(ns fixture.slow-test (:require [clojure.test :refer [deftest]]))\n(deftest slow (Thread/sleep 30000))\n"}})]
          (is (= [:ran :errored -1 true]
                 ((juxt :status :outcome :exit :source-clean?) result)))
          (is (not (.exists file)))))
      (testing "an absolute :path is refused even when the namespace is sound"
        (let [derived (io/file root "test/fixture/abs_test.clj")
              result (port 1000 {:id "abs"
                                 :command ["clojure" "-M:test" "-n" "fixture.abs-test"]
                                 :source {:namespace "fixture.abs-test"
                                          :path "/tmp/elsewhere/abs_test.clj"
                                          :text "bad"}})]
          (is (= :meta-self-heal/acceptance-source-path-refused (:reason result)))
          (is (not (.exists derived)))
          (is (not (.exists (io/file "/tmp/elsewhere/abs_test.clj"))))))
      (testing "launch failure still removes the source and the directories it created"
        (let [deep-dir (io/file root "test/fixture/deep")
              file (io/file deep-dir "nested_test.clj")
              result (with-redefs [sut/launch-bounded-acceptance!
                                   (fn [_ _ _] (throw (ex-info "launcher down" {})))]
                       (port 1000 {:id "deep"
                                   :command ["clojure" "-M:test" "-n" "fixture.deep.nested-test"]
                                   :source {:namespace "fixture.deep.nested-test"
                                            :path "test/fixture/deep/nested_test.clj"
                                            :text "(ns fixture.deep.nested-test)\n"}}))]
          (is (= [:failed :acceptance-runner-error true]
                 ((juxt :status :reason :source-clean?) result)))
          (is (not (.exists file)))
          (is (not (.exists deep-dir)) "the adapter removes the directory it created")
          (is (.isDirectory (io/file root "test/fixture"))
              "a directory that existed before is left alone"))))))

(deftest executed-command-inserts-the-test-runner-main
  "F13: futon2's :test alias has no :main-opts; the fixture repo above now
   mirrors that shape, so the real-unit test exercises this composition."
  (is (= ["clojure" "-M:test" "-m" "cognitect.test-runner" "-n" "a.b-test"]
         (sut/executed-command ["clojure" "-M:test" "-n" "a.b-test"])))
  (is (= ["clojure" "-M:test" "-m" "cognitect.test-runner" "-n" "a.b-test" "-v" "a.b-test/one"]
         (sut/executed-command ["clojure" "-M:test" "-n" "a.b-test" "-v" "a.b-test/one"]))))

(deftest acceptance-review-prompt-describes-pre-repair-result
  (let [prompt (#'sut/producer-prompt :acceptance-review {:result {:status :ran}})]
    (is (str/includes? prompt "run before any repair, in the canonical checkout"))
    (is (str/includes? prompt "result is in :result"))
    (is (str/includes? prompt "fails for the injury's reason"))
    (is (str/includes? prompt "Return exactly one EDN map and no prose"))))

(deftest acceptance-proposal-prompt-requires-complete-source
  (let [prompt (#'sut/producer-prompt :acceptance-proposal {:diagnosis {:status :present}})]
    (is (str/includes? prompt ":source {:namespace"))
    (is (str/includes? prompt "complete clojure.test file"))
    (is (str/includes? prompt "real production functions"))
    (is (str/includes? prompt "with no stubs"))
    (is (str/includes? prompt ":production-functions"))))

(deftest diagnosis-prompt-requires-production-functions
  ;; F11b, click wm-click-fca1c921
  (let [prompt (#'sut/producer-prompt :diagnosis {:injury {:id "injury/x"}})]
    (doseq [s [":classification" ":primary-cause" ":production-functions"
               ":selection-entrypoint" ":required-loop-node-evaluator"
               ":terminal-routing-entrypoint"
               "{:role" ":var" ":file" "repo-relative"
               "/home/joe/code/futon2"
               "found by reading" "read-only shell commands" "nothing that runs code or writes"
               "typed refusal" "never invented"
               "Do not select ordinary work" "claim a repair"
               "Return exactly one EDN map and no prose"
               "Bind every returned identity and pin to this request"]]
      (is (str/includes? prompt s) s))))

(deftest work-order-identities-complete-the-production-work-order
  "Click wm-click-c78d0f27 (2026-10-19): the service forwarded only port
   functions, so coordinator/prepare saw no sessions and no worktree and
   refused :work-order/invalid on every self-heal click. With the runtime's
   two inputs (diagnosis pin, acceptance test) plus the identities the
   adapter now supplies, the real prepare succeeds; without the identities it
   refuses exactly as the click did."
  (let [root (.toString (java.nio.file.Files/createTempDirectory
                         "meta-adapters-identities"
                         (make-array java.nio.file.attribute.FileAttribute 0)))
        ;; the harness reads its campaign fixture relative to futon2's root
        env (with-redefs [harness/campaign-cause
                          (fn []
                            (->> (edn/read-string
                                  (slurp (str "/home/joe/code/futon2/test/fixtures/"
                                              "meta-self-heal/campaign-2026-10-19.edn")))
                                 (filter #(= :fold-output-invalid
                                             (get-in % [:record :failure :kind])))
                                 first))]
              (harness/prepare-environment! root))
        runtime-inputs (select-keys (harness/work-order-options env)
                                    [:diagnosis-evidence-pin :acceptance-test])
        ready {:cast sut/default-cast :repository (:repo env)}
        identities (sut/work-order-identities ready)
        again (sut/work-order-identities ready)
        sessions (map identities [:author-session :reviewer-session
                                  :repair-reviewer-session])
        prepared (coordinator/prepare (:durable env) (merge runtime-inputs identities))
        refused (coordinator/prepare (:durable env) runtime-inputs)]
    (is (= :prepared (:status prepared)))
    (is (= :valid (:status (coordinator/verify-work-order (:work-order prepared)))))
    (is (= sut/default-cast (select-keys identities [:author :reviewer :repair-reviewer])))
    (is (= 3 (count (set sessions))) "one distinct claim session per seat")
    (is (every? (fn [[session-key role]]
                  (str/starts-with? (identities session-key)
                                    (str (get sut/default-cast role) "/")))
                {:author-session :author :reviewer-session :reviewer
                 :repair-reviewer-session :repair-reviewer})
        "each claim session is named under its seat")
    (is (not= sessions (map again [:author-session :reviewer-session
                                   :repair-reviewer-session]))
        "sessions are fresh per call, so a later claim never reuses them")
    (is (= (:repo env) (:target-repository identities) (:target-worktree identities)))
    (is (= :refused (:status refused)))
    (is (= [:work-order/invalid] (:defects refused)))))

(deftest producer-invoke-retries-once-after-a-provider-rate-limit
  "Clicks wm-click-36b47627 and wm-click-fdc08aea (2026-10-19): each was spent
   on a 429 at the review invoke seconds after the author invoke, while the
   same seat answered a probe three minutes later. One retry after a backoff;
   any other failure is returned as it is; a second 429 is the failure."
  (let [calls (atom [])
        rate-limited {:ok false :error "Exit 1: exceeded retry limit, last status: 429 Too Many Requests"}
        ok {:ok true :result "{:status :accepted}" :session-id "s"}
        run (fn [responses]
              (reset! calls [])
              (let [queue (atom responses)]
                (binding [sut/*rate-limit-backoff-ms* 0]
                  (with-redefs [reg/get-agent (fn [_] ready-agent)
                                reg/invoke-agent! (fn [seat _ _]
                                                    (swap! calls conj seat)
                                                    (let [r (first @queue)]
                                                      (swap! queue rest) r))]
                    (#'sut/producer-invoke! "wm-reviewer" :acceptance-review {})))))]
    (let [result (run [rate-limited ok])]
      (is (= :accepted (:status result)) "the retry's answer is the producer's answer")
      (is (= 2 (count @calls)))
      (is (= 0 (get-in result [:transport-retry :after-ms])))
      (is (str/includes? (get-in result [:transport-retry :first-error]) "429")))
    (let [result (run [{:ok false :error "Exit 1: codex invocation failed"} ok])]
      (is (= :meta-self-heal/producer-invoke-failed (:reason result)))
      (is (= 1 (count @calls)) "a failure that is not a rate limit is not retried")
      (is (nil? (:transport-retry result))))
    (let [result (run [rate-limited rate-limited ok])]
      (is (= :meta-self-heal/producer-invoke-failed (:reason result)))
      (is (= 2 (count @calls)) "exactly one retry")
      (is (str/includes? (:detail result) "429"))
      (is (some? (:transport-retry result))))))
