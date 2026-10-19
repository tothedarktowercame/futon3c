(ns futon3c.wm.meta-self-heal-adapters-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.java.shell]
            [clojure.string :as str]
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
    (.mkdirs (io/file root "test/fixture"))
    (spit (io/file root ".gitignore") ".cpcache/\n")
    (spit (io/file root "deps.edn")
          "{:paths [\"test\"] :aliases {:test {:extra-deps {io.github.cognitect-labs/test-runner {:git/url \"https://github.com/cognitect-labs/test-runner.git\" :sha \"5e91ee0989d93115cfb5e15b6f242f5ec7ab762e\"}} :main-opts [\"-m\" \"cognitect.test-runner\"]}}}\n")
    (spit (io/file root "test/fixture/acceptance_test.clj")
          "(ns fixture.acceptance-test (:require [clojure.test :refer [deftest is]]))\n(deftest fails (println \"ok\") (is false))\n")
    (doseq [args [["init" "-q"] ["config" "user.email" "wm@test"]
                  ["config" "user.name" "WM"] ["add" "."]
                  ["commit" "-qm" "fixture"]]]
      (apply clojure.java.shell/sh "git" "-C" root args))
    (binding [sut/*acceptance-repository* root
              sut/*acceptance-runs-root* logs]
      (testing "nonzero exit and full-output pin"
        (let [result (port 60000 {:id "fails" :command ["clojure" "-M:test" "-n"
                                                          "fixture.acceptance-test"]})
              path (get-in result [:run-pin :path])
              bytes (when path (.getBytes (slurp path) "UTF-8"))]
          (is (= [:ran :failed 1] ((juxt :status :outcome :exit) result)))
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
      (testing "only clojure.test namespace/optional-var commands are admitted"
        (doseq [command [["clojure" "-M:test" "-e" "(println 1)"]
                         ["clojure" "-M:test" "-n" "bad namespace"]]]
          (is (= {:status :failed
                  :reason :meta-self-heal/acceptance-command-not-admissible
                  :command command}
                 (port 1000 {:id "unsafe" :command command}))))))))

(deftest acceptance-review-prompt-describes-pre-repair-result
  (let [prompt (#'sut/producer-prompt :acceptance-review {:result {:status :ran}})]
    (is (str/includes? prompt "run before any repair, in the canonical checkout"))
    (is (str/includes? prompt "result is in :result"))
    (is (str/includes? prompt "fails for the injury's reason"))
    (is (str/includes? prompt "Return exactly one EDN map and no prose"))))
