(ns futon3c.wm.meta-self-heal-adapters-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell]
            [clojure.string :as str]
            [futon2.aif.meta-injury-ledger :as ledger]
            [futon2.aif.meta-injury-store :as injury-store]
            [futon2.aif.meta-repair-coordinator :as coordinator]
            [futon2.aif.meta-self-heal-harness :as harness]
            [futon3c.agency.registry :as reg]
            [futon3c.wm.code-identity :as code-identity]
            [futon3c.wm.meta-self-heal-adapters :as sut]))

(def ready-agent
  {:agent/status :idle :agent/invoke-ready? true :agent/session-id nil})

(defn- committed-readiness-repository []
  (let [repo (.toString
              (java.nio.file.Files/createTempDirectory
               "self-heal-readiness" (make-array java.nio.file.attribute.FileAttribute 0)))
        tracked (io/file repo "tracked.txt")]
    (spit tracked "committed\n")
    (doseq [args [["init" "-q"]
                  ["config" "user.email" "wm@test"]
                  ["config" "user.name" "WM"]
                  ["add" "tracked.txt"]
                  ["commit" "-qm" "fixture"]]]
      (let [result (apply clojure.java.shell/sh "git" "-C" repo args)]
        (assert (zero? (:exit result)) (:err result))))
    {:repo repo :tracked tracked}))

(defn- readiness-options [repo transaction-path]
  {:target-repository repo
   :meta-injury-ledger-path "/isolated/ledger.edn"
   :meta-repair-transaction-path transaction-path
   :queue-retry-fn identity})

(defn- ready-readiness [opts]
  (with-redefs [reg/get-agent (fn [_] ready-agent)]
    (sut/readiness opts)))

(deftest authored-transaction-owns-tracked-worktree-dirt
  (let [{:keys [repo tracked]} (committed-readiness-repository)
        transaction "test/resources/wm/click-7-transaction.edn"
        _ (spit tracked "author repair\n")
        result (ready-readiness (readiness-options repo transaction))
        observation (first (:observations result))]
    (is (= :ready (:status result)))
    (is (not (some #{:tracked-worktree-dirty} (:defects result))))
    (is (= :tracked-worktree-dirty (:kind observation)))
    (is (= {:injury-id "injury/d755c886a22d6fc8c04db7fada64089e07157f62"
            :repair-id "repair/239cf98836afc977cd589141c224ec3f4bca83dc"
            :attempt 1}
           (:owned-by observation)))
    (is (some #(str/includes? % "tracked.txt") (:paths observation)))))

(deftest dirty-worktree-without-transaction-is-refused-with-paths
  (let [{:keys [repo tracked]} (committed-readiness-repository)
        _ (spit tracked "unowned\n")
        missing (str repo "/missing-transaction.edn")
        result (ready-readiness (readiness-options repo missing))]
    (is (= :refused (:status result)))
    (is (= [:tracked-worktree-dirty] (:defects result)))
    (is (some #(str/includes? % "tracked.txt")
              (:tracked-worktree-paths result)))
    (is (nil? (:observations result)))))

(deftest work-order-without-author-receipt-does-not-own-dirt
  (let [{:keys [repo tracked]} (committed-readiness-repository)
        fixture (edn/read-string
                 (slurp "test/resources/wm/click-7-transaction.edn"))
        transaction (io/file repo "authored-without-receipts.edn")
        _ (spit transaction (pr-str (dissoc fixture :receipts)))
        _ (spit tracked "not yet authored\n")
        result (ready-readiness
                (readiness-options repo (.getPath transaction)))]
    (is (= :refused (:status result)))
    (is (= [:tracked-worktree-dirty] (:defects result)))
    (is (some #(str/includes? % "tracked.txt")
              (:tracked-worktree-paths result)))))

(deftest clean-worktree-does-not-report-owned-dirt
  (let [{:keys [repo]} (committed-readiness-repository)
        result (ready-readiness
                (readiness-options repo
                                   "test/resources/wm/click-7-transaction.edn"))]
    (is (= :ready (:status result)))
    (is (nil? (:observations result)))
    (is (nil? (:tracked-worktree-paths result)))))

(deftest retry-port-prefers-coordinator-click-and-exposes-id-minting
  (let [queued (atom [])
        ports (with-redefs [sut/readiness (constantly {:status :ready})]
                (sut/ports {:queue-retry-fn #(swap! queued conj %)}))
        retry! (:repair-retry-fn ports)
        work-order {:repair-id "repair/x" :injury-id "injury/x" :attempt 1}
        enact-click "wm-retry-enact-receipt-1"
        base {:work-order work-order
              :transaction {:receipts [{:payload {:patch-sha "patch"}}
                                       {}
                                       {:payload {:retry-click enact-click}}]}}]
    (testing "coordinator-supplied requeue identity wins and carries its ordinal"
      (let [result (retry! (assoc base :retry-click "wm-requeue-1-1-repair/x"
                                  :requeue-ordinal 1))
            queued-request (peek @queued)]
        (is (= "wm-requeue-1-1-repair/x" (:retry-click queued-request)))
        (is (= 1 (:requeue-ordinal queued-request)))
        (is (= "wm-requeue-1-1-repair/x" (get-in result [:payload :retry-click])))))
    (testing "legacy enact receipt remains the fallback"
      (let [result (retry! base)
            queued-request (peek @queued)]
        (is (= enact-click (:retry-click queued-request)))
        (is (not (contains? queued-request :requeue-ordinal)))
        (is (= enact-click (get-in result [:payload :retry-click])))))
    (testing "the coordinator receives the actual public id minting function"
      (is (identical? sut/retry-click-id (:retry-click-id-fn ports)))
      (is (= (sut/retry-click-id "repair/x" 1 1)
             ((:retry-click-id-fn ports) "repair/x" 1 1))))))

(deftest ports-forward-the-serving-retry-click-status-port
  ;; One-click frontier (2026-10-22, futon2 3e17ebedb): the coordinator's
  ;; read-only durable child-status port must survive the adapter so futon2's
  ;; meta_self_heal_production effect-ports actually consumes it; absence is
  ;; not a defect.
  (let [status-fn (fn [_] nil)
        ports (with-redefs [sut/readiness (constantly {:status :ready})]
                (sut/ports {:queue-retry-fn identity
                            :retry-click-status-fn status-fn}))]
    (is (identical? status-fn (:retry-click-status-fn ports))))
  (let [ports (with-redefs [sut/readiness (constantly {:status :ready})]
                (sut/ports {:queue-retry-fn identity}))]
    (is (not (contains? ports :retry-click-status-fn))
        "absence is forwarded as absence, not as a defect")))

(deftest retry-click-identities-preserve-legacy-and-separate-requeues
  "No live record carried a wm-retry id on 2026-10-20. Ordinal zero is pinned
  to the literal historical A:432 form rather than a live capture."
  (testing "ordinal zero preserves existing receipts"
    (is (= "wm-retry-repair-alpha-1"
           (sut/retry-click-id "repair-alpha" 1 0))))
  (testing "each re-queue gets a fresh id, including against another attempt"
    (let [ids [(sut/retry-click-id "repair-alpha" 1 0)
               (sut/retry-click-id "repair-alpha" 1 1)
               (sut/retry-click-id "repair-alpha" 1 2)
               (sut/retry-click-id "repair-alpha" 2 0)]]
      (is (= 4 (count (set ids))))
      ;; A naive suffix form could confuse a hyphen-bearing repair id with
      ;; an attempt/ordinal tail; the separate wm-requeue namespace cannot.
      (is (not= (sut/retry-click-id "repair-1" 1 1)
                (sut/retry-click-id "repair-1--requeue" 11 0)))
      ;; The pair that actually collides under the naive suffix form
      ;; wm-retry-<repair-id>-<attempt>-<ordinal>: both would read
      ;; "wm-retry-repair-1-1-1". Review 2026-10-20: the pair above stays
      ;; distinct under that form, so it did not pin the scheme.
      (is (not= (sut/retry-click-id "repair-1" 1 1)
                (sut/retry-click-id "repair-1-1" 1 0)))))
  (testing "invalid ordinals fail at the public boundary"
    (is (thrown? AssertionError (sut/retry-click-id "repair-alpha" 1 -1)))
    (is (thrown? AssertionError (sut/retry-click-id "repair-alpha" 1 1.5)))))

(deftest enact-mints-the-legacy-ordinal-zero-retry-id
  (let [root (.toString (java.nio.file.Files/createTempDirectory
                         "meta-adapters-enact" (make-array java.nio.file.attribute.FileAttribute 0)))
        sh #(apply clojure.java.shell/sh "git" "-C" root %)
        _ (sh ["init" "-q"])
        _ (sh ["config" "user.email" "wm@test"])
        _ (sh ["config" "user.name" "WM"])
        _ (spit (io/file root "seed") "seed\n")
        _ (sh ["add" "seed"])
        _ (sh ["commit" "-qm" "seed"])
        source (io/file root "src/fixture/retry.clj")
        _ (.mkdirs (.getParentFile source))
        _ (spit source "(ns fixture.retry)\n")
        _ (sh ["add" "src/fixture/retry.clj"])
        _ (sh ["commit" "-qm" "patch"])
        patch (:out (sh ["rev-parse" "HEAD"]))
        work-order {:repair-id "repair-alpha" :attempt 1 :target-repository root}
        request {:work-order work-order
                 :transaction {:receipts [{:payload {:patch-sha (str/trim patch)}}]}}
        result (with-redefs [code-identity/load-file-recorded!
                             (fn [path] {:status :loaded :path path})]
                 (sut/enact! request))]
    (is (= :succeeded (:status result)))
    (is (= (sut/retry-click-id "repair-alpha" 1 0)
           (get-in result [:payload :retry-click])))))

(defn- enact-fixture-repo
  "A repository with a seed commit and one patch commit touching src/fixture/retry.clj.
   Returns the root, the patch sha and a git runner."
  []
  (let [root (.toString (java.nio.file.Files/createTempDirectory
                         "meta-adapters-enact" (make-array java.nio.file.attribute.FileAttribute 0)))
        sh (fn [& args]
             (let [result (apply clojure.java.shell/sh "git" "-C" root args)]
               (assert (zero? (:exit result)) (:err result))
               (str/trim (:out result))))]
    (sh "init" "-q")
    (sh "config" "user.email" "wm@test")
    (sh "config" "user.name" "WM")
    (spit (io/file root "seed") "seed\n")
    (sh "add" "seed")
    (sh "commit" "-qm" "seed")
    (let [source (io/file root "src/fixture/retry.clj")]
      (.mkdirs (.getParentFile source))
      (spit source "(ns fixture.retry)\n"))
    (sh "add" "src/fixture/retry.clj")
    (sh "commit" "-qm" "patch")
    {:root root :patch (sh "rev-parse" "HEAD") :sh sh}))

(defn- enact-with [root patch]
  (with-redefs [code-identity/load-file-recorded!
                (fn [path] {:status :loaded :path path})]
    (sut/enact! {:work-order {:repair-id "repair-alpha" :attempt 1 :target-repository root}
                 :transaction {:receipts [{:payload {:patch-sha patch}}]}})))

(deftest enact-accepts-a-patch-integrated-below-head
  ;; F37 (claude-5 review tests, 2026-10-20): the repair for injury d755c886 is
  ;; futon2 main b463d85cc and main took unrelated commits after it; the old
  ;; (= patch head) test refused that as :patch-not-canonical-head.
  (let [{:keys [root patch sh]} (enact-fixture-repo)]
    (spit (io/file root "notes.txt") "later\n")
    (sh "add" "notes.txt")
    (sh "commit" "-qm" "unrelated commit after the patch")
    (let [head (sh "rev-parse" "HEAD")
          result (enact-with root patch)
          payload (:payload result)]
      (is (not= patch head) "the fixture must put the patch below HEAD")
      (is (= :succeeded (:status result)) (pr-str result))
      (is (= patch (:patch-sha payload)))
      (is (= head (:integration-commit payload)))
      (is (= [{:status :loaded :path (str root "/src/fixture/retry.clj")}]
             (:reload-receipts payload))
          "only the patch's own source files are reloaded")
      (is (not (contains? payload :reload-receipt))
          "the singular key would be copied onto the :enacted ledger entry"))))

(deftest enact-refuses-a-patch-off-the-canonical-branch
  (let [{:keys [root patch sh]} (enact-fixture-repo)
        canonical (sh "rev-parse" "--abbrev-ref" "HEAD")]
    (sh "checkout" "-q" "-b" "other")
    (spit (io/file root "src/fixture/other.clj") "(ns fixture.other)\n")
    (sh "add" "src/fixture/other.clj")
    (sh "commit" "-qm" "off-branch commit")
    (let [other (sh "rev-parse" "HEAD")
          _ (sh "checkout" "-q" canonical)
          result (enact-with root other)]
      (is (= :failed (:status result)))
      (is (= :patch-not-on-canonical-branch (get-in result [:payload :reason])))
      (is (= other (get-in result [:payload :patch-sha])))
      (is (= patch (get-in result [:payload :canonical-head]))))))

(deftest enact-payload-seals-into-an-enacted-ledger-entry
  ;; The live pin of the Lean rev 25 prediction (enactEvidence_beforeF37Refused):
  ;; the coordinator copies (select-keys payload [:reload-receipt :integration-commit])
  ;; onto the :enacted entry, and the ledger admits exactly one of the two.
  (let [{:keys [root patch]} (enact-fixture-repo)
        payload (:payload (enact-with root patch))
        injury "injury/f37-enact-evidence"
        repair "repair/f37-enact-evidence"
        pin {:path "meta-injury-records/f37.edn" :sha256 (apply str (repeat 64 "a"))}
        append (fn [history entry]
                 (let [result (ledger/append-transition history entry)]
                   (is (= :appended (:status result)) (pr-str result))
                   (:ledger result)))
        reviewed (-> ledger/empty-ledger
                     (append {:injury-id injury :state :open :attempt 1
                              :cause-record-pin pin :failure-stage :construction
                              :failure-kind :cascade-construction-failed
                              :detected-click "click-f37-detected"})
                     (append {:injury-id injury :state :claimed :attempt 1 :repair-id repair
                              :algorithm "dedicated-meta-self-heal"
                              :author-session "author/f37" :reviewer-session "reviewer/f37"})
                     (append {:injury-id injury :state :authored :attempt 1 :repair-id repair
                              :patch-sha patch :worktree root})
                     (append {:injury-id injury :state :reviewed :attempt 1 :repair-id repair
                              :verdict :approved :reviewer-sha patch
                              :reviewer-session "reviewer/f37"}))
        enacted-entry (fn [enact-payload]
                        (merge {:injury-id injury :state :enacted :attempt 1 :repair-id repair
                                :retry-click (:retry-click payload)}
                               (select-keys enact-payload [:reload-receipt :integration-commit])))
        admitted (ledger/append-transition reviewed (enacted-entry payload))
        old-shape (ledger/append-transition
                   reviewed (enacted-entry {:integration-commit (:integration-commit payload)
                                            :reload-receipt (:reload-receipts payload)}))]
    (is (= :appended (:status admitted)) (pr-str admitted))
    (is (= :valid (:status (ledger/validate-ledger (:ledger admitted)))))
    (is (= :refused (:status old-shape)))
    (is (some #{:enactment/evidence} (:defects old-shape))
        "the pre-F37 payload shape is refused by the ledger")))

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
      (let [result (sut/agency-invoke! {:seat "wm-author" :request {}})]
        (is (= :agency-response-not-one-edn-map
               (get-in result [:payload :reason])))
        (is (= "done" (get-in result [:payload :response-preview])))))))

(deftest agency-review-prompt-states-the-sealed-reviewer-answer-contract
  (let [prompt (atom nil)
        patch "b463d85cca1eedd975639c22747dda63f4629974"
        request {:phase :review
                 :work-order {:target-worktree "/home/joe/code/futon2"
                              :acceptance-test {:command ["clojure" "-M:test" "-n" "focused"]}}
                 :transaction {:receipts [{:payload {:patch-sha patch}}]}}
        invoke (fn [_ captured _]
                 (reset! prompt captured)
                 {:ok true
                  :result (str "{:status :succeeded :payload {:patch-sha \"" patch
                               "\" :reviewer-sha \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"}}")
                  :session-id "fresh-review-session"})]
    (with-redefs [reg/reset-session! (fn [_] {:ok true :old-session-id "old"})
                  reg/invoke-agent! invoke]
      (sut/agency-invoke! {:seat "zai-3" :session-id "declared" :request request}))
    (doseq [text ["Reviewer answer contract" patch "/home/joe/code/futon2"
                  "[\"clojure\" \"-M:test\" \"-n\" \"focused\"]"
                  ":reviewer-sha" "{:status :rejected"]]
      (is (str/includes? @prompt text)))
    (is (not (str/includes? @prompt "Author answer contract")))
    ;; The author and reviewer contracts are mutually exclusive.
    (reset! prompt nil)
    (with-redefs [reg/reset-session! (fn [_] {:ok true :old-session-id "old"})
                  reg/invoke-agent! invoke]
      (sut/agency-invoke! {:seat "zai-2" :session-id "declared"
                           :request (assoc request :phase :author)}))
    (is (str/includes? @prompt "Author answer contract"))
    (is (not (str/includes? @prompt "Reviewer answer contract")))
    ;; An absent author receipt is named honestly instead of throwing while
    ;; building the review prompt.
    (reset! prompt nil)
    (with-redefs [reg/reset-session! (fn [_] {:ok true :old-session-id "old"})
                  reg/invoke-agent! invoke]
      (sut/agency-invoke! {:seat "zai-3" :session-id "declared"
                           :request (assoc request :transaction {:receipts []})}))
    (is (str/includes? @prompt "<patch-sha absent from the transaction>"))))

(deftest author-session-freshness-does-not-require-an-agency-job
  ;; Live pin: evidence e-715f3041-3eff-44b4-8377-97cfd5c9663a from
  ;; click wm-click-dfc7501c attempt 2, copied verbatim from futon1b :7073.
  (let [answer (slurp "test/futon3c/wm/fixtures/click-10-attempt-2-author-answer.edn")
        parsed (edn/read-string answer)
        old-session "zai-5d88ef02-8503-4035-9ef6-93adb416beee"
        fresh-session "zai-281d9c55-3125-480d-89c3-7a98a01ec6d0"
        invoke (fn [session extras]
                 (with-redefs [reg/reset-session! (fn [_]
                                                    {:ok true
                                                     :old-session-id old-session})
                               reg/invoke-agent! (fn [& _]
                                                   (merge {:ok true
                                                           :result answer
                                                           :session-id session}
                                                          extras))]
                   (sut/agency-invoke! {:seat "zai-2"
                                        :session-id "declared"
                                        :request {:phase :author}})))]
    (testing "a fresh Zai session succeeds with typed job absence"
      (let [result (invoke fresh-session {})]
        (is (= :succeeded (:status result))
            "the old job-id requirement refused this live answer")
        (is (= (get-in parsed [:payload :patch-sha])
               (get-in result [:payload :patch-sha])))
        (is (= fresh-session (:session-id result)))
        (is (= {:status :absent :reason :seat-mints-no-job}
               (:job-id result)))))

    (testing "the check still refuses a reused session"
      (is (= {:status :failed
              :payload {:reason :agency-session-not-fresh}}
             (invoke old-session {}))))

    (testing "a transport job id remains a string"
      (is (= "invoke-x"
             (:job-id (invoke fresh-session
                              {:invoke-meta {:job-id "invoke-x"}})))))))

(deftest author-final-report-is-retained-and-author-prompt-states-contract
  ;; Live pin. test/resources/wm/click-7-author-final-report.md is zai-2's
  ;; final answer as click-7 author (click wm-click-25cd3682, 2026-10-20,
  ;; evidence e-5f26ac11-5166-4421-b789-0b72e4ff6874), 4135 characters,
  ;; recovered verbatim from the seat's in-memory history: the evidence
  ;; store's copy stops at 4096 characters. The harness forced a prose
  ;; final report; the one code fence that follows is never closed and
  ;; its map carries one surplus `]`. The reader refuses it, and F25 puts
  ;; the first 2000 characters of the answer on the refusal.
  (let [report (slurp "test/resources/wm/click-7-author-final-report.md")
        parsed (#'sut/one-edn-map report)
        ;; Authored, not live: the same answer with its fence closed and the
        ;; surplus `]` removed, so that the status the author chose parses.
        authored (str (str/replace report "]}]}" "]}}") "\n```")]
    (testing "the live click-7 report is refused: its fence never closes"
      (is (= 4135 (count report)))
      (is (str/ends-with? report "\"changes not yet committed\"]}]}"))
      (is (nil? (:value parsed)))
      (is (= "read a symbol, not a map with keyword keys" (:read-error parsed))))

    (testing "with the fence closed, the trailing fence is the machine answer"
      (is (= :incomplete-blocking-commit-and-baseline
             (get-in (#'sut/one-edn-map authored) [:value :status])))
      (is (nil? (:value (#'sut/one-edn-map (str authored "\nmore prose"))))
          "a fence followed by prose is not accepted as the answer"))

    (testing "the live refusal retains the first 2000 characters"
      (with-redefs [reg/reset-session! (fn [_] {:ok true :old-session-id "old"})
                    reg/invoke-agent!
                    (fn [& _]
                      {:ok true :result report :session-id "fresh"
                       :invoke-meta {:job-id "click-7-job"}})]
        (let [result (sut/agency-invoke!
                      {:seat "wm-author" :session-id "declared"
                       :request {:phase :author}})]
          (is (= :failed (:status result)))
          (is (= {:reason :agency-response-not-one-edn-map
                  :response-preview (subs report 0 2000)}
                 (:payload result))))))

    (testing "invalid author status retains its status and the first 2000 characters"
      (with-redefs [reg/reset-session! (fn [_] {:ok true :old-session-id "old"})
                    reg/invoke-agent!
                    (fn [& _]
                      {:ok true :result authored :session-id "fresh"
                       :invoke-meta {:job-id "click-7-job"}})]
        (let [result (sut/agency-invoke!
                      {:seat "wm-author" :session-id "declared"
                       :request {:phase :author}})]
          (is (= :failed (:status result)))
          (is (= {:reason :agency-response-status-invalid
                  :status :incomplete-blocking-commit-and-baseline
                  :response-preview (subs authored 0 2000)}
                 (:payload result))))))

    (testing "only the author phase receives commit and bounded-test instructions"
      (let [prompts (atom [])
            command ["clojure" "-M:test" "-n"
                     "futon2.aif.meta-loop-node-injury-acceptance-test"]
            invoke (fn [phase]
                     (with-redefs [reg/reset-session!
                                   (fn [_] {:ok true :old-session-id "old"})
                                   reg/invoke-agent!
                                   (fn [_ prompt _]
                                     (swap! prompts conj prompt)
                                     {:ok true
                                      :result "{:status :failed :payload {:reason :fixture}}"
                                      :session-id "fresh"
                                      :invoke-meta {:job-id (str (name phase) "-job")}})]
                       (sut/agency-invoke!
                        {:seat "wm-author" :session-id "declared"
                         :request {:phase phase
                                   :work-order {:target-worktree "/worktree"
                                                :acceptance-test {:command command}}}})))]
        (invoke :author)
        (invoke :reviewer)
        (let [[author-prompt reviewer-prompt] @prompts]
          (is (str/includes? author-prompt ":patch-sha"))
          (is (str/includes? author-prompt ":worktree"))
          (is (str/includes? author-prompt (pr-str command)))
          (is (str/includes? author-prompt "git status"))
          (is (str/includes? author-prompt "futon2.aif.full-loop-runner-test"))
          (is (str/includes? author-prompt "futon2.aif.selection-failure-retention-test"))
          (is (not (str/includes? reviewer-prompt ":patch-sha"))))))))

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
    (is (str/includes? prompt "Return exactly one EDN map and no prose"))
    ;; F24a: the reviewer echoes the test's identity, not its source.
    (is (str/includes? prompt ":acceptance-test {:id ... :command ... :expected ...}"))
    (is (str/includes? prompt "Do not echo the test source"))
    (is (not (str/includes? prompt "echoing those five exact request values")))))

(deftest acceptance-proposal-prompt-requires-complete-source
  (let [prompt (#'sut/producer-prompt :acceptance-proposal {:diagnosis {:status :present}})]
    (is (str/includes? prompt ":source {:namespace"))
    (is (str/includes? prompt "complete clojure.test file"))
    (is (str/includes? prompt "real production functions"))
    (is (str/includes? prompt "with no stubs"))
    (is (str/includes? prompt ":production-functions"))
    (is (str/includes? prompt "[\"clojure\" \"-M:test\" \"-n\" \"<that-namespace>\"]")
        "the command template quotes the namespace (wm-click-77a24533)")))

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

(deftest malformed-producer-response-retains-a-bounded-preview
  "Click wm-click-8df4e8d2 (2026-10-19, the first click cast on Zai seats):
   the author's acceptance proposal was refused as
   :producer-response-malformed with :detail nil, so the record could not say
   what the seat returned (F18). The refusal now carries a bounded preview of
   the text, and a map fenced under any tag still reads as the map."
  (let [run (fn [text]
              (with-redefs [reg/get-agent (fn [_] ready-agent)
                            reg/invoke-agent! (fn [_ _ _] {:ok true :result text :session-id "s"})]
                (#'sut/producer-invoke! "zai-2" :acceptance-proposal {})))]
    (let [result (run "```json\n{\"status\": \"proposed\"}\n```")]
      (is (= :meta-self-heal/producer-response-malformed (:reason result)))
      (is (str/includes? (:detail result) "{\"status\": \"proposed\"}")
          "the record says what came back"))
    (let [result (run (apply str "prose " (repeat 2000 "x")))]
      (is (= :meta-self-heal/producer-response-malformed (:reason result)))
      (is (str/starts-with? (str (:detail result)) "prose x")
          "the preview is present, not nil or empty")
      (is (<= (count (:detail result)) 520) "the preview is bounded"))
    ;; F25 (click 7, 2026-10-20): a seat harness's final-report reserve forces
    ;; prose before the one machine answer, so prose before a trailing fence
    ;; reads the fence. Prose after the fence is still refused.
    (is (= :proposed (:status (run "Here is the map:\n```edn\n{:status :proposed}\n```")))
        "prose before a trailing fence reads the fence")
    (let [result (run "```edn\n{:status :proposed}\n```\nHere is the map.")]
      (is (= :meta-self-heal/producer-response-malformed (:reason result))
          "prose after the fence is still refused")
      (is (str/includes? (:detail result) "Here is the map.")
          "and the preview shows the prose that caused it"))
    (let [result (run "```json\n{\"x\": 1, \"y\": true}\n```")]
      (is (= :meta-self-heal/producer-response-malformed (:reason result))
          "JSON with unquoted values reads as string-keyed EDN; not a map the caller can act on")
      (is (str/includes? (:detail result) "\"x\": 1")))
    (is (= :proposed (:status (run "```json\n{:status :proposed}\n```")))
        "an EDN map under a json fence tag is the map")
    (is (= :proposed (:status (run "```edn\n{:status :proposed}\n```"))))
    (is (= :proposed (:status (run "{:status :proposed}"))))))

(deftest malformed-producer-response-keeps-the-rate-limit-retry-mark
  "A seat that was rate-limited once and then answered with prose: the
   malformed refusal must still say the invoke was retried (claude-2's review
   of ca2fa4074: the :transport-retry assoc on this branch was untested)."
  (let [calls (atom 0)]
    (binding [sut/*rate-limit-backoff-ms* 0]
      (with-redefs [reg/get-agent (fn [_] ready-agent)
                    reg/invoke-agent! (fn [_ _ _]
                                        (if (= 1 (swap! calls inc))
                                          {:ok false :error "Exit 1: last status: 429 Too Many Requests"}
                                          {:ok true :result "no map here" :session-id "s"}))]
        (let [result (#'sut/producer-invoke! "zai-2" :acceptance-proposal {})]
          (is (= :meta-self-heal/producer-response-malformed (:reason result)))
          (is (= 3 @calls) "one transport retry and one reader-informed re-ask")
          (is (some? (:transport-retry result)) "the retry is recorded on the malformed branch")
          (is (str/includes? (:detail result) "no map here")))))))

(deftest malformed-producer-answer-is-reasked-once-with-reader-evidence
  (let [live-answer (slurp
                     "test/resources/wm/zai2-acceptance-proposal-unbalanced-2026-10-20.txt")
        balanced (pr-str
                  {:status :proposed
                   :acceptance-test
                   {:id "x" :command ["clojure"] :expected :passed
                    :source {:namespace "n" :path "test/n.clj" :text "t"}}})
        unrepairable "{:status :proposed :note \"unterminated"
        run (fn [responses]
              (let [queue (atom responses)
                    calls (atom [])]
                (binding [sut/*rate-limit-backoff-ms* 0]
                  (with-redefs [reg/get-agent (fn [_] ready-agent)
                                reg/invoke-agent!
                                (fn [seat prompt options]
                                  (swap! calls conj {:seat seat :prompt prompt
                                                     :options options})
                                  (let [response (first @queue)]
                                    (swap! queue rest)
                                    response))]
                    {:result (#'sut/producer-invoke!
                              "zai-2" :acceptance-proposal {})
                     :calls @calls}))))]
    (testing "an unterminated string receives one informed re-ask"
      (let [{:keys [result calls]}
            (run [{:ok true :result unrepairable :session-id "s"}
                  {:ok true :result balanced :session-id "s"}])
            retry (:read-retry result)
            second-prompt (:prompt (second calls))]
        (is (= :proposed (:status result)))
        (is (= 2 (count calls)))
        (is (str/includes? (:first-error retry) "EOF while reading"))
        (is (str/includes? (:first-error retry)
                           "1 `{` opened, 0 `}` closed outside strings"))
        (is (string? (:first-preview retry)))
        (is (str/ends-with?
             second-prompt
             (str "Every value that is prose must be one quoted string; "
                  "return the corrected map as one EDN map and nothing else.")))
        (is (str/includes? second-prompt "EOF while reading"))
        (is (str/includes? second-prompt
                           "1 `{` opened, 0 `}` closed outside strings")
            "the reader's brace count reaches the re-ask unchanged")))

    (testing "the committed one-brace-short answer is repaired without a re-ask"
      (let [parsed (#'sut/one-edn-map live-answer)
            {:keys [result calls]}
            (run [{:ok true :result live-answer :session-id "s"}])]
        (is (= :proposed (get-in parsed [:value :status])))
        (is (= 1 (get-in parsed [:read-repair :appended])))
        (is (str/includes? (get-in parsed [:read-repair :error])
                           "3 `{` opened, 2 `}` closed outside strings"))
        (is (= :proposed (:status result)))
        (is (= 1 (count calls)) "a successful read repair sends no re-ask")
        (is (= (:read-repair parsed) (:read-repair result)))))

    (testing "a second malformed answer is refused with both attempts recorded"
      (let [second-answer (str "prose " (apply str (repeat 900 "x"))
                               " SECOND-ANSWER-TAIL")
            {:keys [result calls]}
            (run [{:ok true :result unrepairable :session-id "s"}
                  {:ok true :result second-answer :session-id "s"}])]
        (is (= :meta-self-heal/producer-response-malformed (:reason result)))
        (is (= 2 (count calls)) "exactly one reader-informed re-ask")
        (is (string? (:read-error result))
            "deleting the final :read-error association makes this fail")
        (is (some? (:read-retry result)))
        (is (str/includes? (:detail result) "SECOND-ANSWER-TAIL"))))

    (testing "a readable non-map also receives the one re-ask"
      (let [{:keys [result calls]}
            (run [{:ok true :result "prose answer" :session-id "s"}
                  {:ok true :result balanced :session-id "s"}])]
        (is (= :proposed (:status result)))
        (is (= 2 (count calls)))
        (is (str/includes? (get-in result [:read-retry :first-error])
                           "read a symbol"))))

    (testing "transport retry and read retry survive together"
      (let [{:keys [result calls]}
            (run [{:ok false :error "429 Too Many Requests"}
                  {:ok true :result unrepairable :session-id "s"}
                  {:ok true :result balanced :session-id "s"}])]
        (is (= :proposed (:status result)))
        (is (= 3 (count calls)))
        (is (some? (:transport-retry result)))
        (is (some? (:read-retry result)))))))

(deftest acceptance-proposal-source-is-read-before-review
  (let [click-4-source (slurp
                        "test/resources/wm/click-4-acceptance-test-surplus-paren.clj")
        balanced-source "(ns proposed.acceptance-test)\n(deftest reads (is true))"
        proposal (fn [source]
                   (pr-str {:status :proposed
                            :acceptance-test
                            {:id "source-read" :command ["clojure"] :expected :passed
                             :source {:namespace "proposed.acceptance-test"
                                      :path "test/proposed/acceptance_test.clj"
                                      :text source}}}))
        run (fn [operation responses]
              (let [queue (atom responses)
                    calls (atom [])]
                (binding [sut/*rate-limit-backoff-ms* 0]
                  (with-redefs [reg/get-agent (fn [_] ready-agent)
                                reg/invoke-agent!
                                (fn [seat prompt options]
                                  (swap! calls conj {:seat seat :prompt prompt
                                                     :options options})
                                  (let [response (first @queue)]
                                    (swap! queue rest)
                                    response))]
                    {:result (#'sut/producer-invoke! "zai-2" operation {})
                     :calls @calls}))))]
    (testing "two surplus delimiters get an informed retry at the original position"
      (let [{:keys [result calls]}
            (run :acceptance-proposal
                 [{:ok true :result (proposal (str/replace click-4-source #"\n$" ")\n"))
                   :session-id "s"}
                  {:ok true :result (proposal balanced-source) :session-id "s"}])
            retry (:source-retry result)]
        (is (= :proposed (:status result)))
        (is (= 135 (:first-line retry)))
        (is (= 87 (:first-col retry)))
        (is (str/includes? (:first-error retry) "Unmatched delimiter"))
        (is (= balanced-source (get-in result [:acceptance-test :source :text])))
        (is (nil? (:source-read-error result)))
        (is (= 2 (count calls)))
        (is (str/includes? (:prompt (second calls))
                           "Your test source does not read:"))
        (is (str/includes? (:prompt (second calls)) "at 135:87"))))

    (testing "a second unreadable source is retained for admission and review"
      (let [{:keys [result calls]}
            (run :acceptance-proposal
                 [{:ok true
                   :result (proposal (str/replace click-4-source #"\n$" ")\n"))
                   :session-id "s"}
                  {:ok true :result (proposal "(ns still.bad)))") :session-id "s"}])]
        (is (= :proposed (:status result)))
        (is (some? (:source-retry result)))
        (is (str/includes? (:source-read-error result) "Unmatched delimiter"))
        (is (re-find #" at [0-9]+:[0-9]+$" (:source-read-error result)))
        (is (= 2 (count calls)))))

    (testing "readable source passes with one invoke"
      (let [{:keys [result calls]}
            (run :acceptance-proposal
                 [{:ok true :result (proposal balanced-source) :session-id "s"}])]
        (is (= :proposed (:status result)))
        (is (nil? (:source-retry result)))
        (is (= 1 (count calls)))))

    (testing "an unterminated source string receives the same informed retry"
      (let [{:keys [result calls]}
            (run :acceptance-proposal
                 [{:ok true :result (proposal "(def x \"unterminated") :session-id "s"}
                  {:ok true :result (proposal balanced-source) :session-id "s"}])]
        (is (= :proposed (:status result)))
        (is (str/includes? (get-in result [:source-retry :first-error])
                           "EOF while reading string"))
        (is (= 2 (count calls)))))

    (testing "acceptance review source is not read"
      (let [review (pr-str {:status :accepted
                            :acceptance-test {:source {:text click-4-source}}})
            {:keys [result calls]}
            (run :acceptance-review [{:ok true :result review :session-id "s"}])]
        (is (= :accepted (:status result)))
        (is (nil? (:source-retry result)))
        (is (= 1 (count calls)))))

    (testing "absent and non-string source are left to downstream admission"
      (doseq [answer [(pr-str {:status :proposed :acceptance-test {}})
                      (proposal 42)]]
        (let [{:keys [result calls]}
              (run :acceptance-proposal [{:ok true :result answer :session-id "s"}])]
          (is (= :proposed (:status result)))
          (is (nil? (:source-retry result)))
          (is (= 1 (count calls))))))))

(deftest acceptance-source-removes-one-surplus-delimiter-at-reader-position
  (let [click-4-source (slurp
                        "test/resources/wm/click-4-acceptance-test-surplus-paren.clj")
        click-5-source (slurp
                        "test/resources/wm/click-5-acceptance-test-surplus-paren-mid-file.clj")
        balanced-source "(ns proposed.acceptance-test)\n(deftest reads (is true))"
        proposal (fn [source]
                   (pr-str {:status :proposed
                            :acceptance-test
                            {:id "source-repair" :command ["clojure"] :expected :passed
                             :source {:namespace "proposed.acceptance-test"
                                      :path "test/proposed/acceptance_test.clj"
                                      :text source}}}))
        run (fn [responses]
              (let [queue (atom responses)
                    calls (atom [])]
                (binding [sut/*rate-limit-backoff-ms* 0]
                  (with-redefs [reg/get-agent (fn [_] ready-agent)
                                reg/invoke-agent!
                                (fn [seat prompt options]
                                  (swap! calls conj {:seat seat :prompt prompt
                                                     :options options})
                                  (let [response (first @queue)]
                                    (swap! queue rest)
                                    response))]
                    {:result (#'sut/producer-invoke!
                              "zai-2" :acceptance-proposal {})
                     :calls @calls}))))
        remove-at (fn [source line col]
                    (let [lines (str/split source #"\n" -1)
                          line-index (dec line)]
                      (str/join "\n"
                                (update lines line-index
                                        (fn [line-text]
                                          (str (subs line-text 0 (dec col))
                                               (subs line-text col)))))))]
    (testing "click 4 is repaired at the reader's position without a re-ask"
      (let [{:keys [result calls]}
            (run [{:ok true :result (proposal click-4-source) :session-id "s"}])
            repaired (get-in result [:acceptance-test :source :text])]
        (is (= 1 (count calls)))
        (is (nil? (:source-retry result)))
        (is (= {:removed ")" :line 135 :col 86}
               (select-keys (:source-repair result) [:removed :line :col])))
        (is (str/includes? (get-in result [:source-repair :error])
                           "Unmatched delimiter"))
        (is (= (remove-at click-4-source 135 86) repaired))
        (is (= :readable (:status (#'sut/source-read-result repaired))))))

    (testing "a position whose character is not the named delimiter is not edited"
      ;; F23 review: the reader's position is trusted only when the character
      ;; there is the delimiter it named.
      (with-redefs [sut/remove-source-character
                    (fn [source _ _] {:source (subs source 1) :removed (subs source 0 1)})]
        (let [{:keys [result calls]}
              (run [{:ok true :result (proposal click-4-source) :session-id "s"}
                    {:ok true :result (proposal balanced-source) :session-id "s"}])]
          (is (= 2 (count calls)))
          (is (nil? (:source-repair result)))
          (is (= 135 (get-in result [:source-retry :first-line]))))))

    (testing "click 5 mid-file repair leaves every later line intact"
      (let [{:keys [result calls]}
            (run [{:ok true :result (proposal click-5-source) :session-id "s"}])
            repaired (get-in result [:acceptance-test :source :text])]
        (is (= 1 (count calls)))
        (is (= {:removed ")" :line 115 :col 87}
               (select-keys (:source-repair result) [:removed :line :col])))
        (is (= (remove-at click-5-source 115 87) repaired))
        (is (= (drop 115 (str/split-lines click-5-source))
               (drop 115 (str/split-lines repaired)))
            "t4 and all text after repaired line 115 survive unchanged")
        (is (str/includes? repaired "(deftest t4-terminal-routing-injured-vs-repaired"))
        (is (= :readable (:status (#'sut/source-read-result repaired))))))

    (testing "a brace deficit is not repaired and receives the F22 re-ask"
      (let [deficit (str/replace click-4-source #"\)\)\n$" "\n")
            {:keys [result calls]}
            (run [{:ok true :result (proposal deficit) :session-id "s"}
                  {:ok true :result (proposal balanced-source) :session-id "s"}])]
        (is (= 2 (count calls)))
        (is (str/includes? (get-in result [:source-retry :first-error])
                           "EOF while reading"))
        (is (nil? (:source-repair result)))))

    (testing "a repaired second answer retains both source attempt records"
      (let [deficit (str/replace click-4-source #"\)\)\n$" "\n")
            {:keys [result calls]}
            (run [{:ok true :result (proposal deficit) :session-id "s"}
                  {:ok true :result (proposal click-4-source) :session-id "s"}])]
        (is (= 2 (count calls)))
        (is (some? (:source-retry result)))
        (is (= {:removed ")" :line 135 :col 86}
               (select-keys (:source-repair result) [:removed :line :col])))
        (is (nil? (:source-read-error result)))))))

(deftest eof-brace-count-ignores-strings-and-remains-total
  (let [balanced (#'sut/one-edn-map "{:a \"}{\"}")
        unbalanced-string (#'sut/one-edn-map "{:a \"x")
        surplus (#'sut/one-edn-map "{:a 1}}")
        deficit-two (#'sut/one-edn-map "{:a {:b 1")
        repaired-vector (#'sut/one-edn-map "[:a]\n{")
        repaired-string-key-map (#'sut/one-edn-map "{\"a\" {")]
    (is (= {:a "}{"} (:value balanced))
        "braces inside a balanced string do not affect reading")
    (is (nil? (:read-error balanced)))
    (is (str/includes? (:read-error unbalanced-string) "EOF while reading"))
    (is (str/includes? (:read-error unbalanced-string)
                       "1 `{` opened, 0 `}` closed outside strings")
        "an unbalanced string produces a counted EOF result rather than throwing")
    (let [escaped (#'sut/one-edn-map "{:a \"\\\"}\" :b {")]
      (is (str/includes? (get-in escaped [:read-repair :error])
                         "2 `{` opened, 0 `}` closed outside strings")
          "an escaped quote does not end the string, so its brace is not counted")
      (is (= 2 (get-in escaped [:read-repair :appended]))))
    (is (nil? (:read-repair unbalanced-string))
        "a brace appended inside an unterminated string is not a repair")
    (is (= {:a 1} (:value surplus)) "a surplus brace after the map is trailing text, read as before")
    (is (nil? (:read-repair surplus)) "a surplus brace is never changed")
    (is (= {:a {:b 1}} (:value deficit-two)))
    (is (= 2 (get-in deficit-two [:read-repair :appended])))
    (is (str/includes? (:read-error repaired-vector) "read a vector"))
    (is (nil? (:read-repair repaired-vector)))
    (is (str/includes? (:read-error repaired-string-key-map) "read a map"))
    (is (nil? (:read-repair repaired-string-key-map)))
    (let [trailing (#'sut/one-edn-map "{:status :proposed}\n\nNote: balanced.")]
      (is (= {:status :proposed} (:value trailing))
          "a map followed by trailing prose is the map, as before F21 (no re-ask)")
      (is (nil? (:read-repair trailing))))))

(deftest enact-reload-receipts-seal-into-the-port-receipt
  ;; F39, the finding of click wm-click-96850427 (2026-10-20 19:37Z): with the
  ;; real loader, enact! succeeded and the port receipt could not be sealed,
  ;; because load-file-recorded! returned the loaded Var and enact! put it in
  ;; :reload-receipts. This test does not stub the loader: the fixture repo is
  ;; bound as the canonical root, and the seal is the coordinator's own.
  (let [{:keys [root patch]} (enact-fixture-repo)
        work-order {:injury-id "injury/f39" :repair-id "repair-alpha" :attempt 1
                    :target-repository root}
        request {:work-order work-order :phase :enact
                 :predecessor-pin {:path "p" :sha256 (apply str (repeat 64 "b"))}
                 :transaction {:receipts [{:payload {:patch-sha patch}}]}}
        _ (code-identity/reset-for-test!)
        result (binding [code-identity/*futon2-root* root]
                 (sut/enact! request))
        payload (:payload result)
        seal (fn [p]
               (coordinator/seal-port-receipt
                {:phase :enact :injury-id (:injury-id work-order)
                 :repair-id (:repair-id work-order) :attempt (:attempt work-order)
                 :predecessor-pin (:predecessor-pin request)
                 :status (:status result) :payload (assoc p :adapter "wm-repair-reviewer")}))]
    (is (= :succeeded (:status result)) (pr-str result))
    (is (= 1 (count (:reload-receipts payload))))
    (is (every? map? (:reload-receipts payload)))
    (is (every? #(and (= "src/fixture/retry.clj" (:path %)) (true? (:stable? %))
                      (false? (:dirty? %)) (string? (:content-sha256 %)))
                (:reload-receipts payload)))
    (is (map? (:receipt-pin (seal payload))) "the real payload seals")
    ;; The pre-F39 shape, a Var in the payload, is what the click produced.
    (is (thrown-with-msg? Exception #"Unsupported action identity value"
                          (seal (assoc payload :reload-receipts [#'clojure.core/map]))))))

(deftest acceptance-run-default-shares-the-authoritative-injury-root
  (is (= (str injury-store/default-root "/acceptance-runs")
         sut/*acceptance-runs-root*)))

(deftest envelope-reask-locates-live-unquoted-prose
  (let [file (io/file "test/resources/wm/diagnosis-reply-tilde.txt")
        text (slurp file)
        calls (atom [])]
    (is (= "64706efa6844b8f79a8aa5ee41840cfe563484d5852315a711fe724984bdd85f"
           (#'sut/hex-sha256 (java.nio.file.Files/readAllBytes (.toPath file)))))
    (with-redefs [reg/get-agent (fn [_] ready-agent)
                  reg/invoke-agent! (fn [_ prompt _]
                                      (swap! calls conj prompt)
                                      {:ok true :result (if (= 1 (count @calls))
                                                         text "{:status :succeeded :diagnosis {}}")
                                       :session-id "fixture"})]
      (let [result (#'sut/producer-invoke! "zai-2" :diagnosis {})
            prompt (second @calls)]
        (is (= 2 (count @calls)))
        (is (str/includes? prompt "line 20"))
        (is (str/includes? prompt "reviewer-wait wiring (src/futon2/aif/full_loop_runner.clj ~lines"))
        (is (not (str/includes? prompt "Return the same map")))
        (is (= 20 (get-in result [:read-retry :first-line])))
        (is (pos-int? (get-in result [:read-retry :first-col])))
        (is (= :succeeded (:status result)))))))
