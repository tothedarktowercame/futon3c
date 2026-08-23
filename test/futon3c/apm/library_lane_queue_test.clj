(ns futon3c.apm.library-lane-queue-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.library-lane-runner :as runner]
            [futon3c.apm.library-lane-queue :as queue])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (str (Files/createTempDirectory
        "library-lane-queue-test-" (make-array FileAttribute 0))))

(defn- delete-tree! [root]
  (doseq [file (reverse (file-seq (io/file root)))] (.delete file)))

(defn- write-status! [root problem-id classification]
  (let [dir (io/file root "problems" problem-id)
        file (io/file dir "status.json")]
    (.mkdirs (io/file dir "lean"))
    (spit (io/file dir "lean" "Main.lean") "theorem fixture : True := by trivial\n")
    (spit file (json/generate-string
                {:classification classification :sorry_count_total 1
                 :obstructions [{:areas ["singular-homology"]}]}))))

(defn- corpus [& ids]
  (let [root (temp-dir)]
    (doseq [id ids] (write-status! root id "partial"))
    root))

(defn- closed-result [id]
  {:ok true :problem-id id :keying-targets [(str id "_bridge")]
   :ruling :closed
   :receipt {:receipt/id (str "bank-" id) :receipt/ruling :closed}})

(defn- blocked-result [id]
  {:ok false :problem-id id :keying-targets [(str id "_bridge")]
   :ruling :blocked :seam :verify})

(deftest banked-and-blocked-problems-are-each-attempted-once
  (let [root (corpus "a" "b" "c") calls (atom [])]
    (try
      (let [result
            (queue/run-queue!
             {:corpus-root root :max-problems 4
              :max-consecutive-non-landings 2
              :run-one-fn
              (fn [{:keys [problem-id]}]
                (swap! calls conj problem-id)
                (if (= "a" problem-id)
                  (do (write-status! root problem-id "closed")
                      (closed-result problem-id))
                  (blocked-result problem-id)))})]
        (is (= ["a" "b" "c"] @calls))
        (is (= 1 (count (filter #{"a"} @calls)))
            "banked problem left the freshly derived queue")
        (is (= 1 (count (filter #{"b"} @calls)))
            "attempted exclusion skipped the still-library blocked problem")
        (is (= :consecutive-non-landings (:stop-condition result))))
      (finally (delete-tree! root)))))

(deftest max-problems-is-exact-and-named
  (let [root (corpus "a" "b" "c") calls (atom [])]
    (try
      (let [result (queue/run-queue!
                    {:corpus-root root :max-problems 2
                     :run-one-fn (fn [{:keys [problem-id]}]
                                   (swap! calls conj problem-id)
                                   (closed-result problem-id))})]
        (is (= ["a" "b"] @calls))
        (is (= 2 (count (:reports result))))
        (is (= :max-problems (:stop-condition result))))
      (finally (delete-tree! root)))))

(deftest launched-problem-cannot-be-replaced-by-the-queue-head
  (let [root (corpus "a" "b") calls (atom [])]
    (try
      (let [result (queue/run-queue!
                    {:corpus-root root :problem-id "b" :max-problems 1
                     :run-one-fn (fn [{:keys [problem-id]}]
                                   (swap! calls conj problem-id)
                                   (closed-result problem-id))})]
        (is (= ["b"] @calls))
        (is (= "b" (get-in result [:reports 0 :problem-id])))
        (is (= :max-problems (:stop-condition result))))
      (finally (delete-tree! root)))))

(deftest consecutive-failure-stop-is-explicit
  (let [root (corpus "a" "b" "c")]
    (try
      (let [result (queue/run-queue!
                    {:corpus-root root :max-problems 9
                     :max-consecutive-non-landings 2
                     :run-one-fn (fn [{:keys [problem-id]}]
                                   (blocked-result problem-id))})]
        (is (= ["a" "b"] (mapv :problem-id (:reports result))))
        (is (= :consecutive-non-landings (:stop-condition result))))
      (finally (delete-tree! root)))))

(defn- git [repo & args]
  (apply shell/sh (concat ["git" "-C" repo] args)))

(deftest dry-run-short-circuits-exactly-at-bank
  (let [root (corpus "a")
        _ (git root "init" "-q" "-b" "trunk")
        _ (git root "config" "user.email" "queue@example.test")
        _ (git root "config" "user.name" "Queue Test")
        _ (git root "add" "-A")
        _ (git root "commit" "-q" "-m" "fixture")
        before (str/trim (:out (git root "rev-parse" "trunk")))
        real-bank-calls (atom 0)
        phase-calls (atom [])
        bank-request {:ok true :ruling :closed :trunk-branch "trunk"
                      :axiom-command ["bash" "-lc" "axioms"]
                      :rollup-command ["lake" "build" "ConstructionTargets"]
                      :status-command ["lake" "env" "lean" "Main.lean"]
                      :status-path "problems/a/status.json"}]
    (try
      (let [result
            (queue/run-queue!
             {:corpus-root root :dry-run? true :max-problems 1
              :bank-fn (fn [_] (swap! real-bank-calls inc))
              :run-one-fn
              (fn [{:keys [problem-id bank-fn]}]
                ;; Represents completed real preflight/solve/verify phases.
                (doseq [phase [:preflight :solve :verify]]
                  (swap! phase-calls conj [problem-id phase]))
                (let [banked (bank-fn (dissoc bank-request :ok))]
                  {:ok (:ok banked) :ruling :closed
                   :keying-targets ["a_bridge"]
                   :receipt (:receipt banked)}))})
            report (first (:reports result))]
        (is (= [["a" :preflight] ["a" :solve] ["a" :verify]] @phase-calls))
        (is (zero? @real-bank-calls))
        (is (= before (str/trim (:out (git root "rev-parse" "trunk")))))
        (is (= :closed (:would-ruling report)))
        (is (= (dissoc bank-request :ok) (:bank-request report)))
        (is (= :max-problems (:stop-condition result))))
      (finally (delete-tree! root)))))

(deftest empty-after-exclusions-is-named
  (let [root (corpus)]
    (try
      (is (= :queue-empty-after-exclusions
             (:stop-condition (queue/run-queue! {:corpus-root root}))))
      (finally (delete-tree! root)))))

(deftest phases-drive-to-certified-not-merely-dispatched
  ;; live-job-driver/drive! returns {:ok true :status :awaiting-terminal} once a
  ;; job is dispatched and only later {:status :certified :certificate ...}.
  ;; Advancing on the first :ok marches past a running solver.
  (let [calls (atom 0)
        phase-run (fn [_]
                    (swap! calls inc)
                    (if (< @calls 3)
                      {:ok true :status :awaiting-terminal}
                      {:ok true :status :certified :certificate {:receipt/id "r"}}))
        result (runner/drive-to-certified phase-run {} {:poll-ms 0 :sleep-fn (fn [_] nil)})]
    (is (= :certified (:status result)))
    (is (= 3 @calls) "polled until certified rather than returning on dispatch")))

(deftest awaiting-terminal-past-the-budget-fails-closed
  (let [now (atom 0)
        phase-run (fn [_] {:ok true :status :awaiting-terminal})
        result (runner/drive-to-certified
                phase-run {} {:poll-ms 0 :timeout-ms 10
                              :sleep-fn (fn [_] (swap! now + 100))
                              :now-fn (fn [] @now)})]
    (is (false? (:ok result)))
    (is (= :phase-awaiting-terminal-timeout (:error/code result)))))

(deftest step-one-returns-after-one-nonterminal-phase-observation
  (let [root (corpus "a")
        calls (atom [])
        result (runner/step-one!
                {:corpus-root root :problem-id "a" :contract {}
                 :seat {}
                 :target-fn (fn [_] {:ok true :targets ["a_bridge"]})
                 :phase-inputs-fn
                 (fn [{:keys [kind]}] {:ok true :kind kind})
                 :phase-run-fn
                 (fn [{:keys [kind]}]
                   (swap! calls conj kind)
                   {:ok true :status :awaiting-terminal})
                 :bank-request-fn (fn [_] (throw (ex-info "too early" {})))})]
    (try
      (is (= [:preflight] @calls))
      (is (= :awaiting (:ruling result)))
      (is (= :preflight (:phase result)))
      (finally (delete-tree! root)))))

(deftest step-one-replays-certified-phases-and-banks-without-sleeping
  (let [root (corpus "a")
        calls (atom [])
        expected-receipts {:preflight {:receipt/id "pre"}
                           :solve {:receipt/id "solve"}
                           :verify {:receipt/id "verify"}}
        result (runner/step-one!
                {:corpus-root root :problem-id "a" :contract {} :seat {}
                 :target-fn (fn [_] {:ok true :targets ["a_bridge"]})
                 :phase-inputs-fn
                 (fn [{:keys [kind]}] {:ok true :kind kind})
                 :phase-run-fn
                 (fn [{:keys [kind]}]
                   (swap! calls conj kind)
                   {:ok true :status :certified
                    :certificate (get expected-receipts kind)})
                 :bank-request-fn (fn [{:keys [receipts]}]
                                    {:ok (= expected-receipts receipts)
                                     :request true})
                 :bank-fn (fn [_] {:ok true
                                   :receipt {:receipt/ruling
                                             :partial-banked}})})]
    (try
      (is (= [:preflight :solve :verify] @calls))
      (is (= :partial-banked (:ruling result)))
      (finally (delete-tree! root)))))
