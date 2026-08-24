(ns futon3c.apm.library-loop-exec-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.library-loop-checkpoint :as checkpoint]
            [futon3c.apm.library-loop-exec :as exec]
            [futon3c.apm.library-loop-runner :as runner]
            [futon3c.apm.library-loop-slate :as slate])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "library-loop-exec-"
                                      (make-array FileAttribute 0))))

(def files
  {"ConstructionTargets/A.lean" "import Mathlib\n"
   "ConstructionTargets/B.lean" "import ConstructionTargets.A\n"
   "problems/t00J02/lean/Main.lean" "import ConstructionTargets.B\n"})

(defn- observation [state]
  {:base-sha (:base-sha state)
   :head-sha (:head-sha state)
   :name-status "M\tConstructionTargets/A.lean\n"
   :porcelain ""
   :files files
   :problem-main "problems/t00J02/lean/Main.lean"
   :targets []
   :axiom-audits {"ConstructionTargets.A"
                  {:ok? true :head-sha (:head-sha state)
                   :declarations ['a]}}})

(defn- init-at! [dir phase turn]
  (runner/write-state!
   dir (assoc (runner/initial-state
               {:problem-id "t00J02" :workspace "/tmp/apm-lean"
                :base-sha "base" :head-sha "candidate"})
              :phase phase :turn turn)))

(defn- claim []
  (let [statement "def Producer : Prop := True"]
    {:id :t00J02/producer
     :declaration 'OrientedSurfacePreimageDuality.Producer
     :statement statement
     :statement-digest (checkpoint/statement-digest statement)
     :dependencies #{:surface/duality}
     :strength :equivalent
     :reduction-witness "Discharged one concrete premise."
     :next-plan "Build the remaining producer."
     :provenance {:constructed-by "checkpoint-seat"}}))

(defn- rejected-review-state! [dir]
  (init-at! dir :checkpoint-ready 20)
  (runner/install-checkpoint! dir (claim))
  (let [intent (runner/begin-action! dir :review)
        identity (:pending-checkpoint (runner/read-state dir))]
    (runner/append-receipt!
     dir intent (merge identity
                       {:outcome :rejected :bank-authorized? false
                        :progress-ruling :reduced
                        :review-rationale "Approval withheld."
                        :consecutive-nonreductions 0
                        :finding :review-not-approved}))
    (runner/reconcile! dir (constantly nil))))

(deftest gate-orders-rebuild-consumers-and-main-and-captures-evidence
  (let [dir (temp-dir)
        calls (atom [])]
    (init-at! dir :gating 1)
    (exec/run-gate!
     dir {:observe observation
          :run-command (fn [command]
                         (swap! calls conj command)
                         {:exit 0 :stdout (str "built " (last command)) :stderr ""})})
    (is (= [["lake" "build" "ConstructionTargets.A"]
            ["lake" "build" "ConstructionTargets.B"]
            ["lake" "env" "lean" "problems/t00J02/lean/Main.lean"]]
           @calls))
    (is (= :turn-ready (:phase (runner/read-state dir))))
    (let [receipt (->> (file-seq (java.io.File. dir "receipts/gate"))
                       (filter #(.isFile %)) first slurp read-string)
          result (:result receipt)]
      (is (= :green (:outcome result)))
      (is (= @calls (mapv :command (:commands result))))
      (is (every? #(contains? % :stdout) (:commands result)))
      (is (string? (get-in result [:registration :snapshot :files-digest]))))))

(deftest repeated-failure-pauses-while-changed-and-green-reset
  (let [dir (temp-dir)
        failure (atom "same")
        deps {:observe observation
              :run-command (fn [_] {:exit 1 :stdout "" :stderr @failure})}]
    (init-at! dir :gating 1)
    (exec/run-gate! dir deps)
    (is (= 1 (:consecutive-same-failures (runner/read-state dir))))
    (runner/write-state! dir (assoc (runner/read-state dir) :phase :gating))
    (reset! failure "different")
    (exec/run-gate! dir deps)
    (is (= 1 (:consecutive-same-failures (runner/read-state dir))))
    (runner/write-state! dir (assoc (runner/read-state dir) :phase :gating))
    (exec/run-gate! dir deps)
    (is (= :paused (:phase (runner/read-state dir))))
    (is (= :repeated-gate-failure
           (get-in (runner/read-state dir) [:pause/finding :type])))
    (let [green-dir (temp-dir)]
      (init-at! green-dir :gating 1)
      (runner/write-state! green-dir
                           (assoc (runner/read-state green-dir)
                                  :failure-fingerprint "old"
                                  :consecutive-same-failures 1))
      (exec/run-gate! green-dir
                      {:observe observation
                       :run-command (constantly {:exit 0 :stdout "" :stderr ""})})
      (is (nil? (:failure-fingerprint (runner/read-state green-dir))))
      (is (zero? (:consecutive-same-failures
                  (runner/read-state green-dir)))))))

(deftest gate-restart-settles-existing-receipt-without-running-commands
  (let [dir (temp-dir)
        _ (init-at! dir :gating 1)
        intent (runner/begin-action! dir :gate)]
    (runner/append-receipt! dir intent
                            {:outcome :green :checkpoint-due? false
                             :receipt/path "receipts/gate/existing.edn"})
    (is (= :settled
           (:status (exec/run-gate!
                     dir {:observe (fn [_] (throw (Exception. "duplicate observe")))
                          :run-command (fn [_] (throw (Exception. "duplicate command")))}))))))

(deftest typed-observation-refusal-settles-bounded-red-receipt
  (let [dir (temp-dir)
        _ (init-at! dir :gating 4)
        intent (runner/begin-action! dir :gate)
        result (exec/run-gate!
                dir {:observe (fn [_]
                                (throw
                                 (ex-info
                                  "volatile exception text with SECRET"
                                  {:finding :target-created-turn-provenance-unproved
                                   :module "ConstructionTargets.NewTarget"
                                   :created-turn 2
                                   :creation-commit-sha (apply str (repeat 40 "a"))
                                   :turn-receipt-id (apply str (repeat 64 "b"))
                                   :turn-head-sha (apply str (repeat 40 "c"))
                                   :stdout "SECRET stdout"
                                   :stderr "SECRET stderr"
                                   :path "/tmp/volatile-secret"})))
                     :run-command (fn [_] (throw (Exception. "must not run")))})
        receipt (runner/read-receipt dir intent)
        rendered (pr-str receipt)]
    (is (= :settled (:status result)))
    (is (= :turn-ready (:phase (runner/read-state dir))))
    (is (= 5 (:turn (runner/read-state dir))))
    (is (= 1 (:consecutive-same-failures (runner/read-state dir))))
    (is (= :red (get-in receipt [:result :outcome])))
    (is (= :target-created-turn-provenance-unproved
           (get-in receipt [:result :finding])))
    (is (= "ConstructionTargets.NewTarget"
           (get-in receipt [:result :diagnostic :module])))
    (is (= {:module "ConstructionTargets.NewTarget"
            :created-turn 2
            :creation-commit-sha (apply str (repeat 40 "a"))
            :turn-receipt-id (apply str (repeat 64 "b"))
            :turn-head-sha (apply str (repeat 40 "c"))}
           (:diagnostic
            (runner/preceding-gate-feedback dir (runner/read-state dir)))))
    (is (not (re-find #"SECRET|stdout|stderr|volatile-secret|exception text"
                      rendered)))))

(deftest typed-refusal-fingerprint-is-stable-across-pending-restart
  (let [run! (fn [secret]
               (let [dir (temp-dir)
                     _ (init-at! dir :gating 4)
                     intent (runner/begin-action! dir :gate)
                     calls (atom 0)]
                 ;; This is the restart shape: the exact intent already exists
                 ;; and has no receipt when run-gate! is entered.
                 (exec/run-gate!
                  dir {:observe (fn [_]
                                  (swap! calls inc)
                                  (throw (ex-info "different volatile text"
                                                  {:finding :workspace-head-drift
                                                   :expected-head "candidate"
                                                   :observed-head "other"
                                                   :stderr secret})))
                       :run-command (constantly {:exit 0})})
                 {:fingerprint (get-in (runner/read-receipt dir intent)
                                       [:result :failure-fingerprint])
                  :calls @calls}))
        first-run (run! "first secret")
        restarted (run! "second secret")]
    (is (= 1 (:calls first-run) (:calls restarted)))
    (is (= (:fingerprint first-run) (:fingerprint restarted)))
    (is (re-matches #"[0-9a-f]{64}" (:fingerprint first-run)))))

(deftest untyped-observation-exception-still-propagates
  (let [dir (temp-dir)]
    (init-at! dir :gating 1)
    (is (thrown-with-msg?
         RuntimeException #"programmer bug"
         (exec/run-gate! dir {:observe (fn [_]
                                              (throw (RuntimeException.
                                                      "programmer bug")))
                              :run-command (constantly {:exit 0})})))
    (is (= :gating (:phase (runner/read-state dir))))
    (is (nil? (runner/read-receipt dir (:intent (runner/read-state dir)))))))

(deftest checkpoint-cadence-waits-for-independent-review
  (let [dir (temp-dir)]
    (init-at! dir :gating 20)
    (exec/run-gate! dir {:observe observation
                         :run-command (constantly {:exit 0 :stdout "" :stderr ""})})
    (is (= :checkpoint-ready (:phase (runner/read-state dir))))
    (let [{:keys [identity request-path]} (exec/request-review! dir (claim))]
      (is (.exists (java.io.File. request-path)))
      (is (= identity (:pending-checkpoint (runner/read-state dir))))
      (is (nil? (:intent (runner/read-state dir))))
      (let [review {:checkpoint-digest (:checkpoint-digest identity)
                    :obligation-id (:obligation-id identity)
                    :ruling :reduced
                    :rationale "The dependency set strictly decreased."
                    :approved? true}]
        (exec/apply-review! dir (claim) review)
        (is (= :review-pending (:phase (runner/read-state dir))))))))

(deftest reconsider-review-requires-independent-seat-and-can-authorize-bank
  (let [dir (temp-dir)
        digest (checkpoint/checkpoint-digest (claim))]
    (rejected-review-state! dir)
    (let [original-state (runner/read-state dir)
          result (exec/reconsider-review!
                  dir (claim)
                  {:checkpoint-digest digest
                   :obligation-id :t00J02/producer
                   :ruling :reduced
                   :rationale "Independent evidence confirms reduction."
                   :approved? true
                   :reviewer-id "second-seat"})]
      (is (= :settled (:status result)))
      (is (= :review-pending (:phase (runner/read-state dir))))
      (is (nil? (:intent (runner/read-state dir))))
      (is (= {:action :review
              :intent-id (get-in original-state [:pause/finding :intent-id])}
             (get-in (:receipt result) [:result :prior-review])))
      (is (= "second-seat"
             (get-in (:receipt result) [:result :reviewer-id]))))))

(deftest reconsider-review-refuses-checkpoint-constructor-as-reviewer
  (let [dir (temp-dir)]
    (rejected-review-state! dir)
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"review-independence-unproved"
         (exec/reconsider-review!
          dir (claim)
          {:checkpoint-digest (checkpoint/checkpoint-digest (claim))
           :obligation-id :t00J02/producer :ruling :reduced
           :rationale "Not independent." :approved? true
           :reviewer-id "checkpoint-seat"})))
    (is (= :paused (:phase (runner/read-state dir))))))

(deftest reconsider-review-is-an-explicit-cli-operation
  (let [root (temp-dir)
        dir (exec/run-dir root "t00J02")
        checkpoint-file (java.io.File. root "checkpoint.edn")
        review-file (java.io.File. root "review.edn")]
    (rejected-review-state! dir)
    (runner/atomic-write-edn! checkpoint-file (claim))
    (runner/atomic-write-edn!
     review-file {:checkpoint-digest (checkpoint/checkpoint-digest (claim))
                  :obligation-id :t00J02/producer
                  :ruling :reduced
                  :rationale "A distinct reviewer confirms reduction."
                  :approved? true
                  :reviewer-id "second-seat"})
    (is (= :settled
           (:status (exec/cli! ["reconsider-review" "t00J02"
                                (str checkpoint-file) (str review-file)]
                               {:root root}))))
    (is (= :review-pending (:phase (runner/read-state dir))))))

(deftest continue-without-bank-cli-records-disposition-without-effects
  (let [root (temp-dir)
        dir (exec/run-dir root "t00J02")]
    (init-at! dir :checkpoint-ready 20)
    (runner/install-checkpoint! dir (claim))
    (let [review (runner/begin-action! dir :review)
          identity (:pending-checkpoint (runner/read-state dir))]
      (runner/append-receipt!
       dir review (merge identity
                         {:outcome :approved :bank-authorized? true
                          :progress-ruling :reduced
                          :review-rationale "Approved partial progress."
                          :consecutive-nonreductions 0}))
      (runner/reconcile! dir (constantly nil)))
    (let [result (exec/cli! ["continue-without-bank" "t00J02"]
                            {:root root})
          state (runner/read-state dir)]
      (is (= :settled (:status result)))
      (is (= :continue-without-bank
             (get-in result [:receipt :action])))
      (is (= :continued-without-bank
             (get-in result [:receipt :result :outcome])))
      (is (= :turn-ready (:phase state)))
      (is (= 21 (:turn state)))
      (is (= 1 (:checkpoint state)))
      (is (= "base" (:base-sha state)))
      (is (= "candidate" (:head-sha state))))))

(defn- bank-ready! [dir]
  (let [base (assoc (runner/initial-state
                     {:problem-id "t00J02" :workspace "/tmp/apm-lean"
                      :base-sha "base" :head-sha "candidate"})
                    :phase :review-pending
                    :obligation/id :t00J02/producer)
        identity {:schema 1 :problem-id "t00J02" :turn 1 :checkpoint 0
                  :head-sha "candidate" :obligation-id :t00J02/producer
                  :checkpoint-digest (apply str (repeat 64 "a"))}]
    (runner/write-state! dir (assoc base :pending-checkpoint identity))))

(deftest bank-rebuilds-before-mutation-and-restart-does-not-duplicate-bank
  (let [dir (temp-dir)
        slate-file (java.io.File. dir "demonstrators.edn")
        calls (atom [])
        landed? (atom false)]
    (bank-ready! dir)
    (runner/atomic-write-edn!
     slate-file {:schema 1 :demonstrators
                 [{:problem-id "t00J02" :success? false}]})
    (exec/run-bank!
     dir {:observe observation
          :observe-bank (fn [_] (when @landed?
                                  {:landed? true :bank-sha "candidate"
                                   :ruling :partial-banked :status-sha "s1"}))
          :run-command (fn [command]
                         (swap! calls conj command)
                         (when (= "merge-exact" (first command)) (reset! landed? true))
                         {:exit 0 :stdout "ok" :stderr ""})
          :bank-command (fn [_] ["merge-exact" "candidate"])
          :status-command (fn [_] ["status-recompute" "t00J02"])
          :slate-path slate-file})
    (is (= [["lake" "build" "ConstructionTargets.A"]
            ["lake" "build" "ConstructionTargets.B"]
            ["lake" "env" "lean" "problems/t00J02/lean/Main.lean"]
            ["merge-exact" "candidate"]
            ["status-recompute" "t00J02"]]
           @calls))
    (is (false? (get-in (slate/read-slate slate-file)
                        [:demonstrators 0 :success?])))
    ;; A restarted bank with the exact head already landed records/settles it
    ;; and never invokes the mutation command.
    (let [restart-dir (temp-dir)
          mutations (atom 0)]
      (bank-ready! restart-dir)
      (exec/run-bank!
       restart-dir {:observe observation
                    :observe-bank (constantly {:landed? true
                                               :bank-sha "candidate"
                                               :ruling :closed
                                               :status-sha "s2"})
                    :run-command (fn [_] (swap! mutations inc)
                                   {:exit 0 :stdout "" :stderr ""})
                    :bank-command (constantly ["must-not-run"])
                    :status-command (constantly ["must-not-run"])} )
      (is (zero? @mutations))
      (is (= :turn-ready (:phase (runner/read-state restart-dir)))))))

(deftest slate-only-counts-closed-and-cli-init-status-resume-uses-fake-command
  (let [root (temp-dir)
        slate-file (java.io.File. root "slate.edn")]
    (runner/atomic-write-edn!
     slate-file {:schema 1 :demonstrators
                 [{:problem-id "t00J02" :success? false}]})
    (slate/apply-status! slate-file "t00J02"
                         {:ruling :partial-banked :status-sha "p"})
    (is (false? (get-in (slate/read-slate slate-file)
                        [:demonstrators 0 :success?])))
    (slate/apply-status! slate-file "t00J02"
                         {:ruling :closed :status-sha "c"})
    (is (true? (get-in (slate/read-slate slate-file)
                       [:demonstrators 0 :success?])))
    (exec/cli! ["init" "t00J02" "/tmp/work" "base" "head"] {:root root})
    (is (= :turn-ready (:phase (exec/cli! ["status" "t00J02"] {:root root}))))
    (exec/cli! ["resume" "t00J02"]
               {:root root
                :run-command (constantly {:exit 0 :stdout "turn" :stderr ""})
                :turn-command (constantly ["fake-codex" "one-turn"])
                :observe-head (constantly "head-2")})
    (is (= :gating (:phase (exec/cli! ["status" "t00J02"] {:root root}))))))

(defn- run-turn-result! [process-result]
  (let [root (temp-dir)]
    (exec/cli! ["init" "t00J02" "/tmp/work" "base" "head"] {:root root})
    (let [result (exec/cli!
                  ["resume" "t00J02"]
                  {:root root
                   :run-command (constantly process-result)
                   :turn-command (constantly
                                  ["codex" "exec" "resume"
                                   "01234567-89ab-cdef-0123-456789abcdef"
                                   "secret prompt"])
                   :observe-head (constantly "committed-head")})]
      {:root root :result result
       :state (runner/read-state (exec/run-dir root "t00J02"))})))

(deftest canonical-exit-zero-session-loss-is-a-bounded-turn-failure
  (let [{:keys [root result state]}
        (run-turn-result!
         {:exit 0 :cwd "/tmp/work" :stdout "arbitrary secret stdout"
          :stderr (str "2026-08-24T15:06:54Z ERROR codex_core::session: "
                       "failed to record rollout items: thread "
                       "01234567-89ab-cdef-0123-456789abcdef not found")})
        receipt (:receipt result)]
    (is (= :paused (:phase state)))
    (is (= "committed-head" (:head-sha state)))
    (is (= :turn-failed (get-in state [:pause/finding :type])))
    (is (= :codex-session-not-found (get-in receipt [:result :finding])))
    (is (= {:session-id "01234567-89ab-cdef-0123-456789abcdef"}
           (get-in receipt [:result :diagnostic])))
    (is (= {:exit 0 :cwd "/tmp/work"}
           (get-in receipt [:result :command])))
    (is (not (re-find #"secret|not found" (pr-str receipt))))
    ;; Restart observes settled state and never invokes the command again.
    (is (= :paused
           (:status (exec/cli! ["resume" "t00J02"]
                               {:root root
                                :run-command (fn [_]
                                               (throw (Exception. "duplicate")))}))))))

(deftest ordinary-output-cannot-fabricate-session-loss
  (doseq [result [{:exit 0 :stdout "proof says thread uuid not found" :stderr ""}
                  {:exit 0 :stdout "" :stderr (str "quoted: thread "
                                                   "01234567-89ab-cdef-0123-456789abcdef not found")}
                  {:exit 0 :stdout "ok" :stderr ""}]]
    (is (= :gating (:phase (:state (run-turn-result! result))))))
  (is (= :paused
         (:phase (:state (run-turn-result! {:exit 7 :stdout "" :stderr "failed"}))))))
