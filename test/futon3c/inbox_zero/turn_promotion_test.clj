(ns futon3c.inbox-zero.turn-promotion-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.inbox-zero.turn-promotion :as sut]))

(def proposed {:record/type :inbox-zero/promotion-plan
               :seat/id "seat:a:s" :repo/id "repo" :worktree/id "wt"
               :include [{:path "a.clj" :git/status :modified}] :exclude []
               :verdict :proposed :held/reason nil})

(defn opts [calls]
  {:state-path "/unused/state.edn"
   :now-fn #(java.util.Date. 1)
   :load-state-fn (fn [_] (swap! calls conj :load) {:records {}})
   :plan-fn (fn [_ _ _] [proposed])
   :screen-fn (fn [plan _] plan)
   :size-fn (fn [_ _] nil)
   :roster-fn (fn [] #{"seat:a:s"})
   :route-fn (fn [items _]
               (mapv #(hash-map :route/tier 1 :route/recipient "seat:a:s"
                                :route/item % :route/reason (:held/reason %)
                                :route/message "route") items))
   :deliver! (fn [payload] (swap! calls conj [:deliver payload]) {:status 200})
   :ledger-fn (fn [_ decision] (swap! calls conj [:ledger decision]))
   :print-fn (fn [line] (swap! calls conj [:print line]))})

(deftest off-invokes-no-collaborator
  (let [calls (atom [])]
    (is (nil? (sut/promote-at-turn-end! "a" "s"
                                        (assoc (opts calls) :mode :off))))
    (is (empty? @calls))))

(deftest propose-ledgers-would-message-and-delivers-nothing
  ;; Propose mode is a dry run for the operator's ledger; seats hear nothing.
  (let [calls (atom [])
        result (sut/promote-at-turn-end!
                "a" "s" (assoc (opts calls) :mode :propose
                                 :execute-fn (fn [& _] (swap! calls conj :execute))
                                 :push-fn (fn [& _] (swap! calls conj :push))))
        ledgered (filter #(and (vector? %) (= :ledger (first %))) @calls)]
    (is (= :propose (:mode result)))
    (is (not-any? #{:execute :push} @calls))
    (is (not-any? #(and (vector? %) (= :deliver (first %))) @calls))
    (is (= 1 (count ledgered)))
    (is (= "inbox-zero would promote 1 path(s) in repo: a.clj"
           (get-in (first ledgered) [1 :route/message])))
    (is (= :ledger-only (get-in (first ledgered) [1 :delivery])))))

(deftest propose-held-nothing-promotable-does-not-invoke-agent
  (let [calls (atom [])
        held (assoc proposed
                    :include []
                    :exclude [{:path "c.clj" :reason :unattributed}
                              {:path "a.clj" :reason :unattributed}
                              {:path "owned.clj" :reason :other-seat}
                              {:path "b.clj" :reason :unattributed}]
                    :verdict :held :held/reason :nothing-promotable)]
    (sut/promote-at-turn-end!
     "a" "s" (assoc (opts calls) :mode :propose
                      :plan-fn (fn [& _] [held])))
    (is (not-any? #(and (vector? %) (= :deliver (first %))) @calls))))

(deftest propose-held-nothing-promotable-is-not-routed
  (let [calls (atom [])
        held (assoc proposed
                    :include []
                    :exclude (mapv #(hash-map :path (str "p" % ".clj")
                                              :reason :unattributed)
                                   (range 1 8))
                    :verdict :held :held/reason :nothing-promotable)
        result (sut/promote-at-turn-end!
                "a" "s" (assoc (opts calls) :mode :propose
                                 :plan-fn (fn [& _] [held])))]
    (is (= [held] (:plans result)) "the diagnostic plan remains observable")
    (is (empty? (:decisions result)))
    (is (not-any? #(and (vector? %) (= :deliver (first %))) @calls))))

(deftest propose-labelled-plan-with-zero-includes-does-not-invoke-agent
  (let [calls (atom [])
        empty-proposal (assoc proposed :include [] :exclude [])
        result (sut/promote-at-turn-end!
                "a" "s" (assoc (opts calls) :mode :propose
                                 :plan-fn (fn [& _] [empty-proposal])))]
    (is (= [empty-proposal] (:plans result)))
    (is (empty? (:decisions result)))
    (is (not-any? #(and (vector? %) (= :deliver (first %))) @calls))))

(deftest propose-sensitive-plan-goes-to-tier-three-ledger
  (let [calls (atom [])
        sensitive (assoc proposed :verdict :held :held/reason :sensitive-content
                         :sensitive/hits [{:path "secret.pem" :rule/kind :key-material}])]
    (sut/promote-at-turn-end!
     "a" "s" (assoc (opts calls) :mode :propose
                      :screen-fn (fn [_ _] sensitive)
                      :route-fn (fn [items _]
                                  [{:route/tier 3 :route/recipient "joe"
                                    :route/item (first items)
                                    :route/reason :sensitive-content
                                    :route/message "hold"}])
                      :execute-fn (fn [& _] (swap! calls conj :execute))))
    (is (not-any? #{:execute} @calls))
    (let [ledgered (filter #(and (vector? %) (= :ledger (first %))) @calls)]
      (is (= 1 (count ledgered)))
      (is (= "hold" (get-in (first ledgered) [1 :route/message]))))))

(deftest execute-commits-then-pushes-in-order-with-no-routing
  (let [calls (atom [])
        options (assoc (opts calls) :mode :execute
                       :clock-fn (fn [_ _] {:mission-id "M-test"})
                       :execute-fn (fn [_ execution]
                                     (swap! calls conj [:execute (:message execution)])
                                     {:verdict :committed :commit/sha "abc"})
                       :push-fn (fn [_]
                                  (swap! calls conj :push)
                                  {:verdict :pushed}))
        result (sut/promote-at-turn-end! "a" "s" options)]
    (is (= [:execute :push]
           (->> @calls (keep #(cond (and (vector? %) (= :execute (first %))) :execute
                                    (= :push %) :push)) vec)))
    (is (re-find #"Mission: M-test" (second (first (filter #(and (vector? %)
                                                                   (= :execute (first %)))
                                                             @calls)))))
    (is (empty? (:decisions result)))))

(deftest held-plan-with-dead-seat-skips-execute-and-ledgers-tier-two
  (let [calls (atom [])
        held (assoc proposed :verdict :held :held/reason :nothing-promotable)]
    (sut/promote-at-turn-end!
     "a" "s" (assoc (opts calls) :mode :execute
                      :screen-fn (fn [_ _] held)
                      :roster-fn (fn [] #{})
                      :route-fn (fn [items _]
                                  [{:route/tier 2 :route/recipient "sweeper"
                                    :route/item (first items)
                                    :route/reason :nothing-promotable
                                    :route/message "sweep"}])
                      :execute-fn (fn [& _] (swap! calls conj :execute))))
    (is (not-any? #{:execute} @calls))
    (is (= 1 (count (filter #(and (vector? %) (= :ledger (first %))) @calls))))))

(deftest failed-tier-one-delivery-is-loud-and-next-decision-continues
  (let [calls (atom [])
        second-plan (assoc proposed :repo/id "repo-2")
        options (assoc (opts calls) :mode :execute
                       :plan-fn (fn [& _] [proposed second-plan])
                       :execute-fn (fn [plan _]
                                     (assoc plan :verdict :held :held/reason :gate-failed))
                       :deliver! (fn [payload]
                                   (swap! calls conj [:attempt (:repo-id (:metadata payload))])
                                   {:status (if (= "repo" (:repo-id (:metadata payload))) 503 200)}))]
    (is (map? (sut/promote-at-turn-end! "a" "s" options)))
    (is (= 2 (count (filter #(and (vector? %) (= :attempt (first %))) @calls))))
    (is (some #(and (vector? %) (= :print (first %))) @calls))))

(deftest execute-drops-empty-include-diagnostics-without-delivery-or-ledger
  (let [calls (atom [])
        held (assoc proposed :include []
                    :exclude [{:path "x.clj" :reason :unattributed}]
                    :verdict :held :held/reason :nothing-promotable)
        result (sut/promote-at-turn-end!
                "a" "s" (assoc (opts calls) :mode :execute
                                 :screen-fn (fn [_ _] held)
                                 :execute-fn (fn [& _] (swap! calls conj :execute))))]
    (is (not-any? #{:execute} @calls))
    (is (empty? (:decisions result)))
    (is (= 1 (:diagnostic result)))
    (is (not-any? #(and (vector? %) (#{:deliver :ledger} (first %))) @calls))))

(deftest tier-one-dedupe-key-ignores-membership
  (let [calls (atom [])
        options (assoc (opts calls) :mode :execute
                       :execute-fn (fn [plan _]
                                     (assoc plan :verdict :held :held/reason :gate-failed)))
        _ (sut/promote-at-turn-end! "a" "s" options)
        payload (second (first (filter #(and (vector? %) (= :deliver (first %))) @calls)))]
    (is (= ["seat:a:s" "repo" "wt" :gate-failed] (:dedupe-key payload)))))

(deftest launch-debounces-many-calls-into-one-run-after-the-seat-goes-idle
  (let [runs (atom [])
        busy (atom 2)                       ; first two checks: still invoking
        opts* {:quiet-ms 15
               :invoking?-fn (fn [_] (pos? (swap! busy dec)))
               :run-fn (fn [agent session options] (swap! runs conj [agent session options]))
               :print-fn (fn [_])
               :run-options {:mode :off}}
        futures (doall (repeatedly 3 #(sut/launch-at-turn-end! "a" "s" opts*)))]
    (is (= [:superseded :superseded [["a" "s" {:mode :off}]]]
           [(deref (first futures) 2000 :timeout)
            (deref (second futures) 2000 :timeout)
            (do (deref (last futures) 2000 :timeout) @runs)]))
    (is (= 1 (count @runs)) "three calls, one promotion, after the seat went idle")))

(deftest launch-gives-up-loudly-when-the-seat-never-goes-idle
  (let [runs (atom []) printed (atom [])
        f (sut/launch-at-turn-end! "b" "s" {:quiet-ms 5 :max-wait-ms 20
                                            :invoking?-fn (fn [_] true)
                                            :run-fn (fn [& _] (swap! runs conj :run))
                                            :print-fn (fn [line] (swap! printed conj line))})]
    (is (= :gave-up (deref f 2000 :timeout)))
    (is (empty? @runs))
    (is (re-find #"gave up" (first @printed)))))

(deftest collaborator-exception-never-propagates
  (let [calls (atom [])
        result (sut/promote-at-turn-end!
                "a" "s" (assoc (opts calls) :mode :execute
                                 :load-state-fn (fn [_] (throw (ex-info "boom" {})))))]
    (is (= "boom" (.getMessage (:error result))))
    (is (some #(and (vector? %) (= :print (first %))) @calls))))

(deftest escalation-ledger-append-is-additive-and-readable
  (let [dir (java.nio.file.Files/createTempDirectory
             "promotion-ledger-" (make-array java.nio.file.attribute.FileAttribute 0))
        path (.getPath (io/file (.toFile dir) "ledger.edn"))]
    (sut/append-escalation! path {:route/tier 2 :id 1})
    (sut/append-escalation! path {:route/tier 3 :id 2})
    (is (= [{:route/tier 2 :id 1} {:route/tier 3 :id 2}]
           (edn/read-string (slurp path))))))

(deftest incomplete-seat-is-refused-before-any-planning
  ;; A plan is computed for `seat:<agent>:<session>` and includes a path only
  ;; when a claim carries that exact string, so a blank half can only ever
  ;; produce a hold on nothing. Verified on zone: 3,263 such plans, 0 includes.
  (doseq [[agent session reason] [["a" nil :no-session-id]
                                  ["a" "" :no-session-id]
                                  ["a" "   " :no-session-id]
                                  [nil "s" :no-agent-id]
                                  [nil nil :no-seat]]]
    (let [calls (atom [])
          result (sut/promote-at-turn-end!
                  agent session (assoc (opts calls) :mode :propose))]
      (is (= :refused (:verdict result)) (pr-str [agent session]))
      (is (= reason (:reason result)) (pr-str [agent session]))
      ;; nothing was loaded, planned, routed or delivered
      (is (not-any? #{:load} @calls))
      (is (not-any? #(and (vector? %) (= :deliver (first %))) @calls)))))

(deftest refusal-is-ledgered-once-and-printed-with-a-count
  (let [calls (atom [])
        options (assoc (opts calls) :mode :propose)
        seat "seat:refusal-probe-agent:"]
    (dotimes [_ 3] (sut/promote-at-turn-end! "refusal-probe-agent" nil options))
    (let [ledgered (filter #(and (vector? %) (= :ledger (first %))) @calls)
          printed  (filter #(and (vector? %) (= :print (first %))) @calls)]
      (is (= 1 (count ledgered)) "ledgered once per (agent, reason) per process")
      (is (= :inbox-zero/refusal (:record/type (second (first ledgered)))))
      (is (= seat (:refusal/seat-id (second (first ledgered)))))
      (is (= 1 (count printed)) "printed on the first occurrence only")
      (is (re-find #"REFUSED" (second (first printed)))))))

(deftest complete-seat-still-plans
  (let [calls (atom [])
        result (sut/promote-at-turn-end! "a" "s" (assoc (opts calls) :mode :propose))]
    (is (nil? (:verdict result)))
    (is (= :propose (:mode result)))
    (is (some #{:load} @calls))))

(deftest live-mode-override-applies-to-option-less-calls-and-clears
  ;; Zone's serving JVM reads FUTON3C_INBOX_ZERO_PROMOTION once at boot; the
  ;; override is how the operator flips modes without a restart.
  (try
    (let [calls (atom [])]
      (is (= :propose (sut/set-mode! :propose)))
      (is (= :propose (:mode (sut/promote-at-turn-end! "a" "s" (opts calls)))))
      ;; an explicit :mode still wins over the override
      (is (nil? (sut/promote-at-turn-end! "a" "s" (assoc (opts calls) :mode :off))))
      (is (= :off (sut/set-mode! "nonsense")) "unknown modes resolve to :off"))
    (finally
      (sut/set-mode! nil)
      (is (nil? @@#'sut/!mode-override)))))
