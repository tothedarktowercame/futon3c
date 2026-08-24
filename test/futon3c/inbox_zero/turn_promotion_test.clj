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

(deftest propose-delivers-would-message-without-execute-or-push
  (let [calls (atom [])
        result (sut/promote-at-turn-end!
                "a" "s" (assoc (opts calls) :mode :propose
                                 :execute-fn (fn [& _] (swap! calls conj :execute))
                                 :push-fn (fn [& _] (swap! calls conj :push))))
        payload (second (first (filter vector? @calls)))]
    (is (= :propose (:mode result)))
    (is (not-any? #{:execute :push} @calls))
    (is (= "inbox-zero would promote 1 path(s) in repo: a.clj" (:prompt payload)))))

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
    (is (= 1 (count (filter #(and (vector? %) (= :ledger (first %))) @calls))))))

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
        options (assoc (opts calls) :mode :propose
                       :plan-fn (fn [& _] [proposed second-plan])
                       :deliver! (fn [payload]
                                   (swap! calls conj [:attempt (:repo-id (:metadata payload))])
                                   {:status (if (= "repo" (:repo-id (:metadata payload))) 503 200)}))]
    (is (map? (sut/promote-at-turn-end! "a" "s" options)))
    (is (= 2 (count (filter #(and (vector? %) (= :attempt (first %))) @calls))))
    (is (some #(and (vector? %) (= :print (first %))) @calls))))

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
