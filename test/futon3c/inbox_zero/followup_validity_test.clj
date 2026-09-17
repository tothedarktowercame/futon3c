(ns futon3c.inbox-zero.followup-validity-test
  (:require [clojure.test :refer [deftest is]]
            [futon3.inbox-zero.state :as state]
            [futon3c.inbox-zero.followup-validity :as validity]))

(def now #inst "2026-08-26T10:00:00Z")
(def worktree "worktree:one")
(def seat "seat:claude-3:session-1")

(defn store [status claimed?]
  (state/replay
   (cond->
    [{:record/type :inbox-zero/session-seat :seat/id seat
      :agent/id "claude-3" :session/id "session-1" :surface :test
      :host/id "test" :workspace/root "/repo" :observed-at now
      :registry-witness {:source :test :session/id "session-1"}}
     {:record/type :inbox-zero/file-observation
      :observation/id "obs" :repo/id "repo" :repo/root "/repo"
      :worktree/id worktree :path "a.clj" :git/status status
      :content/hash "hash" :head/sha "head" :observed-at now :source :test}]
     claimed?
     (conj {:record/type :inbox-zero/session-file-claim :claim/id "claim"
            :seat/id seat :repo/id "repo" :worktree/id worktree :path "a.clj"
            :relation :edited-by :witness/type :test :state :active
            :witness/id "witness" :observed-at now
            :first-observed-at now :last-observed-at now}))))

(def attribution-item
  {:agent "claude-3" :session "session-1"
   :metadata {:proposal/type "inbox-zero/attribution"
              :path/key {:repo/id "repo" :worktree/id worktree :path "a.clj"}}})

(deftest attribution-must-remain-unattributed
  (is (true? (validity/still-current? attribution-item
                                      {:load-state-fn (constantly (store :modified false))
                                       :now-fn (constantly now)})))
  (is (false? (validity/still-current? attribution-item
                                       {:load-state-fn (constantly (store :clean false))
                                        :now-fn (constantly now)}))))

(deftest promotion-seat-must-retain-dirty-set
  (let [item {:agent "claude-3" :session "session-1"
              :metadata {:route-tier 1 :repo-id "repo" :worktree-id worktree}}]
    (is (true? (validity/still-current? item
                                        {:load-state-fn (constantly (store :modified true))
                                         :now-fn (constantly now)})))
    (is (false? (validity/still-current? item
                                         {:load-state-fn (constantly (store :clean true))
                                          :now-fn (constantly now)})))))

(deftest unknown-and-errors-fail-open
  (is (true? (validity/still-current? {:metadata {:other true}})))
  (let [printed (atom [])]
    (is (true? (validity/still-current?
                attribution-item
                {:load-state-fn (fn [_] (throw (ex-info "boom" {})))
                 :print-fn #(swap! printed conj %)})))
    (is (= 1 (count @printed)))))

(deftest validator-loads-once
  (let [loads (atom 0)
        current? (validity/validator
                  {:load-state-fn (fn [_] (swap! loads inc) (store :modified false))
                   :now-fn (constantly now)})]
    (is (true? (current? attribution-item)))
    (is (true? (current? attribution-item)))
    (is (= 1 @loads))))

(deftest store-repair-requires-the-exact-still-held-queue
  (let [item {:type :apm-store-repair :metadata {:hold-id "h" :queue-state-path "/fixture"}}]
    (is (true? (validity/still-current? item {:load-queue-fn (constantly {:store-read/hold {:hold/id "h"}})})))
    (is (false? (validity/still-current? item {:load-queue-fn (constantly {:store-read/hold {:hold/id "other"}})})))
    (is (false? (validity/still-current? item {:load-queue-fn (constantly {})})))
    (is (false? (validity/still-current? item {:load-queue-fn (fn [_] (throw (ex-info "unreadable" {})))
                                             :print-fn (constantly nil)})))))

(deftest a-commit-notice-is-cancelled-once-the-repo-is-clean
  (let [item {:type :inbox-zero :agent "claude-7" :session "s1"
              :metadata {:proposal/type :inbox-zero/commit-notice
                         :repo-id "futon3c-d" :dirty-count 12}}
        opts {:roots [{:path "/repo/futon3c-d" :label "futon3c-d"}]
              :threshold 10}]
    (is (false? (validity/still-current?
                 item (assoc opts :git-fn (constantly [])))))
    (is (false? (validity/still-current?
                 item (assoc opts :git-fn (constantly (repeat 9 {:path "a"}))))))
    (is (true? (validity/still-current?
                item (assoc opts :git-fn (constantly (repeat 10 {:path "a"}))))))))

(deftest a-commit-notice-for-an-unwatched-repo-is-cancelled
  (is (false? (validity/still-current?
               {:type :inbox-zero
                :metadata {:proposal/type :inbox-zero/commit-notice
                           :repo-id "not-watched"}}
               {:roots [] :git-fn (constantly (repeat 50 {:path "a"}))}))))
