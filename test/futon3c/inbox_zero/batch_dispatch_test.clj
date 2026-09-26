(ns futon3c.inbox-zero.batch-dispatch-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agents.inbox-zero-board-live :as live]
            [futon3c.inbox-zero.batch-dispatch :as batch])
  (:import [java.time Instant] [java.util Date]))
(def now (Instant/parse "2026-09-14T21:00:00Z"))
(def cue {:id "test-dispatch" :operator "joe" :message "package these up in nice commits and push"
          :source "test:operator-message" :action :package-commit-and-push})
(defn state [n]
  {:records (into {} (for [i (range n)]
                      [(str i) {:record/type :inbox-zero/file-observation :observation/id (str i)
                                :repo/root "/unused/demo" :repo/id "demo" :worktree/id "wt"
                                :path (str "notes/" i ".md") :git/status :modified
                                :observed-at (Date/from (.minusSeconds now (* 48 3600)))}]))})
(defn opts [s records]
  {:dispatch cue :load-fn (constantly s) :now-fn (constantly now)
   :refresh-fn (fn [s _ _] {:state s}) :record! #(swap! records conj %)})
(deftest declared-threshold-counts-current-paths
  (is (= 10 live/batch-dirty-file-threshold))
  (doseq [n [0 9 10 11]]
    (let [p (live/batch-pressure (:records (state n)))]
      (is (= n (:dirty-files p)))
      (is (= (>= n 10) (:threshold-reached? p)))
      (is (:dispatch-required? p))))
  (let [records (:records (state 10))
        recent (assoc (get records "0") :observation/id "new" :observed-at (Date/from now))]
    (is (= 10 (:dirty-files (live/batch-pressure (assoc records "new" recent)))))
    (is (= 9 (:dirty-files (live/batch-pressure (assoc records "new" (assoc recent :git/status :clean))))))))
(deftest pressure-never-authorizes-a-batch
  (let [records (atom [])]
    (is (= :operator-dispatch-required
           (:refusal/reason (batch/prepare-batch!
                            (assoc (opts (state 20) records) :dispatch nil
                                   :load-fn (fn [_] (throw (Exception. "must not read"))))))))
    (is (= 1 (count @records)))))
(deftest dispatch-below-threshold-is-held
  (let [records (atom []) r (batch/prepare-batch! (opts (state 9) records))]
    (is (= :below-batch-threshold (:refusal/reason r)))
    (is (= [:inbox-zero/batch-dispatch :inbox-zero/refusal] (mapv :record/type @records)))))
(deftest dispatched-batch-does-not-bypass-atomicity
  (let [records (atom []) r (batch/prepare-batch! (opts (state 10) records))]
    (is (= :atomic-feel-commit-unavailable (get-in r [:blocker :refusal/reason])))
    (is (empty? (:committed r)))
    (is (empty? (:pushed r)))
    (is (= :blocked (:push/status r)))
    (is (:push/requested? r))
    (is (= 10 (count (mapcat :paths (:held r)))))
    (is (= :content-review (get-in r [:held 0 :kind])))
    (is (= cue (:dispatch r)))
    (is (true? (get-in r [:board/run :certificate :verified?])))
    (is (= r (last @records)))))
(deftest recheck-catches-new-activity
  (let [records (atom []) s (state 10)
        r (batch/prepare-batch!
           (assoc (opts s records) :refresh-fn
                  (fn [s _ _] {:state (assoc-in s [:records "0" :observed-at] (Date/from now))})))]
    (is (every? #(= :in-flight (:refusal/reason %)) (:held r)))
    (is (empty? (:committed r)))))
(deftest janitorial-is-a-disposition-not-an-author-claim
  (let [s (assoc-in (state 1) [:records "0" :path] "checks/__pycache__/x.pyc")
        p (first (batch/packages s cue now))]
    (is (= :janitorial (:kind p)))
    (is (nil? (:seat/id p)))
    (is (= :generated-artifacts-need-disposition (:review/reason p)))
    (is (= (:id cue) (:dispatch/id p)))))
