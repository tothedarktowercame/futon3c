(ns futon3c.inbox-zero.infer-adapters-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3.inbox-zero.infer :as infer]
            [futon3.inbox-zero.state :as state]
            [futon3c.inbox-zero.infer-adapters :as adapters]))

(def seat-id "seat:claude-3:7cdc25b0-2189-4f90-801e-c517e7f37d4d")
(def worktree-id "worktree:83d88d109136f3c8")
(def path-fact {:repo/id "futon3c-d" :worktree/id worktree-id
                :path "scripts/session-cost.py"})

(defn fixture-state []
  (state/replay
   [{:record/type :inbox-zero/session-seat :seat/id seat-id
     :agent/id "claude-3" :session/id "7cdc25b0-2189-4f90-801e-c517e7f37d4d"
     :surface :agent-tool-stream :host/id "zone"
     :workspace/root "/home/joe/code/futon3c"
     :observed-at #inst "2026-08-24T11:12:09Z"
     :registry-witness {:session/id "7cdc25b0-2189-4f90-801e-c517e7f37d4d"}}
    {:record/type :inbox-zero/session-file-claim :claim/id "claim:781b"
     :seat/id seat-id :repo/id "futon3" :worktree/id worktree-id
     :path "src/futon3c/watcher/multi.clj" :relation :edited-by-session
     :witness/type :tool-edit :witness/id "tool:one"
     :first-observed-at #inst "2026-08-24T11:12:34Z"
     :last-observed-at #inst "2026-08-24T11:12:34Z" :state :active}
    {:record/type :inbox-zero/file-observation :observation/id "observation:tracer"
     :repo/id "futon3c-d" :repo/root "/home/joe/code/futon3c"
     :worktree/id worktree-id :path "scripts/session-cost.py"
     :git/status :modified :content/hash "sha256:tracer" :head/sha "head"
     :observed-at #inst "2026-08-24T11:14:44Z" :source :multi-watcher}]))

(def entries
  [{:evidence/id "emacs-start" :evidence/at "2026-08-24T08:19:09.618Z"
    :evidence/author "claude-3" :evidence/body {:text "starting work"}}
   {:evidence/id "emacs-6427" :evidence/at "2026-08-24T08:21:54.580Z"
    :evidence/author "claude-3"
    :evidence/body {:text "Fixed futon3c/scripts/session-cost.py; uncommitted."}}
   {:evidence/id "emacs-end" :evidence/at "2026-08-24T08:59:06.817Z"
    :evidence/author "claude-3" :evidence/body {:text "done"}}])

(defn options [http-get]
  {:state-path "/unused/state.edn"
   :load-state-fn (fn [_] (fixture-state))
   :stat-fn (fn [absolute]
              {:at #inst "2026-08-24T08:21:27.874Z"
               :source/id (str "stat:" absolute)})
   :substrate-url "http://substrate"
   :http-get-fn http-get})

(defn response [xs]
  {:status 200 :body (pr-str {:entries xs})})

(deftest tracer-fixture-is-visible-but-insufficient-without-an-attested-write
  ;; Turn-text mentions are context, not attestation: the tracer's evidence
  ;; ranks the seat first at :weak, which the sweeper (no :allow-weak?) does
  ;; not propose. A structured/shell write is what would make it :direct.
  (let [requested (atom nil)
        bundle (adapters/build-evidence-bundle
                path-fact
                (options (fn [url _] (reset! requested url) (response entries))))
        result (infer/infer-attribution path-fact bundle)
        candidate (first (:candidates result))]
    (is (= :insufficient (:verdict result)))
    (is (= seat-id (:seat/id candidate)))
    (is (= 1 (:rank candidate)))
    (is (= :weak (:confidence candidate)))
    (is (false? (:attested? (first (:substrate-mentions bundle)))))
    (is (= :propose (:verdict (infer/infer-attribution path-fact bundle {:allow-weak? true}))))
    (is (= ["claim:781b"] (mapv :source/id (:same-worktree-claims bundle))))
    (is (= ["emacs-6427"] (mapv :source/id (:substrate-mentions bundle))))
    (is (= #inst "2026-08-24T08:19:09.618Z"
           (get-in bundle [:activity-windows 0 :from])))
    (is (= #inst "2026-08-24T08:59:06.817Z"
           (get-in bundle [:activity-windows 0 :to])))
    (is (str/includes? @requested "session-id=7cdc25b0-2189-4f90-801e-c517e7f37d4d"))
    (is (str/includes? @requested "since=2026-08-24T06%3A21%3A27.874Z"))
    (is (str/includes? @requested "before=2026-08-24T10%3A21%3A27.874Z"))))

(deftest substrate-failure-is-empty-and-does-not-throw
  (doseq [http-get [(fn [& _] (throw (ex-info "down" {})))
                    (fn [& _] {:status 503 :body "down"})]]
    (let [bundle (adapters/build-evidence-bundle path-fact (options http-get))]
      (is (= [] (:substrate-mentions bundle)))
      (is (= [] (:activity-windows bundle))))))

(deftest retrospective-mention-outside-query-window-is-excluded
  (let [retrospective {:evidence/id "later" :evidence/at "2026-08-24T12:00:00Z"
                       :evidence/author "claude-3"
                       :evidence/body {:text "scripts/session-cost.py was fixed"}}
        bundle (adapters/build-evidence-bundle
                path-fact (options (fn [& _] (response (conj entries retrospective)))))]
    (is (= ["emacs-6427"] (mapv :source/id (:substrate-mentions bundle))))
    (is (= #inst "2026-08-24T08:59:06.817Z"
           (get-in bundle [:activity-windows 0 :to])))))

(deftest other-author-is-retained-as-context-but-ignored-by-core
  (let [other (assoc (second entries) :evidence/author "claude-4")
        bundle (adapters/build-evidence-bundle
                path-fact (options (fn [& _] (response [(first entries) other
                                                        (last entries)]))))
        mention (first (:substrate-mentions bundle))
        result (infer/infer-attribution path-fact bundle)]
    (is (false? (:candidate-authored? mention)))
    (is (= :insufficient (:verdict result)))
    (is (= :weak (get-in result [:candidates 0 :confidence])))))

(deftest missing-stat-skips-substrate-and-produces-no-mtime
  (let [called? (atom false)
        bundle (adapters/build-evidence-bundle
                path-fact (assoc (options (fn [& _] (reset! called? true)))
                                 :stat-fn (constantly nil)))]
    (is (= [] (:mtimes bundle)))
    (is (= [] (:substrate-mentions bundle)))
    (is (= [] (:activity-windows bundle)))
    (is (false? @called?))))

;; --- evidence paging -------------------------------------------------------
;; The substrate serves one bounded page and returns :next-cursor when more
;; remain. This ignored it, so a session with more entries than the page limit
;; was silently truncated -- and the activity window is derived from the FIRST
;; and LAST entry, so a dropped tail reports a window the session never ended.

(defn- paged-response
  "A 200 whose body carries ENTRIES and, when CURSOR is given, :next-cursor."
  [xs cursor]
  {:status 200
   :body (pr-str (cond-> {:entries xs} cursor (assoc :next-cursor cursor)))})

(def ^:private page-one
  [{:evidence/id "p1-a" :evidence/at "2026-08-24T08:22:00.000Z"
    :evidence/author "claude-3" :evidence/body {:text "first"}}
   {:evidence/id "p1-b" :evidence/at "2026-08-24T08:30:00.000Z"
    :evidence/author "claude-3" :evidence/body {:text "second"}}])

(def ^:private page-two
  [{:evidence/id "p2-a" :evidence/at "2026-08-24T09:45:00.000Z"
    :evidence/author "claude-3" :evidence/body {:text "last"}}])

(deftest evidence-window-follows-the-cursor-to-its-real-end
  (let [urls (atom [])
        cursor {:at "2026-08-24T08:30:00.000Z" :id "p1-b"}
        http (fn [url _]
               (swap! urls conj url)
               (if (str/includes? url "cursor-id=")
                 (paged-response page-two nil)
                 (paged-response page-one cursor)))
        bundle (adapters/build-evidence-bundle path-fact (options http))
        window (first (:activity-windows bundle))]
    (is (= 2 (count @urls)) "the cursor must be followed exactly once")
    (is (str/includes? (second @urls) "cursor-at=")
        "continuation must pass cursor-at")
    (is (str/includes? (second @urls) "cursor-id=p1-b")
        "continuation must pass the cursor's id")
    (is (= #inst "2026-08-24T09:45:00.000Z" (:to window))
        "the window must end at page two's last entry, not page one's")
    (is (= #inst "2026-08-24T08:22:00.000Z" (:from window)))))

(deftest an-unpaged-window-still-costs-exactly-one-request
  ;; The ordinary case. Each request carries a fixed ~850ms floor at futon1b
  ;; and the permit pool is two, shared with the promotion's visibility reads,
  ;; so a page that nobody asked for is real contention.
  (let [urls (atom [])
        http (fn [url _] (swap! urls conj url) (paged-response page-one nil))
        bundle (adapters/build-evidence-bundle path-fact (options http))]
    (is (= 1 (count @urls)) "no cursor means no second request")
    (is (= #inst "2026-08-24T08:30:00.000Z"
           (:to (first (:activity-windows bundle)))))))

(deftest a-cursor-that-never-ends-is-bounded
  ;; A substrate that always offers another page must not loop forever.
  (let [calls (atom 0)
        http (fn [_ _]
               (swap! calls inc)
               (paged-response page-one {:at "2026-08-24T08:30:00.000Z"
                                         :id "always"}))]
    (adapters/build-evidence-bundle path-fact (options http))
    (is (<= @calls 20) "paging must stop at the page cap")
    (is (> @calls 1) "and it must actually have paged")))
