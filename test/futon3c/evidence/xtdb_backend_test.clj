(ns futon3c.evidence.xtdb-backend-test
  "Protocol compliance tests for XtdbBackend with in-memory XTDB node."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [xtdb.api :as xtdb]
            [futon3c.evidence.backend :as backend]
            [futon3c.evidence.xtdb-backend :as xb]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix])
  (:import [java.time Duration Instant]))

(def ^:dynamic *node* nil)
(def ^:dynamic *backend* nil)

(use-fixtures
  :each
  (fn [f]
    (let [node (xtdb/start-node {})]
      (try
        (binding [*node* node
                  *backend* (xb/make-xtdb-backend node)]
          (f))
        (finally
          (.close node))))))

(defn- append! [entry]
  (backend/-append *backend* entry))

(deftest append-and-get
  (testing "append stores entry in XTDB, -get retrieves it"
    (let [e (fix/make-evidence-entry {:evidence/id "e-1"})
          r (append! e)]
      (is (= true (:ok r)))
      (is (= e (:entry r)))
      (let [got (backend/-get *backend* "e-1")]
        (is (= "e-1" (:evidence/id got)))
        (is (= (:evidence/body e) (:evidence/body got)))
        ;; :xt/id should be stripped
        (is (nil? (:xt/id got)))))))

(deftest get-missing-returns-nil
  (testing "-get returns nil for nonexistent id"
    (is (nil? (backend/-get *backend* "nope")))))

(deftest exists-check
  (testing "-exists? returns true/false correctly"
    (is (false? (backend/-exists? *backend* "e-1")))
    (append! (fix/make-evidence-entry {:evidence/id "e-1"}))
    (is (true? (backend/-exists? *backend* "e-1")))))

(deftest duplicate-id-rejected
  (testing "append rejects duplicate evidence id"
    (append! (fix/make-evidence-entry {:evidence/id "e-dup"}))
    (let [r (append! (fix/make-evidence-entry {:evidence/id "e-dup"}))]
      (fix/assert-valid! shapes/SocialError r)
      (is (= :duplicate-id (:error/code r))))))

(deftest reply-not-found-rejected
  (testing "append rejects in-reply-to referencing missing entry"
    (let [e (fix/make-evidence-entry {:evidence/id "e-child"
                                      :evidence/in-reply-to "no-such"})
          r (append! e)]
      (fix/assert-valid! shapes/SocialError r)
      (is (= :reply-not-found (:error/code r))))))

(deftest fork-not-found-rejected
  (testing "append rejects fork-of referencing missing entry"
    (let [e (fix/make-evidence-entry {:evidence/id "e-fork"
                                      :evidence/fork-of "no-such"})
          r (append! e)]
      (fix/assert-valid! shapes/SocialError r)
      (is (= :fork-not-found (:error/code r))))))

(deftest reply-chain-works
  (testing "append allows valid in-reply-to"
    (append! (fix/make-evidence-entry {:evidence/id "e1"}))
    (let [r (append! (fix/make-evidence-entry {:evidence/id "e2"
                                               :evidence/in-reply-to "e1"}))]
      (is (= true (:ok r))))))

(deftest fork-works
  (testing "append allows valid fork-of"
    (append! (fix/make-evidence-entry {:evidence/id "base"}))
    (let [r (append! (fix/make-evidence-entry {:evidence/id "fork-1"
                                               :evidence/fork-of "base"}))]
      (is (= true (:ok r))))))

(deftest query-filters-by-subject
  (testing "-query filters by subject"
    (let [s1 (fix/make-artifact-ref :mission "M1")
          s2 (fix/make-artifact-ref :mission "M2")]
      (append! (fix/make-evidence-entry {:evidence/id "e1" :evidence/subject s1}))
      (append! (fix/make-evidence-entry {:evidence/id "e2" :evidence/subject s2}))
      (let [xs (backend/-query *backend* {:query/subject s1})]
        (is (= 1 (count xs)))
        (is (= s1 (:evidence/subject (first xs))))))))

(deftest query-filters-by-type
  (testing "-query filters by type"
    (append! (fix/make-evidence-entry {:evidence/id "e1" :evidence/type :reflection}))
    (append! (fix/make-evidence-entry {:evidence/id "e2" :evidence/type :presence-event}))
    (let [xs (backend/-query *backend* {:query/type :presence-event})]
      (is (= 1 (count xs)))
      (is (= :presence-event (:evidence/type (first xs)))))))

(deftest query-excludes-ephemeral-by-default
  (testing "-query excludes ephemeral entries by default"
    (append! (fix/make-evidence-entry {:evidence/id "e1"}))
    (append! (fix/make-evidence-entry {:evidence/id "e2" :evidence/ephemeral? true}))
    (let [xs (backend/-query *backend* {})]
      (is (= 1 (count xs)))
      (is (= "e1" (:evidence/id (first xs)))))))

(deftest query-includes-ephemeral-when-opted-in
  (testing "-query includes ephemeral when opted in"
    (append! (fix/make-evidence-entry {:evidence/id "e1"}))
    (append! (fix/make-evidence-entry {:evidence/id "e2" :evidence/ephemeral? true}))
    (let [xs (backend/-query *backend* {:query/include-ephemeral? true})]
      (is (= 2 (count xs))))))

(deftest query-newest-first
  (testing "-query returns results newest first"
    (let [older (str (Instant/ofEpochMilli 1000))
          newer (str (Instant/ofEpochMilli 2000))]
      (append! (fix/make-evidence-entry {:evidence/id "old" :evidence/at older}))
      (append! (fix/make-evidence-entry {:evidence/id "new" :evidence/at newer}))
      (let [xs (backend/-query *backend* {:query/include-ephemeral? true})]
        (is (= ["new" "old"] (mapv :evidence/id xs)))))))

(deftest query-with-limit
  (testing "-query respects limit"
    (append! (fix/make-evidence-entry {:evidence/id "e1" :evidence/at (str (Instant/ofEpochMilli 1000))}))
    (append! (fix/make-evidence-entry {:evidence/id "e2" :evidence/at (str (Instant/ofEpochMilli 2000))}))
    (append! (fix/make-evidence-entry {:evidence/id "e3" :evidence/at (str (Instant/ofEpochMilli 3000))}))
    (let [xs (backend/-query *backend* {:query/limit 2})]
      (is (= 2 (count xs))))))

(deftest query-with-limit-and-type-uses-pushdown
  (testing "limit is pushed after type filtering, not before it"
    (append! (fix/make-evidence-entry {:evidence/id "new-wrong"
                                       :evidence/type :presence-event
                                       :evidence/at (str (Instant/ofEpochMilli 4000))}))
    (append! (fix/make-evidence-entry {:evidence/id "new-match"
                                       :evidence/type :reflection
                                       :evidence/at (str (Instant/ofEpochMilli 3000))}))
    (append! (fix/make-evidence-entry {:evidence/id "old-match"
                                       :evidence/type :reflection
                                       :evidence/at (str (Instant/ofEpochMilli 2000))}))
    (with-redefs-fn {#'futon3c.evidence.xtdb-backend/query-all-entries
                     (fn [_]
                       (throw (ex-info "full scan should not be used by -query" {})))}
      (fn []
        (let [xs (backend/-query *backend* {:query/type :reflection
                                            :query/limit 1})]
          (is (= ["new-match"] (mapv :evidence/id xs))))))))

(deftest query-with-limit-and-tags-does-not-under-return
  (testing "tag membership is pushed before limit"
    (append! (fix/make-evidence-entry {:evidence/id "new-wrong"
                                       :evidence/tags [:other]
                                       :evidence/at (str (Instant/ofEpochMilli 4000))}))
    (append! (fix/make-evidence-entry {:evidence/id "new-match"
                                       :evidence/tags [:mission :backfill]
                                       :evidence/at (str (Instant/ofEpochMilli 3000))}))
    (append! (fix/make-evidence-entry {:evidence/id "old-match"
                                       :evidence/tags [:mission :backfill]
                                       :evidence/at (str (Instant/ofEpochMilli 2000))}))
    (let [xs (backend/-query *backend* {:query/tags [:mission :backfill]
                                        :query/limit 1})]
      (is (= ["new-match"] (mapv :evidence/id xs))))))

(deftest query-filters-by-since
  (testing "-query filters by since timestamp"
    (append! (fix/make-evidence-entry {:evidence/id "e1" :evidence/at (str (Instant/ofEpochMilli 1000))}))
    (append! (fix/make-evidence-entry {:evidence/id "e2" :evidence/at (str (Instant/ofEpochMilli 3000))}))
    (let [xs (backend/-query *backend* {:query/since (str (Instant/ofEpochMilli 2000))})]
      (is (= 1 (count xs)))
      (is (= "e2" (:evidence/id (first xs)))))))

(deftest forks-of
  (testing "-forks-of returns forked entries sorted by time"
    (append! (fix/make-evidence-entry {:evidence/id "base"
                                       :evidence/at (str (Instant/ofEpochMilli 1000))}))
    (append! (fix/make-evidence-entry {:evidence/id "fork-1"
                                       :evidence/fork-of "base"
                                       :evidence/at (str (Instant/ofEpochMilli 2000))}))
    (append! (fix/make-evidence-entry {:evidence/id "fork-2"
                                       :evidence/fork-of "base"
                                       :evidence/at (str (Instant/ofEpochMilli 3000))}))
    (append! (fix/make-evidence-entry {:evidence/id "unrelated"
                                       :evidence/at (str (Instant/ofEpochMilli 2500))}))
    (let [forks (backend/-forks-of *backend* "base")]
      (is (= ["fork-1" "fork-2"] (mapv :evidence/id forks))))))

(deftest delete-removes-entries
  (testing "-delete! removes entries and returns count"
    (append! (fix/make-evidence-entry {:evidence/id "e1"}))
    (append! (fix/make-evidence-entry {:evidence/id "e2"}))
    (append! (fix/make-evidence-entry {:evidence/id "e3"}))
    (let [r (backend/-delete! *backend* #{"e1" "e3"})]
      (is (= {:compacted 2} r)))
    (is (nil? (backend/-get *backend* "e1")))
    (is (some? (backend/-get *backend* "e2")))
    (is (nil? (backend/-get *backend* "e3")))))

(deftest all-returns-everything
  (testing "-all returns all entries including ephemeral"
    (append! (fix/make-evidence-entry {:evidence/id "e1"}))
    (append! (fix/make-evidence-entry {:evidence/id "e2" :evidence/ephemeral? true}))
    (let [all (backend/-all *backend*)]
      (is (= 2 (count all)))
      (is (= #{"e1" "e2"} (set (map :evidence/id all)))))))

;; ---------------------------------------------------------------------------
;; Recency-window fast path (limit without since)
;;
;; XTDB 1.x :order-by realizes ALL matching rows before sorting, so a bare
;; :query/limit full-scans the store (measured 15-20s against the live 85k
;; store). -query now probes a widening ladder of :evidence/at windows and
;; answers from the first window that yields a full page. These tests pin
;; the witness metadata, the widening/fallback semantics, and lucy-like
;; scale timing.
;; ---------------------------------------------------------------------------

(defn- hours-ago [h]
  (str (.minus (Instant/now) (Duration/ofMillis (long (* h 3600000))))))

(deftest recency-fast-path-witness-and-ephemeral-boundary
  (testing "bare limit query answers from the 1h window, ephemeral excluded"
    (append! (fix/make-evidence-entry {:evidence/id "r1" :evidence/at (hours-ago 0.1)}))
    (append! (fix/make-evidence-entry {:evidence/id "r-eph" :evidence/at (hours-ago 0.2)
                                       :evidence/ephemeral? true}))
    (append! (fix/make-evidence-entry {:evidence/id "r3" :evidence/at (hours-ago 0.3)}))
    (append! (fix/make-evidence-entry {:evidence/id "old" :evidence/at (hours-ago 50)}))
    (let [xs (backend/-query *backend* {:query/limit 2})]
      (is (= ["r1" "r3"] (mapv :evidence/id xs))
        "newest-first, ephemeral skipped WITHOUT under-filling the window")
      (is (= :recency-window (:evidence/fast-path (meta xs)))
          "witness: the fast path answered this query")
      (is (= 1 (:evidence/window-hours (meta xs)))))))

(deftest recency-fast-path-widens-until-page-fills
  (testing "an under-full window widens to the next rung"
    (append! (fix/make-evidence-entry {:evidence/id "a" :evidence/at (hours-ago 0.5)}))
    (append! (fix/make-evidence-entry {:evidence/id "b" :evidence/at (hours-ago 30)}))
    (append! (fix/make-evidence-entry {:evidence/id "c" :evidence/at (hours-ago 31)}))
    (let [xs (backend/-query *backend* {:query/limit 3})]
      (is (= ["a" "b" "c"] (mapv :evidence/id xs)))
      (is (= :recency-window (:evidence/fast-path (meta xs))))
      (is (= 96 (:evidence/window-hours (meta xs)))
          "1h/6h/24h rungs under-fill; the 96h rung holds all three"))))

(deftest recency-fast-path-falls-back-when-ladder-never-fills
  (testing "a store with fewer entries than limit returns them via the old path"
    (append! (fix/make-evidence-entry {:evidence/id "only-1" :evidence/at (hours-ago 1)}))
    (append! (fix/make-evidence-entry {:evidence/id "only-2" :evidence/at (hours-ago 2)}))
    (let [xs (backend/-query *backend* {:query/limit 5})]
      (is (= ["only-1" "only-2"] (mapv :evidence/id xs))
          "fallback preserves exact unbounded-query semantics")
      (is (nil? (:evidence/fast-path (meta xs)))))))

(defn- bulk-load!
  "Batch-put minimal evidence docs straight into the node (bypassing
   put-and-sync's per-doc await) — fixture loader for scale tests."
  [node docs]
  (let [tx (last (for [batch (partition-all 1000 docs)]
                   (xtdb/submit-tx node (mapv (fn [d] [::xtdb/put d]) batch))))]
    (xtdb/await-tx node tx)))

(deftest recency-fast-path-at-lucy-like-scale
  (testing "limit-without-since and the workshop mirror query stay fast at 20k entries"
    ;; 20k entries spread over 97 days + a dense 300-entry last-hour cluster
    ;; (the workshop 'mirror phase' projects limit=200&since=<1h ago>).
    (let [spread (for [i (range 20000)]
                   (let [id (str "bulk-" i)]
                     {:xt/id id :evidence/id id :evidence/author "bulk"
                      :evidence/type :reflection :evidence/claim-type :observation
                      :evidence/subject {:ref/type :mission :ref/id "SCALE"}
                      :evidence/body {:text "x"} :evidence/tags [:test]
                      :evidence/at (hours-ago (+ 1.5 (* i 0.1164)))}))
          recent (for [i (range 300)]
                   (let [id (str "recent-" i)]
                     {:xt/id id :evidence/id id :evidence/author "bulk"
                      :evidence/type :reflection :evidence/claim-type :observation
                      :evidence/subject {:ref/type :mission :ref/id "SCALE"}
                      :evidence/body {:text "x"} :evidence/tags [:test]
                      :evidence/at (hours-ago (* i 0.003))}))]
      (bulk-load! *node* (concat spread recent))
      (let [t0 (System/nanoTime)
            top3 (backend/-query *backend* {:query/limit 3})
            ms3 (/ (- (System/nanoTime) t0) 1e6)]
        (is (= ["recent-0" "recent-1" "recent-2"] (mapv :evidence/id top3)))
        (is (= :recency-window (:evidence/fast-path (meta top3)))
            "witness: bare limit=3 answered by a window probe, not a full scan")
        (is (< ms3 2000) (str "limit=3 took " ms3 "ms")))
      (let [t0 (System/nanoTime)
            mirror (backend/-query *backend* {:query/limit 200
                                              :query/since (hours-ago 1)})
            ms200 (/ (- (System/nanoTime) t0) 1e6)]
        (is (= 200 (count mirror)))
        (is (< ms200 2000) (str "limit=200&since=1h took " ms200 "ms"))))))

(deftest make-xtdb-backend-accepts-xtdb-store
  (testing "make-xtdb-backend extracts :node from a record with :node field"
    (let [fake-store (with-meta {:node *node*} {:type ::fake-store})
          ;; Use record-like map — make-xtdb-backend checks (:node store-or-node)
          b (xb/make-xtdb-backend fake-store)]
      (is (some? b))
      (is (nil? (backend/-get b "nope"))))))

(deftest store-integration-via-store-api
  (testing "XtdbBackend works through store.clj parameterised API"
    (require '[futon3c.evidence.store :as store])
    (let [store-ns (find-ns 'futon3c.evidence.store)
          append* (ns-resolve store-ns 'append*)
          query* (ns-resolve store-ns 'query*)
          get-entry* (ns-resolve store-ns 'get-entry*)
          get-reply-chain* (ns-resolve store-ns 'get-reply-chain*)]
      ;; append via store API
      (let [r (append* *backend*
                       {:evidence-id "int-1"
                        :subject (fix/make-artifact-ref :mission "INT")
                        :type :reflection
                        :claim-type :observation
                        :author "test-agent"
                        :body {:text "integration"}
                        :tags [:test]})]
        (is (= true (:ok r))))
      ;; get-entry via store API
      (let [e (get-entry* *backend* "int-1")]
        (is (= "int-1" (:evidence/id e))))
      ;; query via store API
      (let [xs (query* *backend* {})]
        (is (= 1 (count xs))))
      ;; reply chain via store API
      (append* *backend*
               {:evidence-id "int-2"
                :subject (fix/make-artifact-ref :mission "INT")
                :type :reflection
                :claim-type :step
                :author "test-agent"
                :body {:text "reply"}
                :in-reply-to "int-1"
                :tags [:test]})
      (let [chain (get-reply-chain* *backend* "int-2")]
        (is (= ["int-1" "int-2"] (mapv :evidence/id chain)))))))
