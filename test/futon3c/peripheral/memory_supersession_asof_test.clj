(ns futon3c.peripheral.memory-supersession-asof-test
  "M-typed-memories P2 demonstration: supersession gives correct current and
   as-of answers while the correction event is retained.

   The graph fixture is a minimal bitemporal store: each post adds a version
   stamped with the edge's valid-time, and fetch with :valid-as-of returns
   the version whose valid-time is the latest at or before that instant —
   the same question the real substrate answers over its hyperedge history.
   The lifecycle and recall code under test is the production code; only the
   transport is in-memory, as in pattern-memory-phase3-test."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.memory-lifecycle :as lifecycle]
            [futon3c.peripheral.memory-recall :as recall])
  (:import [java.time Instant]))

(def ctx
  {:agent-id "zai-p2"
   :session-id "p2-supersession"
   :domain :mathematics})

(def endpoint "math-formalization/tactic-algebra-interference")

(def original-entry
  {:evidence/id "e-original"
   :evidence/subject {:ref/type :problem :ref/id "topology/old"}
   :evidence/type :memory
   :evidence/claim-type :assert
   :evidence/author "zai-earlier"
   :evidence/session-id "earlier-session"
   :evidence/at "2026-07-20T00:00:00Z"
   :evidence/body {:hook "Use the old topology."}
   :evidence/tags [:memory :memory/assert]})

(def correcting-entry
  {:evidence/id "e-correcting"
   :evidence/subject {:ref/type :problem :ref/id "topology/old"}
   :evidence/type :memory
   :evidence/claim-type :assert
   :evidence/author "zai-p2"
   :evidence/session-id "p2-supersession"
   :evidence/at "2026-08-01T00:00:00Z"
   :evidence/body {:hook "Use the corrected topology."}
   :evidence/tags [:memory :memory/assert]})

(defn- edge-for
  [id entry-id state valid-time]
  {:hx/id id
   :hx/type :memory/assert
   :hx/endpoints [entry-id "topology/old" endpoint]
   :hx/valid-time valid-time
   :hx/props
   {:domain :mathematics
    :state state
    :attachment-status :reviewed
    :witness-status :self-asserted
    :valid-time valid-time
    :roles {:entry entry-id
            :subjects ["topology/old" endpoint]
            :patterns [endpoint]}}})

(defn- valid-instant
  [edge]
  (or (some-> (:hx/valid-time edge) Instant/parse)
      ;; no valid-time: valid since the epoch, like an unversioned insert.
      Instant/EPOCH))

(defn- bitemporal-graph
  "Versions per hyperedge id. Fetch without :valid-as-of returns each edge's
   newest version; fetch with :valid-as-of T returns the version that was
   current at T (the latest version whose valid-time is <= T), or omits the
   edge entirely when its first version postdates T."
  [initial]
  (let [versions (atom (into {} (map (juxt :hx/id (fn [e] [e]))) initial))]
    {:versions versions
     :fetch
     (fn [endpoint* {:keys [type limit valid-as-of]}]
       (let [as-of (some-> valid-as-of Instant/parse)]
         (->> (vals @versions)
              (mapcat
               (fn [vs]
                 (when-let [v (if as-of
                                (->> (sort-by valid-instant vs)
                                     ;; keep versions whose valid-time
                                     ;; is at or before the as-of instant
                                     (filter #(not (neg? (compare as-of
                                                                  (valid-instant %)))))
                                     last)
                                (last (sort-by valid-instant vs)))]
                   [v])))
              (filter #(and (= type (:hx/type %))
                            (some #{endpoint*} (:hx/endpoints %))))
              (take (or limit 10))
              vec)))
     :post
     (fn [_ edge]
       (swap! versions update (:hx/id edge) (fnil conj []) edge)
       {:ok true :hyperedge edge})}))

(defn- recall-opts [graph store]
  {:fetch-hyperedges (:fetch graph)
   :fetch-entry #(estore/get-entry* store %)})

(deftest supersession-current-and-as-of-answers-retain-the-correction-event
  (let [store (atom {:entries {"e-original" original-entry}
                     :order ["e-original"]})
        original-valid "2026-07-20T00:00:00Z"
        correction-valid "2026-08-01T00:00:00Z"
        graph (bitemporal-graph
               [(edge-for "hx-original" "e-original" :current original-valid)])
        record-memory
        (fn [_ _]
          (swap! store
                 (fn [s] (-> s
                             (assoc-in [:entries "e-correcting"] correcting-entry)
                             (update :order conj "e-correcting"))))
          ((:post graph)
           nil (edge-for "hx-correcting" "e-correcting" :current correction-valid))
          {:ok true :id "e-correcting" :hx-id "hx-correcting"})
        result
        (lifecycle/supersede-memory!
         (assoc ctx :evidence-store store)
         {:memory-id "e-original"
          :reason "The topology changed; the old hook is wrong."
          :valid-from (Instant/parse correction-valid)
          :correction
          {:name "corrected-topology"
           :hook "Use the corrected topology."
           :kind :feedback
           :body "The service moved."}}
         {:fetch-hyperedges (:fetch graph)
          :post-hyperedge (:post graph)
          :record-memory record-memory})]
    (is (true? (:ok result)) result)

    (testing "current recall returns the correcting memory, not the original"
      (let [current (recall/recall-by-endpoint
                     {:domain :mathematics :evidence-store store}
                     endpoint
                     (recall-opts graph store))]
        (is (:ok current))
        (is (= :current (get-in current [:temporal-basis :mode])))
        (is (= ["e-correcting"] (mapv :memory/id (:memories current))))
        (is (= 1 (:state-excluded (:audit current))))))

    (testing "as-of recall before the correction still answers with the original"
      (let [before (recall/recall-by-endpoint
                    {:domain :mathematics :evidence-store store}
                    endpoint
                    (assoc (recall-opts graph store)
                           :valid-as-of "2026-07-25T00:00:00Z"))]
        (is (:ok before))
        (is (= :as-of (get-in before [:temporal-basis :mode])))
        (is (= "2026-07-25T00:00:00Z"
               (get-in before [:temporal-basis :valid-as-of])))
        (is (= ["e-original"] (mapv :memory/id (:memories before))))))

    (testing "as-of recall after the correction answers with the correcting memory"
      (let [after (recall/recall-by-endpoint
                   {:domain :mathematics :evidence-store store}
                   endpoint
                   (assoc (recall-opts graph store)
                          :valid-as-of "2026-08-02T00:00:00Z"))]
        (is (= ["e-correcting"] (mapv :memory/id (:memories after))))))

    (testing "the correction event is retained, and nothing was erased"
      (let [event (estore/get-entry* store (:challenge-id result))]
        (is (some? event))
        (is (= :challenge (:evidence/claim-type event)))
        (is (= :superseded (get-in event [:evidence/body :memory/action])))
        (is (= "e-correcting"
               (get-in event [:evidence/body :memory/correcting-memory-id])))
        (is (= "e-original" (get-in event [:evidence/body :memory/id])))
        (is (pos? (get-in event [:evidence/body :memory/correction-lag-ms]))))
      (is (= original-entry (estore/get-entry* store "e-original")))
      (is (some? (estore/get-entry* store "e-correcting"))))

    (testing "the original edge's superseded version is a retained version, not a rewrite"
      (let [hx-versions (get @(:versions graph) "hx-original")]
        (is (= 2 (count hx-versions)))
        (is (= :current (:state (get-in (first hx-versions) [:hx/props]))))
        (is (= :superseded (:state (get-in (second hx-versions) [:hx/props]))))
        (is (= "e-correcting"
               (get-in (second hx-versions) [:hx/props :superseded-by])))))))
