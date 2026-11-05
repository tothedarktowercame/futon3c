(ns futon3c.agency.promise-log-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.promise-log :as promise-log]))

(def entry
  {:evidence/id "promise-history:123e4567-e89b-12d3-a456-426614174000"
   :evidence/type :promise/dependency-terminated
   :evidence/author "agent-7"
   :evidence/at "2026-11-05T12:00:00Z"
   :evidence/body {:history/format 3
                   :history/payload-edn "{:record {:id \"park-7\"}}"
                   :history/promise-id "park-7"
                   :history/promise-sequence 2
                   :history/predecessor {:sequence 1
                                         :id "promise-history:previous"
                                         :type :promise/park-made}}})

(deftest prepared-entry-has-stable-lossless-hyperedge
  (is (= {:hx/id "promise-log:123e4567-e89b-12d3-a456-426614174000"
          :hx/type :promise/history-event
          :hx/endpoints ["park-7" "agent-7"]
          :hx/valid-time "2026-11-05T12:00:00Z"
          :hx/props {:history/payload-edn "{:record {:id \"park-7\"}}"
                     :history/promise-id "park-7"
                     :history/promise-sequence 2
                     :history/predecessor {:sequence 1
                                           :id "promise-history:previous"
                                           :type :promise/park-made}
                     :history/transition-type :promise/dependency-terminated
                     :history/event-time "2026-11-05T12:00:00Z"
                     :history/evidence-id (:evidence/id entry)}}
         (promise-log/entry->hyperedge entry))))

(deftest write-requires-exact-readback-and-replays
  (let [edges (atom {})
        posts (atom 0)]
    (binding [promise-log/*post-fn* (fn [edge]
                                      (swap! posts inc)
                                      (swap! edges assoc (:hx/id edge) edge)
                                      {:ok true :hx/id (:hx/id edge)})
              promise-log/*get-fn* #(get @edges %)]
      (is (:ok (promise-log/write! entry)))
      (is (:ok (promise-log/write! entry)))
      (is (= 2 @posts))
      (is (= 1 (count @edges))))))

(deftest mismatched-readback-is-refused
  (binding [promise-log/*post-fn* (constantly {:ok true})
            promise-log/*get-fn* (fn [id]
                                   (assoc (promise-log/entry->hyperedge entry)
                                          :hx/id id :hx/endpoints ["wrong"]))]
    (is (= :promise-log-readback-mismatch
           (try (promise-log/write! entry) nil
                (catch clojure.lang.ExceptionInfo error
                  (:reason (ex-data error))))))))

(deftest bounded-reader-rebuilds-current-and-as-of-rows
  (let [edge (promise-log/entry->hyperedge entry)
        calls (atom [])]
    (binding [promise-log/*read-hyperedges-fn*
              (fn [type options]
                (swap! calls conj [type options])
                [edge])]
      (is (= [entry] (promise-log/read-history)))
      (is (= [entry] (promise-log/read-history
                      {:mode :as-of :at "2026-11-05T12:01:00Z"})))
      (is (= [:promise/history-event :promise/history-event]
             (mapv first @calls)))
      (is (every? #(= {:limit 20000 :page-size 1000 :request-budget 20}
                      (select-keys (second %) [:limit :page-size :request-budget]))
                  @calls))
      (is (= {:valid-as-of "2026-11-05T12:01:00Z"
              :system-as-of "2026-11-05T12:01:00Z"}
             (select-keys (second (second @calls))
                          [:valid-as-of :system-as-of]))))))

(deftest bounded-reader-refuses-a-partial-population
  (binding [promise-log/*read-hyperedges-fn*
            (fn [_ _] (with-meta [] {:partial? true :next-cursor "more"}))]
    (is (= :truncated-input
           (try (promise-log/read-history) nil
                (catch clojure.lang.ExceptionInfo error
                  (:reason (ex-data error))))))))

(deftest first-format-evidence-ids-map-to-log-ids
  ;; 2,207 promise-history rows from 2026-09-27/28 use e-<uuid> ids; the
  ;; reconciler threw on the first one in an open chain.
  (is (= "promise-log:e-3cfaa48d-a4de-40a7-afc0-ef127c9fbfd5"
         (promise-log/hyperedge-id "e-3cfaa48d-a4de-40a7-afc0-ef127c9fbfd5")))
  (is (= "promise-log:abc" (promise-log/hyperedge-id "promise-history:abc")))
  (is (thrown? clojure.lang.ExceptionInfo (promise-log/hyperedge-id "e-not-a-uuid")))
  (is (thrown? clojure.lang.ExceptionInfo (promise-log/hyperedge-id nil))))
