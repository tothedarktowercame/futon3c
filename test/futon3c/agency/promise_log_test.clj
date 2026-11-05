(ns futon3c.agency.promise-log-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.promise-log :as promise-log]))

(def entry
  {:evidence/id "promise-history:123e4567-e89b-12d3-a456-426614174000"
   :evidence/type :promise/dependency-terminated
   :evidence/author "agent-7"
   :evidence/at "2026-11-05T12:00:00Z"
   :evidence/body {:history/payload-edn "{:record {:id \"park-7\"}}"
                   :history/promise-id "park-7"
                   :history/promise-sequence 2
                   :history/predecessor {:sequence 1
                                         :id "promise-history:previous"
                                         :type :promise/park-made}}})

(deftest prepared-entry-has-stable-lossless-hyperedge
  (is (= {:hx/id "promise-log:123e4567-e89b-12d3-a456-426614174000"
          :hx/type :promise/history-event
          :hx/endpoints ["park-7" "agent-7"]
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
