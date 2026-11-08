(ns futon3c.arxana.graph-read
  "C3 bounded breadth-first traversal with visible truncation."
  (:require [futon3c.arxana.store :as store]))

(defn- query-neighbours [graph-store mode edge-type endpoint-role node]
  (if (and (= mode :endpoint-expansion) (.startsWith ^String node "clusion:"))
    (let [id (subs node (count "clusion:"))
          r (store/bounded-query! graph-store {:query/type :clusion-endpoints
                                               :clusion-id id})]
      (if (:error r) r
          {:ok {:status (get-in r [:ok :status])
                :ids (mapv #(str "endpoint:" %) (get-in r [:ok :endpoint-ids]))}}))
    (let [endpoint (if (= mode :endpoint-expansion)
                     (subs node (count "endpoint:")) node)
          r (store/bounded-query! graph-store {:query/type :role-reverse
                                               :edge-type edge-type
                                               :endpoint-role endpoint-role
                                               :endpoint-id endpoint :limit 200})]
      (if (:error r) r
          {:ok (assoc (:ok r) :ids (mapv #(if (= mode :endpoint-expansion)
                                           (str "clusion:" %) %)
                                        (get-in r [:ok :edge-ids]))) }))))

(defn traverse
  [graph-store {:keys [start/id edge/type endpoint/role traversal/mode max-depth max-nodes]
                :or {max-depth 8 max-nodes 100 mode :clusion}}]
  (let [max-depth (max 0 (long max-depth))
        max-nodes (max 0 (long max-nodes))]
    (if (zero? max-nodes)
      {:status :truncated :truncation/reason :max-nodes :nodes []}
      (let [start (if (= mode :endpoint-expansion) (str "endpoint:" id) id)]
       (loop [depth 0 visited [start] frontier [start]]
        (cond
          (empty? frontier) {:status :complete :nodes visited}
          (>= depth max-depth) {:status :truncated :truncation/reason :max-depth
                                :nodes visited}
          :else
          (let [answers (mapv #(query-neighbours graph-store mode type role %) frontier)
                stale (some #(when (= :stale-index (get-in % [:ok :status])) %) answers)
                fan-out (some #(when (get-in % [:ok :truncated?]) %) answers)
                error (some :error answers)]
            (cond
              error {:error error}
              stale {:error {:error/category :stale
                             :error/code :arxana.graph/index-behind
                             :error/detail (:ok stale)}}
              fan-out {:status :truncated :truncation/reason :fan-out
                       :nodes visited}
              :else
              (let [seen (set visited)
                    candidates (->> answers (mapcat #(get-in % [:ok :ids]))
                                    distinct (remove seen) vec)
                    room (- max-nodes (count visited))
                    added (vec (take room candidates))
                    next-visited (into visited added)]
                (if (> (count candidates) room)
                  {:status :truncated :truncation/reason :max-nodes
                   :nodes next-visited}
                  (recur (inc depth) next-visited added)))))))))))
