(ns futon3c.arxana.graph-read
  "C3 bounded breadth-first traversal with visible truncation."
  (:require [futon3c.arxana.store :as store]))

(defn traverse
  [graph-store {:keys [start/id edge/type endpoint/role max-depth max-nodes]
                :or {max-depth 8 max-nodes 100}}]
  (let [max-depth (max 0 (long max-depth))
        max-nodes (max 0 (long max-nodes))]
    (if (zero? max-nodes)
      {:status :truncated :truncation/reason :max-nodes :nodes []}
      (loop [depth 0 visited [id] frontier [id]]
        (cond
          (empty? frontier) {:status :complete :nodes visited}
          (>= depth max-depth) {:status :truncated :truncation/reason :max-depth
                                :nodes visited}
          :else
          (let [answers (mapv #(store/bounded-query!
                                graph-store {:query/type :role-reverse
                                             :edge-type type :endpoint-role role
                                             :endpoint-id % :limit 200}) frontier)
                stale (some #(when (= :stale-index (get-in % [:ok :status])) %) answers)
                error (some :error answers)]
            (cond
              error {:error error}
              stale {:error {:error/category :stale
                             :error/code :arxana.graph/index-behind
                             :error/detail (:ok stale)}}
              :else
              (let [seen (set visited)
                    candidates (->> answers (mapcat #(get-in % [:ok :edge-ids]))
                                    distinct (remove seen) vec)
                    room (- max-nodes (count visited))
                    added (vec (take room candidates))
                    next-visited (into visited added)]
                (if (> (count candidates) room)
                  {:status :truncated :truncation/reason :max-nodes
                   :nodes next-visited}
                  (recur (inc depth) next-visited added))))))))))
