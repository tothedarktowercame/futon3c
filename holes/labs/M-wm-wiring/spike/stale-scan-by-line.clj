(let [nss (filter #(re-find #"^futon2\.(aif|report)\." (str (ns-name %))) (all-ns))
      check (fn [ns]
              (let [vars (filter (fn [v] (and (:line (meta v)) (:file (meta v)))) (vals (ns-interns ns)))
                    file (:file (meta (first vars)))
                    res (when file (clojure.java.io/resource file))
                    lines (when res (vec (clojure.string/split-lines (slurp res))))]
                (cond
                  (empty? vars) {:status :no-vars}
                  (nil? res) {:status :no-resource :file file}
                  :else
                  (let [bad (for [v vars
                                  :let [l (:line (meta v)) nm (str (:name (meta v)))
                                        txt (get lines (dec l) "")]
                                  :when (not (clojure.string/includes? txt nm))]
                              [nm l])]
                    (if (seq bad) {:status :stale :file file :mismatched (count bad) :of (count vars) :sample (vec (take 3 bad))}
                        {:status :current :of (count vars)})))))
      out (into (sorted-map) (map (fn [ns] [(ns-name ns) (check ns)]) nss))]
  {:total (count out)
   :stale (into (sorted-map) (filter (fn [[_ v]] (= :stale (:status v))) out))
   :other (into (sorted-map) (filter (fn [[_ v]] (not (#{:stale :current} (:status v)))) out))})
