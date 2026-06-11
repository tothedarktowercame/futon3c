(ns futon3c.wm.promote
  "WM operator-promote sentinel — stop-the-line PROMOTION, the dual of the
   operator-clear sentinel (transport/http.clj ~4659).

   Joe, 2026-06-11: 'we should always be able to' put a mission at the top
   of the WM's queue with stop-the-line semantics. Sentinel file (EDN):

     ~/code/storage/futon0/wm-operator-promote.edn
     {:targets [\"M-first-flights\"]            ;; action :target strings
      :until   \"2026-06-13T00:00:00Z\"        ;; expiry — promotions decay
      :reason  \"...\"                          ;; operator's words
      :by      \"joe\"}

   Applied at scan-assembly time (wm.scheduler/refresh-one-window!), next
   to apply-wm-operator-clear. VISIBLE, never silent: promoted entries
   carry :operator-promoted true and keep their honest G (the promotion
   reorders the queue, it does not fake the score); the judgement carries
   :operator-promote {sentinel + :hits} so every reader can see the
   operator's hand. Lives in its own ns (not http.clj) to stay
   hot-reloadable without touching the route layer; fold into http.clj's
   sentinel section at the next restart boundary if preferred."
  (:require [clojure.edn :as edn]))

(def ^:private sentinel-path
  (str (System/getProperty "user.home")
       "/code/storage/futon0/wm-operator-promote.edn"))

(defn read-sentinel
  "Sentinel iff present and unexpired; nil otherwise."
  []
  (try
    (let [f (java.io.File. ^String sentinel-path)]
      (when (.exists f)
        (let [data (edn/read-string (slurp f))
              until (:until data)
              until-inst (cond
                           (instance? java.util.Date until) (.toInstant ^java.util.Date until)
                           (string? until) (java.time.Instant/parse until)
                           :else nil)]
          (when (and until-inst (.isAfter until-inst (java.time.Instant/now)))
            data))))
    (catch Exception _ nil)))

(defn apply-operator-promote
  "Reorder BUNDLE's ranked-actions so sentinel targets lead, visibly.
   Pure given the sentinel (pass :sentinel in OPTS for tests)."
  ([bundle] (apply-operator-promote bundle {}))
  ([{:keys [judgement] :as bundle} {:keys [sentinel]}]
   (let [promo (or sentinel (read-sentinel))]
     (if-not promo
       bundle
       (let [targets (set (map str (:targets promo)))
             ras (:ranked-actions judgement)
             hit? (fn [e] (contains? targets (str (get-in e [:action :target]))))
             promoted (->> ras (filter hit?) (mapv #(assoc % :operator-promoted true)))
             remainder (vec (remove hit? ras))
             reranked (vec (map-indexed (fn [i e] (assoc e :rank (inc i)))
                                        (concat promoted remainder)))]
         (-> bundle
             (assoc-in [:judgement :ranked-actions] reranked)
             (assoc-in [:judgement :operator-promote]
                       (assoc (select-keys promo [:targets :until :reason :by])
                              :hits (count promoted)))))))))
