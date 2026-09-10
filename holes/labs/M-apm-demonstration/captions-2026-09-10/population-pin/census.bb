#!/usr/bin/env bb
;; GET-only census of the memories actually exposed by the archived APM shelves.
(require '[cheshire.core :as json] '[clojure.edn :as edn]
         '[clojure.java.io :as io] '[clojure.string :as str])
(import '[java.net URI URLEncoder] '[java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
        '[java.security MessageDigest] '[java.math BigInteger])

(def root "data/apm-campaigns/jit-all-open-v3")
(def audit "holes/labs/M-apm-demonstration/analysis/memory-audit-2026-09-10/readback.json")
(def base (or (System/getenv "FUTON1B_URL") "http://127.0.0.1:7073"))
(def client (.build (HttpClient/newBuilder)))
(defn sha [s] (format "%064x" (BigInteger. 1 (.digest (MessageDigest/getInstance "SHA-256") (.getBytes (str s) "UTF-8")))))
(defn read-edn [f] (edn/read-string (slurp f)))
(defn files [re] (->> (file-seq (io/file root)) (filter #(.isFile %)) (filter #(re-find re (str %))) (sort-by str)))
(defn get-edn [path]
  (loop [attempt 1]
    (let [result
          (try
            (let [req (-> (HttpRequest/newBuilder (URI/create (str base path)))
                          (.header "Accept" "application/edn") (.GET) (.build))
                  res (.send client req (HttpResponse$BodyHandlers/ofString))]
              {:status (.statusCode res) :raw (.body res)
               :body (when (= 200 (.statusCode res)) (edn/read-string (.body res)))})
            (catch Exception e {:status :unreachable :error (.getMessage e)}))]
      (if (and (= :unreachable (:status result)) (< attempt 5))
        (do (Thread/sleep (* 100 attempt)) (recur (inc attempt)))
        result))))
(defn q [s] (URLEncoder/encode (str s) "UTF-8"))
(def attempts (->> (files #"/live/student-attempt-[0-9]+\.edn$")
                   (mapv (fn [f] [(.getPath f) (read-edn f)]))
                   (filterv (fn [[_ a]] (<= 190 (Integer/parseInt (subs (get-in a [:request :frame-id]) 1)) 213)))))
(def exposures
  (reduce (fn [m [path a]]
            (reduce (fn [m id] (update m id (fnil conj [])
                                      {:path path :frame (get-in a [:request :frame-id])
                                       :attempt (get-in a [:request :attempt-ordinal])
                                       :problem (get-in a [:request :problem-id])}))
                    m (get-in a [:request :memory-snapshot :accessible-memory-ids])))
          (sorted-map) attempts))
(def snapshot-memories
  (reduce (fn [m f]
            (reduce (fn [m x] (assoc m (:memory-id x) x)) m (:snapshot/memories (read-edn f))))
          {} (files #"/snapshots/.*-memory\.edn$")))
(def audited (->> (json/parse-string (slurp audit) true) :rows (map :memory-id) set))
(defn kval [x] (when x (if (keyword? x) (name x) (str x))))
(defn edge-state [e] (kval (or (get-in e [:hx/props :state]) (:prop/state e))))
(defn edge-status [e] (kval (or (get-in e [:hx/props :attachment-status]) (:prop/attachment-status e))))
(defn current-reviewed? [e] (and (= "current" (edge-state e)) (= "reviewed" (edge-status e))))
(defn one [[id xs]]
  (let [s (get snapshot-memories id)
        mem (get-edn (str "/api/alpha/evidence/" (q id)))
        rid (:review-evidence-id s)
        review (when rid (get-edn (str "/api/alpha/evidence/" (q rid))))
        hedges (get-edn (str "/api/alpha/hyperedges?end=" (q id) "&type=" (q "memory/assert") "&limit=1000"))
        es (vec (or (get-in hedges [:body :hyperedges]) []))
        eb (:evidence/body (:body mem))
        body (:body eb)
        admiss (cond
                 (not= 200 (:status mem)) "memory-unreadable"
                 (str/blank? body) "body-missing"
                 (not-any? current-reviewed? es) "no-current-reviewed-attachment"
                 (and rid (not= 200 (:status review))) "review-unreadable"
                 :else "admissible")]
    (sorted-map
     :memory-id id :depositor (or (:evidence/author (:body mem)) (:depositor s))
     :review-pointer rid :reviewer (or (:evidence/author (:body review)) (:reviewer s))
     :deposit (select-keys (:provenance s) [:campaign-id :frame-id :problem-id])
     :kind (case (:memory-use/kind s) :substitutive "substitutive" :regulative "regulative" "other")
     :admissibility-state admiss :body-sha256 (when (string? body) (sha body))
     :recorded-content-digest (:content-digest s)
     :audited-57-subset (contains? audited id)
     :exposure {:count (count xs) :first (first xs) :last (last xs)}
     :readback {:memory-status (:status mem) :review-status (when rid (:status review))
                :hyperedge-status (:status hedges) :attachment-edge-count (count es)
                :current-reviewed-edge-count (count (filter current-reviewed? es))})))
(def population (into (sorted-map) (map (fn [id] [id (get exposures id [])])
                                         (into (set (keys exposures)) audited))))
(def rows (->> population (partition-all 8)
               (mapcat (fn [batch] (mapv deref (mapv #(future (one %)) batch))))
               (sort-by :memory-id) vec))
(def snapshot-only (sort (remove (set (keys population)) (keys snapshot-memories))))
(def result
  (sorted-map
   :schema "apm-caption-population-pin/v1" :observation-date "2026-09-10"
   :scope "union of accessible-memory-ids in archived f190-f213 jit-all-open-v3 student attempts plus the pinned 57-memory audit cohort; current state verified by GET"
   :endpoint base :attempt-count (count attempts) :population-count (count rows)
   :audited-subset-count (count (filter :audited-57-subset rows))
   :shelf-size-range [(apply min (map #(count (get-in (second %) [:request :memory-snapshot :accessible-memory-ids])) attempts))
                      (apply max (map #(count (get-in (second %) [:request :memory-snapshot :accessible-memory-ids])) attempts))]
   :snapshot-only-excluded snapshot-only
   :counts {:kind (frequencies (map :kind rows)) :admissibility (frequencies (map :admissibility-state rows))}
   :rows rows))
(println (json/generate-string result {:pretty true}))
