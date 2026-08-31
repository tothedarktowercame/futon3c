#!/usr/bin/env clojure -M
;; Latency monitor for the shared memory projection seam
;; (E-memory-latency "Monitoring still required"; M-shared-memory Phase 1/3
;; read seam).  Runs a fixed, bounded probe series against a live futon1b and
;; writes an EDN record.  Read-only: GET/POST bounded projections and one
;; bounded text-search per repetition, explicit limits everywhere.
;;
;; Usage:
;;   clojure -M scripts/memory_latency_monitor.clj [base-url] [reps] [out-path]
;;   clojure -M scripts/memory_latency_monitor.clj --self-check
;;
;; Percentiles are reported only for kinds with at least
;; `percentile-min-sample` warm observations; the threshold is stated in the
;; output record.

(require '[clojure.string :as str])

(import '(java.net URI)
        '(java.net.http HttpClient HttpRequest
                        HttpRequest$BodyPublishers
                        HttpResponse$BodyHandlers)
        '(java.time Instant))

(def default-base-url "http://127.0.0.1:7073")
(def percentile-min-sample 20)

(def known-endpoint "math-formalization/tactic-algebra-interference")
(def probe-endpoints [known-endpoint
                      "probe/miss-1" "probe/miss-2"
                      "probe/miss-3" "probe/miss-4"])
(def fixed-fts-query "normalize a denominator before using field_simp")
(def fts-limit 5)
(def projection-limit 9)

(defn- fresh-client
  ;; one client per request: futon1b closes keep-alive connections and the
  ;; JDK client surfaces that as EOF on reuse; a fresh client pays a small
  ;; connect cost but never reuses a half-closed connection.
  []
  (HttpClient/newHttpClient))

(defn- wall-ms
  [f]
  (let [t0 (System/nanoTime)
        result (f)
        t1 (System/nanoTime)]
    [result (double (/ (- t1 t0) 1e6))]))

(defn- http
  [method url body]
  (let [builder (-> (HttpRequest/newBuilder (URI. url))
                    (.timeout (java.time.Duration/ofSeconds 30))
                    (.header "content-type" "application/json"))]
    (when body (.POST builder (HttpRequest$BodyPublishers/ofString body)))
    (when (and (nil? body) (= method :get)) (.GET builder))
    (let [req (.build builder)
          [resp elapsed] (wall-ms #(.send (fresh-client) req
                                          (HttpResponse$BodyHandlers/ofString)))]
      {:status (.statusCode resp)
       :body (.body resp)
       :wall-ms elapsed})))

(defn- parse-json-ish-count
  "Count components/groups occurrences for a compact size signal."
  [body]
  {:groups (count (re-seq #"\"endpoint\"" body))
   :components (count (re-seq #"\"hyperedge-id\"" body))})

(defn probe-projection
  [base-url]
  (let [{:keys [status body wall-ms]}
        (http :post (str base-url "/api/alpha/memory/projection")
              (format "{\"endpoints\": [%s], \"limit\": %d}"
                      (str/join "," (map pr-str probe-endpoints)) projection-limit))]
    {:kind :projection
     :status status
     :wall-ms wall-ms
     :endpoints-requested (count probe-endpoints)
     :limit projection-limit
     :projection-revision (when-let [m (re-find #"\"projection-revision\":(\d+)" body)]
                            (Long/parseLong (second m)))
     :groups (get (parse-json-ish-count body) :groups)
     :components (get (parse-json-ish-count body) :components)}))

(defn probe-fts
  [base-url]
  (let [url (str base-url "/api/alpha/evidence/text-search?q="
                 (java.net.URLEncoder/encode fixed-fts-query "UTF-8")
                 "&limit=" fts-limit)
        {:keys [status body wall-ms]} (http :get url nil)]
    {:kind :fts
     :status status
     :wall-ms wall-ms
     :limit fts-limit
     :result-ids (count (re-seq #"e-[0-9a-f]{8}-" body))}))

(defn percentile
  "Linear-interpolated percentile of a sorted numeric seq (0-100)."
  [p values]
  (let [s (sort values)
        n (count s)]
    (when (pos? n)
      (let [idx (/ (* p (dec n)) 100.0)
            lo (int (Math/floor idx))
            hi (int (Math/ceil idx))
            lo-v (double (nth s lo))
            hi-v (double (nth s (min hi (dec n))))]
        (if (= lo hi)
          lo-v
          (+ lo-v (* (- idx lo) (- hi-v lo-v))))))))

(defn summarize-kind
  [observations]
  (let [warm (->> observations (filter #(= :warm (:class %))) (map :wall-ms))
        cold (->> observations (filter #(= :cold (:class %))) (map :wall-ms))
        n (count warm)]
    (cond-> {:cold-count (count cold)
             :warm-count n
             :warm-min (when (pos? n) (apply min warm))
             :warm-max (when (pos? n) (apply max warm))
             :percentiles-reported? (>= n percentile-min-sample)}
      (>= n percentile-min-sample)
      (assoc :p50 (double (percentile 50 warm))
             :p95 (double (percentile 95 warm))))))

(defn run-series
  [base-url reps]
  (let [t (Instant/now)
        obs (vec
             (mapcat
              (fn [i]
                (let [class (if (zero? i) :cold :warm)]
                  [(assoc (probe-projection base-url) :class class :rep i)
                   (assoc (probe-fts base-url) :class class :rep i)]))
              (range reps)))]
    {:monitor-version 1
     :recorded-at (str t)
     :base-url base-url
     :reps reps
     :percentile-min-sample percentile-min-sample
     :probes {:projection {:endpoints probe-endpoints :limit projection-limit}
              :fts {:query fixed-fts-query :limit fts-limit}}
     :failures (vec (filter #(not= 200 (:status %)) obs))
     :observations obs
     :summary {:projection (summarize-kind (filter #(= :projection (:kind %)) obs))
               :fts (summarize-kind (filter #(= :fts (:kind %)) obs))}}))

(defn self-check
  []
  (assert (= 25.0 (percentile 50 [10 20 30 40])))
  (assert (= 20.0 (percentile 50 [10 20 30])))
  (assert (= 10.0 (percentile 0 [10 20 30])))
  (assert (= 30.0 (percentile 100 [10 20 30])))
  (assert (false? (:percentiles-reported?
                   (summarize-kind [{:class :warm :wall-ms 1.0}]))))
  (assert (:percentiles-reported?
           (summarize-kind (repeat 20 {:class :warm :wall-ms 1.0}))))
  (println "self-check OK"))

(defn -main
  [& args]
  (if (= (first args) "--self-check")
    (self-check)
    (let [base-url (or (first args) default-base-url)
          reps (if (second args) (Long/parseLong (second args)) 25)
          out-path (or (nth args 2)
                       (format "holes/labs/M-typed-memories/latency-monitor-%s.edn"
                               (str/replace (subs (str (Instant/now)) 0 10) #"-" "")))
          record (run-series base-url reps)
          summary (:summary record)]
      (spit out-path (pr-str record))
      (println "wrote" out-path)
      (println "projection summary:" (pr-str (:projection summary)))
      (println "fts summary:" (pr-str (:fts summary)))
      (println "failures:" (count (:failures record))))))

(when *command-line-args*
  (apply -main *command-line-args*))
