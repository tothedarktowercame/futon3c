#!/usr/bin/env bb
;; WS2 LIVE MEMORY/PATTERN CONNECTIVITY METER — PREREGISTERED CRITERION
;; (written before the first store read performed by this script).
;;
;; Verdict :dynamics-informative iff the largest CURRENT REVIEWED-edge
;; component has:
;;   1. at least 10 nodes,
;;   2. at least 2 distinct relation/attachment types, and
;;   3. lambda_2 > 0.1.
;; Otherwise the verdict is :component-limited.
;;
;; Grounding: v0's largest 3-node components were dynamically degenerate;
;; v1's connected 23-node, 4-relation graph differentiated operators.
;;
;; STORE SAFETY: this instrument performs one bounded GET and no store write.
;; On 503/:expensive-read-busy it waits 5 seconds and retries exactly once.
;; Local date-stamped outputs are write-once; reruns meter the frozen export.

(ns connectivity-meter
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def root "/home/joe/code/futon3c/holes/labs/M-typed-memories")
(def base-url "http://127.0.0.1:7073")
(def export-path (str root "/live-graph-export-20260727.edn"))
(def meter-path (str root "/connectivity-meter-20260727.edn"))
(def query-limit 5000)
(def retry-backoff-ms 5000)
(def jacobi-tolerance 1.0e-10)
(def jacobi-max-sweeps 200)
(def largest-component-count 5)

(def verdict-criterion
  {:minimum-largest-reviewed-component-nodes 10
   :minimum-distinct-edge-types 2
   :minimum-lambda-2-exclusive 0.1})

(defn parse-body [body]
  (cond
    (map? body) body
    (string? body)
    (try
      (edn/read-string {:default (fn [_tag value] value)} body)
      (catch Exception exception
        {:meter/parse-error (.getMessage exception)}))
    :else nil))

(defn retryable? [status body]
  (or (= 503 status)
      (= :expensive-read-busy (:error/code body))
      (= :expensive-read-busy (:error body))))

(defn bounded-query []
  (let [type-name "memory/assert"
        url (str base-url
                 "/api/alpha/hyperedges?type="
                 (URLEncoder/encode type-name "UTF-8")
                 "&limit=" query-limit
                 "&include-total=false")]
    (loop [attempt-number 1
           attempts []]
      (let [started (System/nanoTime)
            response
            (try
              (http/get url
                        {:headers {"accept" "application/edn"}
                         :timeout 60000
                         :throw false})
              (catch Exception exception
                {:status nil
                 :transport-error (.getMessage exception)}))
            elapsed-ms (/ (- (System/nanoTime) started) 1.0e6)
            body (when (:body response) (parse-body (:body response)))
            status (:status response)
            attempt
            {:attempt attempt-number
             :status status
             :elapsed-ms elapsed-ms
             :error (or (:transport-error response)
                        (:meter/parse-error body)
                        (:error/code body)
                        (:error body))}
            attempts-next (conj attempts attempt)]
        (cond
          (= 200 status)
          (let [rows (vec (or (:hyperedges body) []))
                server-count (:count body)
                count-exact? (true? (:count-exact? body))
                truncated?
                (or (= query-limit (count rows))
                    (and count-exact?
                         (number? server-count)
                         (> server-count (count rows))))]
            {:type :memory/assert
             :url url
             :limit query-limit
             :attempts attempts-next
             :row-count (count rows)
             :server-count server-count
             :server-count-exact? count-exact?
             :truncated? truncated?
             :hyperedges rows})

          (and (= attempt-number 1) (retryable? status body))
          (do
            (Thread/sleep retry-backoff-ms)
            (recur 2 attempts-next))

          :else
          {:type :memory/assert
           :url url
           :limit query-limit
           :attempts attempts-next
           :row-count 0
           :server-count nil
           :server-count-exact? false
           :truncated? false
           :error {:status status
                   :code (or (:error/code body) (:error body))
                   :parse-error (:meter/parse-error body)
                   :transport-error (:transport-error response)}
           :hyperedges []})))))

(defn write-once! [path value]
  (if (.exists (java.io.File. path))
    (let [existing (edn/read-string (slurp path))]
      (when-not (= existing value)
        (throw (ex-info "refusing to overwrite a prior meter artifact"
                        {:path path})))
      :existing)
    (do (spit path (pr-str value)) :written)))

(defn load-or-export! []
  (if (.exists (java.io.File. export-path))
    (edn/read-string (slurp export-path))
    (let [query (bounded-query)
          export
          {:export/version 1
           :captured-at (str (Instant/now))
           :source {:kind :futon1b
                    :base-url base-url}
           :store-read-only? true
           :queries [(dissoc query :hyperedges)]
           :row-count (:row-count query)
           :truncated? (:truncated? query)
           :errors (vec (keep :error [(dissoc query :hyperedges)]))
           :hyperedges (:hyperedges query)}]
      (write-once! export-path export)
      export)))

(defn props [hyperedge]
  (or (:hx/props hyperedge) {}))

(defn memory-id [hyperedge]
  (get-in hyperedge [:hx/props :roles :entry]))

(defn current-reviewed? [hyperedge]
  (and (= :current (get-in hyperedge [:hx/props :state]))
       (= :reviewed (get-in hyperedge
                            [:hx/props :attachment-status]))))

(defn graph-edges [hyperedges]
  (->>
   hyperedges
   (mapcat
    (fn [hyperedge]
      (let [memory (memory-id hyperedge)
            roles (:roles (props hyperedge))
            reviewed? (current-reviewed? hyperedge)
            common
            {:hyperedge-id (:hx/id hyperedge)
             :reviewed-current? reviewed?}]
        (when (string? memory)
          (concat
           (for [pattern (:patterns roles)
                 :when (string? pattern)]
             (merge common
                    {:source memory
                     :target pattern
                     :edge-type :pattern-attachment}))
           (for [distilled (:distills roles)
                 :when (and (string? distilled)
                            (.startsWith ^String distilled "e-"))]
             (merge common
                    {:source memory
                     :target distilled
                     :edge-type :distills})))))))
   (remove nil?)
   (reduce
    (fn [by-key edge]
      (let [key [(:source edge) (:target edge) (:edge-type edge)]]
        (update by-key key
                (fn [prior]
                  (if prior
                    (assoc prior :reviewed-current?
                           (or (:reviewed-current? prior)
                               (:reviewed-current? edge)))
                    edge)))))
    {})
   vals
   (sort-by (juxt :edge-type :source :target))
   vec))

(defn graph-nodes [hyperedges edges]
  (->> (concat (keep memory-id hyperedges)
               (map :source edges)
               (map :target edges))
       distinct sort vec))

(defn components [nodes edges]
  (let [neighbors
        (reduce
         (fn [m {:keys [source target]}]
           (-> m
               (update source (fnil conj #{}) target)
               (update target (fnil conj #{}) source)))
         (zipmap nodes (repeat #{}))
         edges)]
    (loop [remaining (set nodes)
           found []]
      (if-let [start (first (sort remaining))]
        (let [component
              (loop [frontier [start]
                     seen #{}]
                (if-let [node (peek frontier)]
                  (if (seen node)
                    (recur (pop frontier) seen)
                    (recur (into (pop frontier) (get neighbors node))
                           (conj seen node)))
                  seen))]
          (recur (apply disj remaining component)
                 (conj found (vec (sort component)))))
        (->> found
             (sort-by (juxt (comp - count) first))
             vec)))))

(defn matrix [n]
  (make-array Double/TYPE n n))

(defn copy-matrix [input]
  (let [n (alength ^objects input)
        output (matrix n)]
    (dotimes [i n]
      (dotimes [j n]
        (aset ^doubles (aget ^objects output i) j
              (aget ^doubles (aget ^objects input i) j))))
    output))

(defn off-diagonal-norm [a]
  (let [n (alength ^objects a)]
    (Math/sqrt
     (* 2.0
        (reduce
         +
         0.0
         (for [i (range n)
               j (range (inc i) n)]
           (let [value (aget ^doubles (aget ^objects a i) j)]
             (* value value))))))))

(defn jacobi-eigenvalues [input]
  (let [a (copy-matrix input)
        n (alength ^objects a)]
    (loop [sweep 0]
      (let [norm-before (off-diagonal-norm a)]
        (if (or (< norm-before jacobi-tolerance)
                (= sweep jacobi-max-sweeps))
          {:eigenvalues
           (->> (range n)
                (mapv #(aget ^doubles (aget ^objects a %) %))
                sort vec)
           :converged? (< norm-before jacobi-tolerance)
           :sweeps sweep
           :off-diagonal-norm norm-before}
          (do
            (doseq [p (range n)
                    q (range (inc p) n)]
              (let [apq (aget ^doubles (aget ^objects a p) q)]
                (when (> (Math/abs apq) 1.0e-15)
                  (let [app (aget ^doubles (aget ^objects a p) p)
                        aqq (aget ^doubles (aget ^objects a q) q)
                        tau (/ (- aqq app) (* 2.0 apq))
                        sign (if (neg? tau) -1.0 1.0)
                        t (/ sign
                             (+ (Math/abs tau)
                                (Math/sqrt (+ 1.0 (* tau tau)))))
                        c (/ 1.0 (Math/sqrt (+ 1.0 (* t t))))
                        s (* t c)]
                    (doseq [i (range n)
                            :when (and (not= i p) (not= i q))]
                      (let [aip (aget ^doubles (aget ^objects a i) p)
                            aiq (aget ^doubles (aget ^objects a i) q)
                            nip (- (* c aip) (* s aiq))
                            niq (+ (* s aip) (* c aiq))]
                        (aset ^doubles (aget ^objects a i) p nip)
                        (aset ^doubles (aget ^objects a p) i nip)
                        (aset ^doubles (aget ^objects a i) q niq)
                        (aset ^doubles (aget ^objects a q) i niq)))
                    (aset ^doubles (aget ^objects a p) p (- app (* t apq)))
                    (aset ^doubles (aget ^objects a q) q (+ aqq (* t apq)))
                    (aset ^doubles (aget ^objects a p) q 0.0)
                    (aset ^doubles (aget ^objects a q) p 0.0)))))
            (recur (inc sweep))))))))

(defn component-laplacian [component edges]
  (let [index (zipmap component (range))
        result (matrix (count component))
        allowed (set component)]
    (doseq [{:keys [source target]} edges
            :when (and (allowed source) (allowed target))]
      (let [i (index source)
            j (index target)]
        (aset ^doubles (aget ^objects result i) i
              (inc (aget ^doubles (aget ^objects result i) i)))
        (aset ^doubles (aget ^objects result j) j
              (inc (aget ^doubles (aget ^objects result j) j)))
        (aset ^doubles (aget ^objects result i) j
              (dec (aget ^doubles (aget ^objects result i) j)))
        (aset ^doubles (aget ^objects result j) i
              (dec (aget ^doubles (aget ^objects result j) i)))))
    result))

(defn component-reading [component edges]
  (let [allowed (set component)
        component-edges
        (filterv #(and (allowed (:source %))
                       (allowed (:target %)))
                 edges)
        solved (jacobi-eigenvalues
                (component-laplacian component component-edges))
        values (:eigenvalues solved)]
    {:nodes component
     :size (count component)
     :edge-count (count component-edges)
     :edge-types (->> component-edges (map :edge-type) distinct sort vec)
     :lambda-2 (when (> (count values) 1) (second values))
     :spectrum values
     :jacobi (dissoc solved :eigenvalues)}))

(defn size-histogram [component-list]
  (into (sorted-map) (frequencies (map count component-list))))

(defn meter [export]
  (let [hyperedges (:hyperedges export)
        edges (graph-edges hyperedges)
        nodes (graph-nodes hyperedges edges)
        all-components (components nodes edges)
        reviewed-edges (filterv :reviewed-current? edges)
        reviewed-components (components nodes reviewed-edges)
        largest-readings
        (mapv #(component-reading % reviewed-edges)
              (take largest-component-count reviewed-components))
        largest (first largest-readings)
        informative?
        (and largest
             (>= (:size largest)
                 (:minimum-largest-reviewed-component-nodes
                  verdict-criterion))
             (>= (count (:edge-types largest))
                 (:minimum-distinct-edge-types verdict-criterion))
             (number? (:lambda-2 largest))
             (> (:lambda-2 largest)
                (:minimum-lambda-2-exclusive verdict-criterion)))]
    {:meter/version 1
     :measured-from {:export-file (.getName (java.io.File. export-path))
                     :captured-at (:captured-at export)}
     :source-complete? (and (empty? (:errors export))
                            (not (:truncated? export)))
     :node-count (count nodes)
     :edge-count (count edges)
     :edge-count-by-type (frequencies (map :edge-type edges))
     :review-status-counts
     {:reviewed-current (count reviewed-edges)
      :unreviewed-or-noncurrent (- (count edges) (count reviewed-edges))}
     :components
     {:all-edge-count (count all-components)
      :all-edge-size-histogram (size-histogram all-components)
      :reviewed-edge-count (count reviewed-components)
      :reviewed-edge-size-histogram (size-histogram reviewed-components)
      :largest-reviewed largest-readings}
     :verdict-criterion verdict-criterion
     :verdict (if informative?
                :dynamics-informative
                :component-limited)}))

(def export (load-or-export!))
(def reading (meter export))
(def meter-write (write-once! meter-path reading))

(println "connectivity meter complete")
(println "export:" export-path "(" (:row-count export) "memory/assert rows)")
(println "nodes/edges:" (:node-count reading) "/" (:edge-count reading))
(println "largest reviewed component:"
         (select-keys (get-in reading
                              [:components :largest-reviewed 0])
                      [:size :edge-count :edge-types :lambda-2]))
(println "verdict:" (:verdict reading))
(println "meter artifact:" meter-write)
