(ns futon3c.enrichment.query
  "Query futon1a hyperedge store for enrichment data.

   Given a file path, returns composite enrichment: missions, patterns,
   evidence counts, tensions, and deps for every symbol in the file.

   Queries futon1a via HTTP (not classpath) to respect I-5 boundaries."
  (:require [clojure.string :as str]
            [cheshire.core :as json])
  (:import [java.net URLEncoder]
           [java.nio.charset StandardCharsets]))

(def ^:private default-futon1a-url "http://localhost:7071")

(defn- url-encode [s]
  (URLEncoder/encode (str s) (.name StandardCharsets/UTF_8)))

(defn- http-get-json
  "GET url, parse JSON response. Returns parsed body or nil on error."
  [url]
  (try
    (let [conn (doto (.openConnection (java.net.URL. url))
                 (.setRequestMethod "GET")
                 (.setConnectTimeout 5000)
                 (.setReadTimeout 10000)
                 (.setRequestProperty "Accept" "application/json"))
          status (.getResponseCode conn)]
      (when (<= 200 status 299)
        (with-open [is (.getInputStream conn)]
          (json/parse-stream (java.io.InputStreamReader. is) true))))
    (catch Exception _ nil)))

(defn- query-by-endpoint
  "Query futon1a for all hyperedges involving endpoint-id."
  [futon1a-url endpoint-id]
  (let [url (str futon1a-url "/api/alpha/hyperedges?end=" (url-encode endpoint-id))
        result (http-get-json url)]
    (or (:hyperedges result) [])))

(defn- query-by-type
  "Query futon1a for all hyperedges of a given type."
  [futon1a-url hx-type]
  (let [url (str futon1a-url "/api/alpha/hyperedges?type=" (url-encode hx-type))
        result (http-get-json url)]
    (or (:hyperedges result) [])))

;; --- Path → Namespace mapping ---

(defn path->namespace
  "Convert a Clojure source path to a namespace string.
   e.g. src/futon3c/peripheral/mission_control_backend.clj
     → futon3c.peripheral.mission-control-backend"
  [path]
  (when (and path (not (str/blank? path)))
    (let [;; Strip src/ prefix and .clj/.cljc suffix
          cleaned (-> path
                      (str/replace #"^.*/src/" "")
                      (str/replace #"^src/" "")
                      (str/replace #"\.cljc?$" ""))
          ;; Convert path separators to dots, underscores to hyphens
          ns-str (-> cleaned
                     (str/replace "/" ".")
                     (str/replace "_" "-"))]
      ns-str)))

;; --- Enrichment aggregation ---

(defn- classify-hyperedge
  "Classify a hyperedge into an enrichment category."
  [{:keys [hx/type]}]
  (let [t (str (when (namespace type) (str (namespace type) "/")) (name type))]
    (cond
      (str/starts-with? t "project/mission")  :missions
      (str/starts-with? t "project/pattern")  :patterns
      (str/starts-with? t "evidence/")        :evidence
      (str/starts-with? t "tension/")         :tensions
      (str/starts-with? t "dep/")             :deps
      (str/starts-with? t "code/file-churn")  :churn
      (str/starts-with? t "code/indentation") :complexity
      (str/starts-with? t "code/requires")    :requires
      (str/starts-with? t "code/ns-contains") :vars
      (str/starts-with? t "invariant/")       :invariants
      :else                                   :other)))

(defn- extract-var-name
  "Extract the var name from a var: endpoint id.
   e.g. var:futon3c.core/foo → foo"
  [endpoint-id]
  (when (str/starts-with? (str endpoint-id) "var:")
    (let [full (subs endpoint-id 4)]
      (when-let [idx (str/index-of full "/")]
        (subs full (inc idx))))))

(defn- extract-mission-id
  "Extract mission ID from a mission hyperedge."
  [hx]
  (or (get-in hx [:hx/props :mission-id])
      (get-in hx [:hx/props "mission-id"])
      ;; Try to find mission endpoint
      (some (fn [ep]
              (when (and (string? ep) (str/starts-with? ep "mission:"))
                (subs ep (count "mission:"))))
            (:hx/endpoints hx))))

(defn- extract-pattern-name
  "Extract pattern name from a pattern hyperedge."
  [hx]
  (or (get-in hx [:hx/props :pattern-name])
      (get-in hx [:hx/props "pattern-name"])
      (some (fn [ep]
              (when (and (string? ep) (str/starts-with? ep "pattern:"))
                (subs ep (count "pattern:"))))
            (:hx/endpoints hx))))

(defn- build-symbol-enrichment
  "Build per-symbol enrichment map from hyperedges grouped by var endpoint."
  [var-edges]
  (let [grouped (group-by classify-hyperedge var-edges)]
    (cond-> {}
      (:missions grouped)
      (assoc :missions (vec (distinct (keep extract-mission-id (:missions grouped)))))

      (:patterns grouped)
      (assoc :patterns (vec (distinct (keep extract-pattern-name (:patterns grouped)))))

      (:evidence grouped)
      (assoc :evidence-count (count (:evidence grouped)))

      (:tensions grouped)
      (assoc :tensions (mapv (fn [hx]
                               (or (get-in hx [:hx/props :summary])
                                   (get-in hx [:hx/props "summary"])
                                   (name (:hx/type hx))))
                             (:tensions grouped)))

      (:requires grouped)
      (assoc :deps {:requires (vec (distinct
                                    (mapcat (fn [hx]
                                              (remove #(or (not (string? %))
                                                          (str/starts-with? % "ns:"))
                                                      (:hx/endpoints hx)))
                                            (:requires grouped))))}))))

(defn enrich-file
  "Query futon1a for all enrichment data for a source file.

   Returns:
   {:file path
    :namespace ns-str
    :enrichment-layer N  (highest layer found)
    :missions [{:mission/id ... :mission/status ...}]
    :churn {:commits N :max-depth N}
    :invariants [...]
    :symbols {\"var-name\" {:missions [...] :patterns [...] ...}}}"
  ([path] (enrich-file path {}))
  ([path {:keys [futon1a-url] :or {futon1a-url default-futon1a-url}}]
   (let [ns-str (path->namespace path)
         ns-endpoint (str "ns:" ns-str)
         ;; Query all edges involving this namespace
         ns-edges (query-by-endpoint futon1a-url ns-endpoint)
         ;; Find all var endpoints for this namespace
         var-endpoints (->> ns-edges
                            (filter #(= (keyword "code" "ns-contains-var")
                                        (:hx/type %)))
                            (mapcat :hx/endpoints)
                            (filter #(str/starts-with? (str %) "var:"))
                            distinct
                            vec)
         ;; Query edges for each var
         var-edge-map (into {}
                            (for [vep var-endpoints
                                  :let [var-name (extract-var-name vep)
                                        edges (query-by-endpoint futon1a-url vep)]
                                  :when var-name]
                              [var-name edges]))
         ;; Namespace-level classifications
         ns-classified (group-by classify-hyperedge ns-edges)
         ;; Find highest enrichment layer
         layer-edges (query-by-type futon1a-url "meta/enrichment-layer")
         max-layer (if (seq layer-edges)
                     (apply max (map #(or (get-in % [:hx/props :layer])
                                         (get-in % [:hx/props "layer"])
                                         0)
                                     layer-edges))
                     0)
         ;; Extract churn data
         churn-edges (filter #(= (keyword "code" "file-churn") (:hx/type %))
                             ns-edges)
         churn (when (seq churn-edges)
                 (let [hx (first churn-edges)]
                   {:commits (or (get-in hx [:hx/props :commits])
                                 (get-in hx [:hx/props "commits"]))
                    :score (or (get-in hx [:hx/props :score])
                               (get-in hx [:hx/props "score"]))}))
         ;; Extract missions at namespace level
         ns-missions (->> (concat (:missions ns-classified) [])
                          (keep extract-mission-id)
                          distinct
                          vec)
         ;; Extract invariant violations
         invariants (->> (:invariants ns-classified)
                         (mapv (fn [hx]
                                 {:type (name (:hx/type hx))
                                  :summary (or (get-in hx [:hx/props :summary])
                                               (get-in hx [:hx/props "summary"])
                                               "")
                                  :invariant (or (get-in hx [:hx/props :invariant])
                                                 (get-in hx [:hx/props "invariant"])
                                                 "")})))
         ;; Build per-symbol enrichment
         symbols (into {}
                       (for [[var-name edges] var-edge-map
                             :let [enrichment (build-symbol-enrichment edges)]
                             :when (seq enrichment)]
                         [var-name enrichment]))]
     {:file path
      :namespace ns-str
      :enrichment-layer max-layer
      :missions ns-missions
      :churn churn
      :invariants invariants
      :var-count (count var-endpoints)
      :symbols symbols})))
