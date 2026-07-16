(ns futon3c.aif.mission-delta-t
  "Targeted mission ΔT reader over substrate-2.

   v0 scope:
   - target vertices are mission-doc endpoints only
   - incoming edge families considered: code/v05/related-mission and
     code/v05/mission-cross-ref
   - source vertices may be sorry vertices or mission-doc vertices

   This is an INSTANTIATE-car-#1 substrate reader for
   M-action-cost-modelling §5 T9-completion subpart (a)."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def ^:private FUTON1A
  (or (System/getenv "FUTON_SUBSTRATE_URL")
      (System/getenv "FUTON1A_URL") "http://localhost:7071"))

(def ^:private PENHOLDER
  (or (System/getenv "FUTON1A_PENHOLDER") "api"))

(def phase-t-table
  {"head" 1.0
   "identify" 0.9
   "map" 0.8
   "derive" 0.7
   "argue" 0.6
   "verify" 0.5
   "instantiate" 0.3
   "document" 0.1
   "complete" 0.0
   "unknown" 0.5
   nil 0.5})

(def ^:private incoming-edge-types
  #{"code/v05/related-mission"
    "code/v05/mission-cross-ref"
    "code/v05/file→mission"})

(def ^:private related-mission-kw
  (keyword "code" "v05/related-mission"))

(def ^:private mission-cross-ref-kw
  (keyword "code" "v05/mission-cross-ref"))

(def ^:private file-to-mission-kw
  (keyword "code" "v05/file→mission"))

(def ^:private file-default-t
  0.5)

(defn- url-encode
  [s]
  (java.net.URLEncoder/encode (str s) "UTF-8"))

(defn- parse-json-body
  [body]
  (cond
    (string? body) (json/parse-string body true)
    (map? body) body
    :else nil))

(defn- hx-type-str
  [hx]
  (let [t (:hx/type hx)]
    (cond
      (keyword? t) (if-let [ns-part (namespace t)]
                     (str ns-part "/" (name t))
                     (name t))
      (string? t) t
      :else (str t))))

(defn- real-endpoints
  [hx]
  (vec (remove #(and (string? %) (str/starts-with? % "dir:"))
               (:hx/endpoints hx))))

(defn- mission-endpoint?
  [endpoint]
  (and (string? endpoint)
       (str/includes? endpoint "/mission/")))

(defn- file-endpoint?
  [endpoint]
  (and (string? endpoint)
       (str/includes? endpoint "/file/")))

(defn- sorry-endpoint?
  [endpoint]
  (and (string? endpoint)
       (str/includes? endpoint "/sorry/")))

(defn- normalize-phase
  [phase]
  (cond
    (nil? phase) nil
    (keyword? phase) (some-> phase name str/lower-case)
    (string? phase) (-> phase str/trim str/lower-case)
    :else (-> phase str str/lower-case)))

(defn phase->t
  [phase]
  (get phase-t-table (normalize-phase phase) 0.5))

(defn fetch-hyperedges-by-endpoint
  "Targeted substrate-2 read by endpoint. Returns all matching hyperedges."
  ([endpoint] (fetch-hyperedges-by-endpoint endpoint {}))
  ([endpoint {:keys [limit futon1a-url]
              :or {limit 200
                   futon1a-url FUTON1A}}]
   (let [url (str futon1a-url
                  "/api/alpha/hyperedges?end="
                  (url-encode endpoint)
                  "&limit="
                  limit)
         resp (http/get url {:headers {"Accept" "application/json"
                                       "X-Penholder" PENHOLDER}
                             :throw false
                             :timeout 5000})]
     (if (= 200 (:status resp))
       (vec (or (:hyperedges (parse-json-body (:body resp))) []))
       (throw (ex-info "hyperedges-by-endpoint failed"
                       {:endpoint endpoint
                        :url url
                        :status (:status resp)
                        :body (:body resp)}))))))

(defn- find-vertex-doc
  [endpoint hyperedges]
  (some (fn [hx]
          (let [eps (real-endpoints hx)]
            (when (and (= 1 (count eps))
                       (= endpoint (first eps)))
              hx)))
        hyperedges))

(defn- edge-type->kw
  [edge-type]
  (case edge-type
    "code/v05/related-mission" related-mission-kw
    "code/v05/mission-cross-ref" mission-cross-ref-kw
    "code/v05/file→mission" file-to-mission-kw
    (keyword edge-type)))

(defn- mission-phase-from-doc
  [vertex-doc]
  (or (get-in vertex-doc [:hx/props "mission/phase"])
      (get-in vertex-doc [:hx/props :mission/phase])))

(defn- sorry-t-from-doc
  [vertex-doc]
  (let [raw (or (get-in vertex-doc [:hx/props "sorry/t"])
                (get-in vertex-doc [:hx/props :sorry/t]))]
    (cond
      (number? raw) (double raw)
      (string? raw) (Double/parseDouble raw)
      :else 0.0)))

(defn- mission-state
  [endpoint hyperedges]
  (let [vertex-doc (find-vertex-doc endpoint hyperedges)
        phase (mission-phase-from-doc vertex-doc)
        t (double (phase->t phase))]
    {:endpoint endpoint
     :phase (normalize-phase phase)
     :T t}))

(defn- source-state
  [endpoint hyperedges]
  (cond
    (sorry-endpoint? endpoint)
    (let [vertex-doc (find-vertex-doc endpoint hyperedges)
          t (sorry-t-from-doc vertex-doc)]
      {:endpoint endpoint
       :source-type :sorry
       :phase nil
       :T t})

    (file-endpoint? endpoint)
    {:endpoint endpoint
     :source-type :file
     :phase nil
     :T file-default-t}

    (mission-endpoint? endpoint)
    (let [{:keys [phase T]} (mission-state endpoint hyperedges)]
      {:endpoint endpoint
       :source-type :mission-doc
       :phase phase
       :T T})

    :else
    {:endpoint endpoint
     :source-type :unknown
     :phase nil
     :T 0.0}))

(defn- incoming-edge?
  [mission-endpoint hx]
  (let [edge-type (hx-type-str hx)
        eps (real-endpoints hx)]
    (and (incoming-edge-types edge-type)
         (= 2 (count eps))
         (= mission-endpoint (second eps)))))

(defn- summarize-contribs
  [contribs]
  {:n-edges (count contribs)
   :delta-T (double (reduce + 0.0 (map :grad-T contribs)))})

(defn delta-t-mission
  "Compute v0 mission ΔT over incoming related-mission + mission-cross-ref
   edges for the given mission endpoint string."
  ([mission-endpoint] (delta-t-mission mission-endpoint {}))
  ([mission-endpoint {:keys [limit futon1a-url]
                      :or {limit 200
                           futon1a-url FUTON1A}}]
   (let [cache (atom {})
         fetch* (fn [endpoint]
                  (or (get @cache endpoint)
                      (let [hyperedges (fetch-hyperedges-by-endpoint endpoint
                                                                     {:limit limit
                                                                      :futon1a-url futon1a-url})]
                        (swap! cache assoc endpoint hyperedges)
                        hyperedges)))
         target-hyperedges (fetch* mission-endpoint)
         {:keys [phase T]} (mission-state mission-endpoint target-hyperedges)
         incoming (->> target-hyperedges
                       (filter #(incoming-edge? mission-endpoint %))
                       vec)
         contributions
         (mapv (fn [edge]
                 (let [[source-endpoint _target] (real-endpoints edge)
                       edge-type (hx-type-str edge)
                       source-hyperedges (if (file-endpoint? source-endpoint)
                                           []
                                           (fetch* source-endpoint))
                       source (source-state source-endpoint source-hyperedges)
                       grad (- T (:T source))]
                   {:source-endpoint source-endpoint
                    :source-type (:source-type source)
                    :source-phase (:phase source)
                    :edge-type (edge-type->kw edge-type)
                    :source-T (double (:T source))
                    :grad-T (double grad)}))
               incoming)
         by-edge-type
         (into {}
               (for [[k xs] (group-by :edge-type contributions)]
                 [k (summarize-contribs xs)]))
         by-source-type
         (into {}
               (for [[k xs] (group-by :source-type contributions)]
                 [k (summarize-contribs xs)]))
         delta-t (double (reduce + 0.0 (map :grad-T contributions)))]
     {:mission-endpoint mission-endpoint
      :mission-phase phase
      :mission-T (double T)
      :delta-T delta-t
      :n-edges (count contributions)
      :by-edge-type by-edge-type
      :by-source-type by-source-type
      :per-edge-contributions contributions})))
