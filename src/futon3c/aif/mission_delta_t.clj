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
            [clojure.edn :as edn]
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

;; 1a→1b port (2026-07-17): the retired futon1a XTDB1 store (7071) is gone —
;; futon1b is the live substrate and serves `application/edn`, NOT JSON. It also
;; does not serve the `?end=<endpoint>` targeted route (it hangs); only the
;; `?type=<hx-type>` route responds. So we fetch the fixed set of edge/vertex
;; families delta-t needs by TYPE and filter to those touching the endpoint.
;; Shape differences handled: EDN body (edn/read-string), and `:hx/props` that
;; arrives either as an EDN-encoded string OR an already-parsed map. Endpoints
;; already arrive as a string list under `:hx/endpoints`, so `real-endpoints`
;; is unchanged.
(def ^:private fetch-types
  ["code/v05/mission-doc"
   "code/v05/related-mission"
   "code/v05/mission-cross-ref"
   "code/v05/file→mission"
   "code/v05/sorry-doc"])

(defn- selected-fetch-types
  "Return FAMILIES in canonical fetch order, rejecting requests outside the
   delta-T family universe. Nil retains the full historical family set."
  [families]
  (if (nil? families)
    fetch-types
    (let [requested (set families)
          known (set fetch-types)
          unknown (remove known requested)]
      (when (seq unknown)
        (throw (ex-info "Unknown mission delta-T fetch families"
                        {:unknown-families (vec (sort unknown))
                         :known-families fetch-types})))
      (filterv requested fetch-types))))

(defn- parse-edn-body
  [body]
  (cond
    (string? body) (try (edn/read-string body) (catch Exception _ nil))
    (map? body) body
    :else nil))

(defn- normalize-props
  "1b delivers `:hx/props` as an EDN-encoded string on some records and a parsed
   map on others. Normalize to a map so `mission-phase-from-doc` / `sorry-t-from-doc`
   read it uniformly."
  [hx]
  (let [p (:hx/props hx)]
    (if (string? p)
      (assoc hx :hx/props (or (try (edn/read-string p) (catch Exception _ nil)) {}))
      hx)))

(defn fetch-hyperedges-by-type
  "Fetch all hyperedges of one type from the live substrate (the `?type=` route,
   which 1b serves; the `?end=` route is not served and hangs).

   Best-effort: a slow/oversized/unavailable type degrades to `[]` rather than
   throwing, so one flaky edge-family cannot break the essential `mission-doc`
   read that supplies `:mission-T`. `:mission-T` is preserved because `mission-doc`
   is the reliable family; only the ΔT contributions from a failed family are lost."
  [futon1a-url hx-type limit]
  (let [url (str futon1a-url "/api/alpha/hyperedges?type="
                 (url-encode hx-type) "&limit=" limit)
        ;; The essential `mission-doc` family (supplies :mission-T) is a large
        ;; read — ~6-9s in the loaded serving JVM — so it gets ample headroom.
        ;; The ΔT-only edge families stay short: they are best-effort and some
        ;; are oversized/absent in 1b, so a tight bound keeps selection snappy.
        timeout-ms (if (= hx-type "code/v05/mission-doc") 30000 6000)]
    (try
      (let [resp (http/get url {:headers {"Accept" "application/edn"
                                          "X-Penholder" PENHOLDER}
                                :throw false
                                :timeout timeout-ms})]
        (if (= 200 (:status resp))
          (mapv normalize-props (or (:hyperedges (parse-edn-body (:body resp))) []))
          (do (binding [*out* *err*]
                (println "[mission-delta-t] hyperedges type fetch non-200"
                         {:hx-type hx-type :status (:status resp)}))
              [])))
      (catch Exception e
        (binding [*out* *err*]
          (println "[mission-delta-t] hyperedges type fetch failed"
                   {:hx-type hx-type :error (.getMessage e)}))
        []))))

;; Per-process snapshot cache: one fetch per (base,type) is reused across all
;; missions in a judgement. The WM full loop runs as a fresh JVM per click, so
;; this is a point-in-time snapshot, never stale across clicks; call
;; `reset-type-cache!` to force a refresh inside a long-lived JVM.
(def ^:private type-cache (atom {}))

(defn reset-type-cache! [] (reset! type-cache {}))

(defn- cached-fetch-by-type
  [futon1a-url hx-type limit]
  (let [k [futon1a-url hx-type]]
    (if-let [hit (get @type-cache k)]
      hit
      (let [v (fetch-hyperedges-by-type futon1a-url hx-type limit)]
        ;; Never cache an empty essential `mission-doc` result — that only
        ;; happens on a transient failure, and caching it would poison every
        ;; mission's :mission-T to the nil-phase default for the process
        ;; lifetime. Edge families legitimately return empty and stay cached.
        (when-not (and (= hx-type "code/v05/mission-doc") (empty? v))
          (swap! type-cache assoc k v))
        v))))

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
  "Return all hyperedges (of the families delta-t cares about) touching `endpoint`.

   1b does not serve the targeted `?end=` route, so we fetch the fixed
   `fetch-types` families via the served `?type=` route (per-process cached) and
   filter to those whose endpoints include `endpoint`. Semantically equivalent to
   the old 1a targeted read for delta-t's purposes. `:families` may restrict the
   read to a validated subset; nil preserves the complete family set."
  ([endpoint] (fetch-hyperedges-by-endpoint endpoint {}))
  ([endpoint {:keys [limit futon1a-url families]
              :or {limit 500
                   futon1a-url FUTON1A}}]
   (->> (selected-fetch-types families)
        (mapcat #(cached-fetch-by-type futon1a-url % limit))
        (filter (fn [hx] (some #(= endpoint %) (real-endpoints hx))))
        vec)))

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
   edges for the given mission endpoint string. `:families` is forwarded to
   endpoint reads; omitting it preserves full-family behavior."
  ([mission-endpoint] (delta-t-mission mission-endpoint {}))
  ([mission-endpoint {:keys [limit futon1a-url families]
                      :or {limit 200
                           futon1a-url FUTON1A}}]
   (let [cache (atom {})
         fetch* (fn [endpoint]
                  (or (get @cache endpoint)
                      (let [hyperedges (fetch-hyperedges-by-endpoint endpoint
                                                                     {:limit limit
                                                                      :futon1a-url futon1a-url
                                                                      :families families})]
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
