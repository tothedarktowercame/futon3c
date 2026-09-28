(ns futon3c.agency.disclosure-record
  "Pure validation and hyperedge mapping for disclosed choices.

   A disclosure records a visible choice inside a dispatched request. It is
   never a grant, scope, consent, or authority for another act. Prose fields
   are capped at 4096 characters: enough to quote a request paragraph and
   explain one choice, while preventing a per-choice record from becoming a
   second report or packet."
  (:require [clojure.string :as str]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.act-stamp :as act-stamp])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.time Instant]))

(def disclosure-type :disclosure/choice)
(def schema-version 1)
(def prose-limit 4096)

(def ^:private record-keys
  #{:id :kind :schema :author :at :source-job :unspecified :chosen
    :affects :inside-request :act/stamp :act/harness})
(def ^:private affects-keys #{:kind :id :path})
(def ^:private affects-kinds #{:git-commit :file :evidence})
(def ^:private inside-request-keys #{:basis :quote :text-sha256})

(defn- refuse! [reason field]
  (throw (ex-info "Invalid disclosure choice" {:reason reason :field field})))

(defn- text? [value]
  (and (string? value) (not (str/blank? value))))

(defn- bounded-text? [value]
  (and (text? value) (<= (count value) prose-limit)))

(defn- act-id? [value]
  (and (text? value) (boolean (re-matches #"act:.+" value))))

(defn- source-job-id? [value]
  (and (text? value) (str/starts-with? value "invoke-")))

(defn- sha256? [value]
  (and (string? value) (boolean (re-matches #"[0-9a-f]{64}" value))))

(defn- parse-instant! [value]
  (try (Instant/parse value)
       (catch Exception _ (refuse! :invalid-at :at))))

(defn- closed-map! [value allowed reason field]
  (when-not (map? value) (refuse! reason field))
  (when-let [key (first (remove allowed (keys value)))]
    (refuse! :unexpected-key key))
  value)

(defn validate!
  "Return a closed schema-1 disclosure or throw ex-info with typed reason data."
  [record]
  (when-not (map? record) (refuse! :invalid-record :record))
  (when-let [key (first (remove record-keys (keys record)))]
    (refuse! :unexpected-key key))
  (when-let [key (first (remove #(contains? record %) record-keys))]
    (refuse! :missing-field key))
  (when-not (act-id? (:id record)) (refuse! :invalid-act-id :id))
  (when-not (= disclosure-type (:kind record))
    (refuse! :wrong-record-kind :kind))
  (when-not (= schema-version (:schema record))
    (refuse! :unsupported-schema :schema))
  (when-not (text? (:author record)) (refuse! :missing-author :author))
  (parse-instant! (:at record))
  (when-not (source-job-id? (:source-job record))
    (refuse! :invalid-source-job :source-job))
  (doseq [field [:unspecified :chosen]]
    (when-not (bounded-text? (get record field))
      (refuse! :invalid-prose field)))
  (let [affects (closed-map! (:affects record) affects-keys
                             :invalid-affects :affects)]
    (when-not (contains? affects-kinds (:kind affects))
      (refuse! :invalid-affects-kind :affects))
    (when-not (text? (:id affects)) (refuse! :invalid-affects-id :affects))
    (when (and (contains? affects :path)
               (not (bounded-text? (:path affects))))
      (refuse! :invalid-affects-path :affects)))
  (let [inside (closed-map! (:inside-request record) inside-request-keys
                            :invalid-inside-request :inside-request)]
    (when-not (= :source-span (:basis inside))
      (refuse! :invalid-inside-request-basis :inside-request))
    (when-not (bounded-text? (:quote inside))
      (refuse! :invalid-request-quote :inside-request))
    (when-not (sha256? (:text-sha256 inside))
      (refuse! :invalid-request-hash :inside-request)))
  (act-stamp/validate! (:act/stamp record) #{:dispatch-edge})
  (when-not (= (:author record) (get-in record [:act/stamp :signer]))
    (refuse! :not-the-assignee :act/stamp))
  (act-harness/validate! (:act/harness record))
  record)

(defn sha256-text
  "Return the lowercase SHA-256 digest used to bind a source prompt."
  [text]
  (let [bytes (.getBytes (str text) StandardCharsets/UTF_8)
        digest (.digest (MessageDigest/getInstance "SHA-256") bytes)]
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest))))

(defn- edge-body [edge] (or (:evidence/body edge) edge))
(defn- edge-value [edge key plain-key]
  (or (get (edge-body edge) key) (get edge plain-key)))
(defn- edge-id [edge]
  (or (:evidence/id edge) (:source edge) (:evidence-id edge)))
(defn- invoke-edge-for? [source-job edge]
  (let [kind (edge-value edge :edge/kind :kind)
        kind (if (keyword? kind) kind (some-> kind keyword))]
    (and (= source-job (edge-value edge :edge/id :job-id))
         (= :invoke kind))))

(defn validate-against-source!
  "Validate DISCLOSURE against immutable SOURCE-PROMPT and its stored invoke
   EDGE evidence. EDGE may be one map or a collection; zero and duplicate
   matching dispatch edges refuse without guessing an orchestrator."
  [disclosure source-prompt edge]
  (let [disclosure (validate! disclosure)
        edges (cond (nil? edge) [] (map? edge) [edge] (sequential? edge) edge
                    :else [])
        matches (filterv #(invoke-edge-for? (:source-job disclosure) %) edges)]
    (when (empty? matches) (refuse! :orchestrator-unknown :source-job))
    (when (< 1 (count matches)) (refuse! :orchestrator-ambiguous :source-job))
    (let [edge (first matches)
          inside (:inside-request disclosure)
          author (:author disclosure)]
      (when-not (= (:text-sha256 inside) (sha256-text source-prompt))
        (refuse! :request-hash-mismatch :inside-request))
      (when-not (str/includes? (str source-prompt) (:quote inside))
        (refuse! :span-not-in-request :inside-request))
      (when-not (and (= author (edge-value edge :edge/to :to))
                     (= author (get-in disclosure [:act/stamp :signer])))
        (refuse! :not-the-assignee :author))
      (when-not (= {:dispatch-edge (edge-id edge)}
                   (get-in disclosure [:act/stamp :authority]))
        (refuse! :authority-not-dispatch-edge :act/stamp))
      disclosure)))

(defn- affected-endpoint [{:keys [kind id]}]
  (str (name kind) ":" id))

(defn ->hyperedge
  "Map a validated disclosure to a schema-1 minted-act hyperedge."
  [record]
  (let [record (validate! record)]
    {:hx/id (:id record)
     :hx/type disclosure-type
     :hx/valid-time (:at record)
     ;; The act id is :hx/id alone, as for offers, agreements and grants:
     ;; futon1b mints it after storing endpoints, so it cannot be one.
     :hx/endpoints [(str "job:" (:source-job record))
                    (str "agent:" (:author record))
                    (affected-endpoint (:affects record))]
     :hx/props (dissoc record :id :kind)}))

(defn hyperedge->record
  "Losslessly map one schema-1 disclosure hyperedge to its plain record."
  [hyperedge]
  (when-not (= disclosure-type (:hx/type hyperedge))
    (refuse! :wrong-record-kind :hx/type))
  (let [props (:hx/props hyperedge)
        record (assoc (dissoc props :at)
                      :id (:hx/id hyperedge)
                      :kind disclosure-type
                      :at (or (:hx/valid-time hyperedge) (:at props)))]
    (validate! record)))

(defn unrecorded-citations
  "Return cited act ids absent from STORED-ACT-IDS. Pass every stored act id
   the report could cite (grants and offers too), or a cited grant is reported
   as an unrecorded disclosure. This detects only explicit citations; an
   undisclosed choice with no id remains unknowable."
  [report-text stored-act-ids]
  (let [stored (set stored-act-ids)]
    (->> (re-seq #"(?<![A-Za-z0-9:_-])act:[A-Za-z0-9][A-Za-z0-9:_-]*"
                 (str report-text))
         distinct
         (remove stored)
         (mapv (fn [id] {:id id :reason :disclosure-unrecorded})))))
