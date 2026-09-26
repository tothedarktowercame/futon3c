(ns futon3c.inbox-zero.attribution
  "Read-side attribution of inbox-zero commit links to durable clock lineage.

  Temporal validity belongs to the substrate query basis, not to fields on an
  edge document. The IO boundary therefore fetches each agent's clock edges at
  the link instant; the pure projection still guards against future-clocked
  fixture or caller data. Fetch failure is :unknown, never false absence."
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3.inbox-zero.state :as inbox-state])
  (:import [java.net URLEncoder]
           [java.time Instant]
           [java.util Date]))

(def clock-type "clock/clocked-on")
(def query-limit 1000)

(defn- prop [props key]
  (or (get props key) (get props (name key))))

(defn- epoch-ms [instant]
  (cond
    (instance? Date instant) (.getTime ^Date instant)
    (instance? Instant instant) (.toEpochMilli ^Instant instant)
    (integer? instant) instant
    (string? instant) (.toEpochMilli (Instant/parse instant))
    :else (throw (ex-info "Unsupported attribution instant"
                          {:error/type :inbox-zero/invalid-instant
                           :value instant}))))

(defn- strict-iso-instant [instant]
  (.toString (Instant/ofEpochMilli (epoch-ms instant))))

(defn- seat-identity [seat-id]
  (when (string? seat-id)
    (when-let [[_ agent-id session-id] (re-matches #"seat:([^:]+):(.+)" seat-id)]
      {:agent-id agent-id :session-id session-id})))

(defn- edge-target [edge]
  (let [props (:hx/props edge)]
    (or (prop props :excursion-id)
        (prop props :mission-id)
        (prop props :campaign-id)
        (first (remove #(str/starts-with? (str %) "agent:")
                       (:hx/endpoints edge))))))

(defn- result [link status primary]
  {:link/id (:link/id link)
   :seat/id (:seat/id link)
   :primary primary
   :attribution/basis :clock-lineage-as-of
   :attribution/status status})

(defn attribute-links
  "Purely attribute LINKS from one already-fetched temporal BASIS.

  BASIS is `{:basis-instant t :edges [...]}`. The substrate query is
  responsible for excluding edges not valid at t. This projection additionally
  requires exact agent/session props and rejects an edge whose clocked-at-ms is
  later than a link's linked-at. Distinct simultaneous targets are ambiguous."
  [links {:keys [basis-instant edges]}]
  ;; Realize the basis eagerly so malformed instants fail before partial output.
  (epoch-ms basis-instant)
  (mapv
   (fn [link]
     (let [{:keys [agent-id session-id]} (seat-identity (:seat/id link))
           linked-at-ms (epoch-ms (:linked-at link))
           targets
           (->> edges
                (keep (fn [edge]
                        (let [props (:hx/props edge)
                              clocked-at (prop props :clocked-at-ms)]
                          (when (and agent-id session-id
                                     (= agent-id (some-> (prop props :agent-id) str))
                                     (= session-id (some-> (prop props :session-id) str))
                                     (integer? clocked-at)
                                     (<= clocked-at linked-at-ms))
                            (some-> (edge-target edge) str)))))
                distinct
                vec)]
       (case (count targets)
         0 (result link :unattributed nil)
         1 (result link :attributed (first targets))
         (result link :ambiguous nil))))
   links))

(defn- unknown-results [links]
  (mapv #(result % :unknown nil) links))

(defn- encoded [value]
  (URLEncoder/encode (str value) "UTF-8"))

(defn- fetch-basis [http-get base-url agent-id linked-at]
  (let [instant (strict-iso-instant linked-at)
        url (str (str/replace base-url #"/$" "")
                 "/api/alpha/hyperedges?type=" (encoded clock-type)
                 "&end=" (encoded (str "agent:" agent-id))
                 "&as-of=" (encoded instant)
                 "&limit=" query-limit
                 "&include-total=false")
        response (http-get url {:headers {"Accept" "application/edn"}
                                :throw false})]
    (when (= 200 (:status response))
      (let [body (:body response)
            parsed (when (string? body) (edn/read-string body))
            edges (:hyperedges parsed)]
        (when (sequential? edges)
          {:basis-instant linked-at :edges edges})))))

(defn attribute-links-from-clock-store
  "Fetch temporal clock bases and attribute LINKS.

  Links are grouped by exact agent and linked-at instant, producing one bounded
  type+end+as-of query per group. Any exception, non-200 response, or malformed
  EDN marks only that group's links :unknown. HTTP-GET is injectable for tests."
  ([links] (attribute-links-from-clock-store links {}))
  ([links {:keys [base-url http-get]
           :or {base-url (or (System/getenv "FUTON_SUBSTRATE_URL")
                             (System/getenv "FUTON1A_URL")
                             "http://localhost:7071")
                http-get http/get}}]
   (let [indexed (map-indexed vector links)
         groups (group-by (fn [[_ link]]
                            (let [{:keys [agent-id]} (seat-identity (:seat/id link))]
                              [agent-id (epoch-ms (:linked-at link))]))
                          indexed)
         attributed
         (reduce-kv
          (fn [results [agent-id _] indexed-links]
            (let [group-links (mapv second indexed-links)
                  group-results
                  (if-not agent-id
                    (attribute-links group-links
                                     {:basis-instant (:linked-at (first group-links))
                                      :edges []})
                    (try
                      (if-let [basis (fetch-basis http-get base-url agent-id
                                                  (:linked-at (first group-links)))]
                        (attribute-links group-links basis)
                        (unknown-results group-links))
                      (catch Exception _
                        (unknown-results group-links))))]
              (into results (map vector (map first indexed-links) group-results))))
          {}
          groups)]
     (mapv attributed (range (count links))))))

(defn attribute-state
  "Attribute every stored session-commit link in inbox-zero STATE.

  This is the complete read-side IO wrapper: inbox-zero state remains
  immutable, and only temporal clock-lineage reads occur."
  ([state] (attribute-state state {}))
  ([state options]
   (attribute-links-from-clock-store
    (inbox-state/records-of-type state :inbox-zero/session-commit-link)
    options)))
