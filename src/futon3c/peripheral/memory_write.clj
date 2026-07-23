(ns futon3c.peripheral.memory-write
  "Write-side seam for deliberate Zai memories.

   P0 records one append-only evidence entry followed by one memory/assert
   hyperedge.  The evidence body remains authoritative when the second write
   fails, so callers receive a successful receipt plus :hx-error in that case."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.evidence.boundary :as boundary]
            [org.httpkit.client :as http])
  (:import [java.time Instant]
           [java.util UUID]))

(defn- now-str []
  (str (Instant/now)))

(defn- social-error
  [component code message & {:as context}]
  (cond-> {:error/component component
           :error/code code
           :error/message message
           :error/at (now-str)}
    (seq context) (assoc :error/context context)))

(defn- elapsed-ms
  [started-ns]
  (/ (double (- (System/nanoTime) started-ns)) 1000000.0))

(defn- log-write!
  [stage id elapsed ok?]
  (binding [*out* *err*]
    (println (format "[memory-record] %s id=%s elapsed-ms=%.3f ok=%s"
                     stage id elapsed (boolean ok?)))))

(defn- normalize-kind
  [kind]
  (cond
    (keyword? kind) kind
    (and (string? kind) (not (str/blank? kind)))
    (keyword (str/replace kind #"^:" ""))
    :else kind))

(defn- normalize-subject
  [subject]
  (cond-> subject
    (and (map? subject)
         (string? (:ref/type subject))
         (not (str/blank? (:ref/type subject))))
    (update :ref/type #(keyword (str/replace % #"^:" "")))))

(defn- normalize-payload
  [payload]
  (let [how-to-apply (or (:how-to-apply payload) (:how_to_apply payload))
        volatile? (cond
                    (contains? payload :volatile?) (:volatile? payload)
                    (contains? payload :volatile) (:volatile payload)
                    :else false)]
    (cond-> (-> payload
                (assoc :kind (normalize-kind (:kind payload))
                       :subjects (mapv normalize-subject (or (:subjects payload) []))
                       :volatile? (boolean volatile?))
                (dissoc :how_to_apply :volatile))
      (some? how-to-apply) (assoc :how-to-apply how-to-apply))))

(defn- entry-body
  [payload]
  (select-keys payload [:name :hook :kind :body :why :how-to-apply]))

(defn- resolve-distill
  [{:keys [turn-id round]} distill]
  (if (= "@current-round" distill)
    {:turn-id (some-> turn-id str)
     :round round}
    distill))

(defn- memory-hyperedge
  [{:keys [session-id mission-id domain] :as ctx} payload evidence-id hx-id]
  (let [subject-ids (mapv (comp str :ref/id) (:subjects payload))
        pattern-ids (->> (:subjects payload)
                         (filter #(= :pattern (:ref/type %)))
                         (mapv (comp str :ref/id)))
        distills (mapv #(resolve-distill ctx %) (or (:distills payload) []))
        explicit-distill-ids (filterv #(and (string? %)
                                            (str/starts-with? % "e-"))
                                      distills)
        endpoints (->> (concat [evidence-id]
                               subject-ids
                               explicit-distill-ids
                               [(some-> session-id str)
                                (some-> mission-id str)])
                       (remove str/blank?)
                       distinct
                       vec)
        roles (cond-> {:entry evidence-id
                       :subjects subject-ids
                       :distills distills
                       :session (some-> session-id str)}
                (seq pattern-ids) (assoc :patterns pattern-ids)
                mission-id (assoc :mission (str mission-id)))]
    {:hx/id hx-id
     :hx/type :memory/assert
     :hx/endpoints endpoints
     :hx/props (cond-> {:roles roles
                        :kind (:kind payload)
                        :name (:name payload)
                        :hook (:hook payload)
                        :volatile? (:volatile? payload)
                        :state :current}
                 domain (assoc :domain domain)
                 ;; Agent-supplied pattern subjects are proposals.  A
                 ;; librarian/reviewer must promote the attachment before it
                 ;; can surface as a recall warrant.
                 (seq pattern-ids) (assoc :attachment-status :proposed)
                 (contains? payload :facets)
                 (assoc :facets (vec (:facets payload))))}))

(defn- store-base-url
  [evidence-store]
  (some-> (or (:base-url evidence-store)
              (System/getenv "FUTON1B_URL")
              (System/getenv "FUTON_SUBSTRATE_URL")
              (System/getenv "FUTON1A_URL"))
          (str/replace #"/+$" "")))

(defn- penholder []
  (or (System/getenv "FUTON1B_PENHOLDER")
      (System/getenv "FUTON1A_PENHOLDER")
      "api"))

(defn post-hyperedge!
  "POST one hyperedge to the URL carried by the configured evidence backend.
   Kept as a narrow public seam so tests can substitute an in-memory graph."
  [{:keys [evidence-store]} hyperedge]
  (if-let [base-url (store-base-url evidence-store)]
    (try
      (let [{:keys [status body error]}
            @(http/post (str base-url "/api/alpha/hyperedge")
                        {:timeout 30000
                         :as :text
                         :headers {"content-type" "application/edn"
                                   "accept" "application/edn"
                                   "x-penholder" (penholder)}
                         :body (pr-str hyperedge)})
            parsed (when (seq (str body))
                     (try (edn/read-string {:default (fn [_tag value] value)} body)
                          (catch Throwable _ body)))]
        (cond
          error
          {:ok false
           :error (social-error :transport :hyperedge-unreachable
                                "Hyperedge write transport failed"
                                :detail (str error))}

          (<= 200 (long status) 299)
          {:ok true
           :hyperedge (or (:hyperedge parsed) hyperedge)}

          :else
          {:ok false
           :error (social-error :E-store :hyperedge-rejected
                                (str "Hyperedge write returned HTTP " status)
                                :status status :body parsed)}))
      (catch Throwable t
        {:ok false
         :error (social-error :transport :hyperedge-exception
                              (or (.getMessage t) "Hyperedge write failed")
                              :exception-class (.getName (class t)))}))
    {:ok false
     :error (social-error :E-store :missing-substrate-url
                          "No substrate URL is configured on the evidence backend or environment")}))

(defn record-memory!
  "Record one P0 assert memory.

   ctx carries trusted agent/session/turn identity and the evidence backend;
   payload identity fields are deliberately ignored.  The evidence id is
   minted before validation or I/O and is returned on every failure."
  [{:keys [agent-id session-id evidence-store] :as ctx} raw-payload]
  (let [evidence-id (str "e-" (UUID/randomUUID))
        ;; Derive the edge id from the entry id: the body identity is fixed at
        ;; creation, and any later repair can therefore reproduce this id.
        hx-id (str "hx-mem-" (subs evidence-id 2))
        payload (normalize-payload raw-payload)
        primary-subject (first (:subjects payload))
        ;; Never assoc nil identity keys: EvidenceEntry rejects present-but-nil
        ;; (live failure 2026-07-22 — nil session-id killed all 8 of zai-3's
        ;; memory_record attempts). Absent keys pass; nil values do not.
        entry (cond-> {:evidence/id evidence-id
                       :evidence/subject primary-subject
                       :evidence/type :memory
                       :evidence/claim-type :assert
                       :evidence/at (now-str)
                       :evidence/body (entry-body payload)
                       :evidence/tags [:memory :memory/assert]}
                agent-id (assoc :evidence/author (str agent-id))
                session-id (assoc :evidence/session-id (str session-id)))
        entry-start (System/nanoTime)
        entry-receipt (boundary/append! evidence-store entry)
        entry-ms (elapsed-ms entry-start)]
    (log-write! "entry-write" evidence-id entry-ms (:ok entry-receipt))
    (if-not (:ok entry-receipt)
      {:ok false
       :id evidence-id
       :error (social-error :E-store
                            (or (:error/code entry-receipt) :memory-entry-failed)
                            (or (:error/message entry-receipt)
                                "Memory evidence entry was rejected")
                            :receipt entry-receipt)
       :elapsed-ms {:entry entry-ms}}
      (let [hyperedge (memory-hyperedge ctx payload evidence-id hx-id)
            hx-start (System/nanoTime)
            hx-receipt (post-hyperedge! ctx hyperedge)
            hx-ms (elapsed-ms hx-start)
            base {:ok true
                  :id evidence-id
                  :hx-id hx-id
                  :elapsed-ms {:entry entry-ms :hyperedge hx-ms}}]
        (log-write! "hyperedge-write" hx-id hx-ms (:ok hx-receipt))
        (if (:ok hx-receipt)
          base
          (do
            (binding [*out* *err*]
              (println (str "[memory-record] HYPEREDGE WRITE FAILED after evidence landed "
                            "id=" evidence-id " hx-id=" hx-id " error="
                            (pr-str (:error hx-receipt)))))
            (assoc base :hx-error (:error hx-receipt))))))))
