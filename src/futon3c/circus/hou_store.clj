(ns futon3c.circus.hou-store
  "Read-only snapshot assembly and one atomic focus publication. No Giraffe
  precedence wiring. Needs come from the party-pinned run's input.edn, produced
  by run.clj build-adapter-result/write-run!: excursions.clj:156-158 installs
  checked edges; readings.clj:110-114 reverses unblocks ONCE; pass input edges
  already mean from needs to. Alternatives are excluded as in party.py:335-349.
  Each source reading is a separate incidence node, not one node per run."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mention-writer :as writer]
            [futon3c.circus.hou :as hou]
            [futon3c.xiang.turn-store :as turns]))

(defn default-config []
  (merge (writer/default-config)
         {:party-path (str (System/getProperty "user.home") "/storage/gcc-preview/party.json")
          :runs-dir (str (System/getProperty "user.home") "/storage/giraffe")
          :rule :named-items :associated-radius 1}))

(defn- sha-file [path]
  (digest/sha256-hex (java.nio.file.Files/readAllBytes (.toPath (io/file path)))))
(defn- id-text [x] (if (keyword? x) (name x) x))
(defn- require! [ok why] (when-not ok (throw (ex-info "focus evidence refused" {:reason why}))))

(defn read-needs
  "The input and individual reading lines must match the run's certificates.
  No producer, subprocess, or current-pointer substitution. A certificate does
  not imply that today's source documents still match: their pins are checked
  too, and all bytes consulted enter the header's sources/pin."
  [cfg]
  (let [party (json/parse-string (slurp (:party-path cfg)) true)
        run-name (get-in party [:sources :giraffe_run])
        _ (require! (and (string? run-name) (= run-name (.getName (io/file run-name)))) :invalidEvidence)
        dir (io/file (:runs-dir cfg) run-name)
        manifest (edn/read-string (slurp (io/file dir "manifest.edn")))
        path (str (io/file dir "input.edn"))
        input-bytes (java.nio.file.Files/readAllBytes (.toPath (io/file path)))
        sha (digest/sha256-hex input-bytes)
        _ (require! (= sha (:sha256 (first (filter #(= "input.edn" (:file %)) (:files manifest))))) :invalidEvidence)
        inp (edn/read-string (String. ^bytes input-bytes "UTF-8"))
        edges (remove #(= :alternative (:class %)) (:edges inp))
        owners (group-by (comp id-text :reading-member) edges)
        _ (require! (not (contains? owners nil)) :invalidEvidence)
        members (into {} (map (fn [m] [(id-text (:id m)) m])) (get-in inp [:graph-coverage :members]))
        generation (get-in manifest [:readings-generation :path])
        source-files (atom [{:kind :file :path path :sha256 sha}])
        item-set (some-> (:item-ids cfg) set)
        readings
        (into [] (keep (fn [[owner es]]
                (let [member (get members owner)
                      pin (:record-pin member)
                      batch (io/file generation (.getName (io/file (:batch pin))))
                      line (nth (str/split (slurp batch) #"\n") (dec (:line pin)))
                      _ (require! (= (:sha256 pin) (digest/sha256-hex (.getBytes line "UTF-8"))) :invalidEvidence)
                      raw (json/parse-string line true)
                      _ (require! (and (= :read (:status member)) (= owner (:id raw))) :invalidEvidence)
                      pins (cons (:source raw) (keep :evidence (:edges raw)))
                      ;; The Giraffe generation can retain readings for files
                      ;; deliberately excluded from the GCC item inventory
                      ;; (for example completed requisition packets). Such a
                      ;; reading is not a GCC record: admitting it makes
                      ;; LoopBuilt false because it has no item endpoint.
                      incident-items (vec (distinct (mapcat (fn [e] [owner (:to e)])
                                                  (remove #(= "alternative" (or (:intended_class %) (:intended-class %)))
                                                          (:edges raw)))))]
                  (when (or (nil? item-set) (some item-set incident-items))
                    (doseq [p pins]
                      (require! (and (string? (:path p)) (string? (:sha256 p))
                                     (= (:sha256 p) (sha-file (:path p)))) :evidenceChanged)
                      (swap! source-files conj {:kind :file :path (:path p) :sha256 (:sha256 p)}))
                    (swap! source-files conj {:kind :file :path (str batch) :sha256 (sha-file batch)})
                    {:record {:id (str "giraffe-reading/" owner) :reading raw :line-sha256 (:sha256 pin)}
                     :needs (mapv #(vector (id-text (:from %)) (id-text (:to %))) es)
                     ;; Basic.GCC.incident uses every accepted edge of this
                     ;; reading, including part-of/relates-to, not only needs.
                     :incident-items incident-items}))))
              (sort-by key owners))
        basis {:id (str "giraffe-input/" run-name) :sha256 sha
               :generation (:readings-generation manifest)
               :manifest-files (:files manifest)}]
    {:readings readings :basis-records [basis] :sources (vec (distinct @source-files))}))

(defn settled? [turn]
  (contains? #{"analyzed" "drafted" "declared" "not-requested"} (:analysis_status turn)))
(defn reply-landed? [turn] (and (contains? turn :reply_text) (string? (:reply_text turn))))

(defn snapshot [cfg turn]
  (let [items (writer/inventory cfg nil)
        cfg (assoc cfg :item-ids items)
        plans (mapv #(writer/plan-field cfg turn %) [:source_text :reply_text])
        refs (vec (keep :reference plans))
        ms (vec (mapcat :mentions plans))
        source-ref (get-in (first plans) [:reference :id])
        need-data ((or (:read-needs cfg) read-needs) cfg)
        turn-basis {:id (str "turn-record/" (:id turn))
                    :fields (select-keys turn [:source_text :reply_text :mission_id :analysis_status :created_at :origin])}
        basis-records (vec (concat (:basis-records need-data) [turn-basis] (map hou/item-record items)))
        local-records (concat refs basis-records (map :record (:readings need-data)))
        arxana ms
        stored (into (into {} (map (juxt :id identity)) local-records)
                     (map (fn [r] [(:id r) (:ok (store/get-entity (:graph-store cfg) (:id r)))])) arxana)
        text-by-ref (into {} (keep #(when (:reference %) [(get-in % [:reference :id]) (:text %)])) plans)
        turn-source {:kind :turn :dir (:dir (:turn-store cfg)) :id (:id turn)
                     :sha256 (digest/canonical-digest (select-keys turn [:source_text :reply_text :mission_id :analysis_status :created_at :origin]))}]
    (merge need-data
           {:turn-id (:id turn) :at (or (:created_at turn) "") :mission-id (:mission_id turn)
            :settled? (settled? turn) :reply-landed? (reply-landed? turn)
            :rule (:rule cfg :named-items) :associated-radius (:associated-radius cfg 1)
            :basis-records basis-records :items items :references refs :mention-records ms :source-ref source-ref
            :text-by-ref text-by-ref :stored stored
            :sources (vec (concat (:sources need-data) [turn-source]
                                  (when source-ref [{:kind :evidence :id (:evidence_id turn)
                                                     :sha256 (:revision/digest (first refs))}])
                                  (map (fn [r] {:kind :arxana :id (:id r) :sha256 (digest/canonical-digest r)}) arxana)))})))

(defn- attempt! [cfg turn why]
  ;; The port supports attempt/append, but requires an EXISTING clusion subject
  ;; (futon1b_arxana.clj:719-721). Use a verified item when possible; if none
  ;; exists, log the failure instead of inventing a focus header to attach it to.
  (if-let [id (first (writer/inventory cfg (:source_text turn)))]
    (let [a (records/make-attempt
             {:subject/id (:id (hou/item-record id)) :dynamics {:id "hou/focus" :version 1}
              :inputs {:turn (:id turn)} :outcome :refused :reason why
              :actor "futon3c/hou" :time ((or (:now-ms cfg) #(System/currentTimeMillis)))})
          result (http/append-batch! (:graph-store cfg) (str "hou/attempt/" (:id a))
                                      [{:operation :attempt/append :record a}])]
      (require! (:ok result) :readerFailedAfterRetries)
      result)
    (binding [*out* *err*] (println "[hou] failure without existing receipt subject" (:id turn) why))))

(defn write-focus!
  "One focus batch: header, links, attestation. :parser attests deterministic
  extraction/graph calculation, not a claim about unspoken operator intent.
  Readiness is checked before any snapshot I/O. Typed failures remain visible
  as attempt receipts (or logs if there is no valid existing receipt subject)."
  [cfg turn]
  (let [out (cond
              (not (settled? turn)) {:failed :turnNotSettled}
              (not (reply-landed? turn)) {:failed :replyNotLanded}
              (not= "operator" (or (:origin turn) "operator")) {:failed :invalidEvidence}
              :else (try (hou/hou-run (snapshot cfg turn))
                         (catch Exception e
                           (let [reason (or (:reason (ex-data e)) :invalidEvidence)]
                             (binding [*out* *err*]
                               (println "[hou] snapshot refused" (:id turn) reason
                                        (.getName (class e)) (.getMessage e) (pr-str (ex-data e))))
                             {:failed reason}))))]
    (if-let [header (:written out)]
      (let [att (records/make-attestation {:subject-clusion-id (:id header) :basis :parser
                                         :standing :verified :coverage :complete :producer "futon3c/hou"
                                         :method {:name :exact-turn-focus :version 1}})
            rows (into [{:operation :clusion/assert :record header}]
                       (concat (map #(hash-map :operation :clusion/assert :record %) (:links out))
                               [{:operation :evidence/append :record att}]))
            result (http/append-batch! (:graph-store cfg) (str "hou/focus/" (:id header)) rows)]
        (require! (:ok result) :readerFailedAfterRetries)
        out)
      (do (attempt! cfg turn (:failed out)) out))))

(defn- reverse-records [cfg type role id]
  (let [r (:ok (store/bounded-query! (:graph-store cfg)
                {:query/type :role-reverse :edge-type type
                 :endpoint-role role :endpoint-id id :limit 200 :order :desc}))]
    (require! (and (= :ok (:status r)) (not (:truncated? r))) :invalidEvidence)
    (mapv #(let [record (:ok (store/get-entity (:graph-store cfg) %))]
             (require! record :invalidEvidence) record) (:edge-ids r))))

(defn- complete-header-links [cfg header]
  ;; The endpoint index caps pages at 200. Header content determines every
  ;; expected immutable link ID, so get those directly and require the index's
  ;; TOTAL to equal their count. This detects extra/missing/altered links even
  ;; for a full 500-record atomic batch, without trusting a truncated page.
  (let [expected (hou/relation-links header)
        q (:ok (store/bounded-query! (:graph-store cfg)
                {:query/type :role-reverse :edge-type [:hou/focus-relation 1]
                 :endpoint-role :focus :endpoint-id (:id header) :limit 200 :order :desc}))]
    (require! (and (= :ok (:status q)) (= (count expected) (:total q))) :invalidEvidence)
    (mapv (fn [e]
            (let [r (:ok (store/get-entity (:graph-store cfg) (:id e)))]
              (require! (= e r) :invalidEvidence) r)) expected)))

(defn- source-reference-from-turn
  "Reconstruct the immutable source reference identity from the turn record.
  This deliberately does not read EL text: focus consumers read the already
  published Hou header, rather than re-running mention extraction."
  [cfg turn]
  (when (and (string? (:source_text turn)) (seq (:evidence_id turn)))
    (records/make-external-reference
     {:ref/kind :evidence-text
      :store/id "futon1b/evidence"
      :store/endpoint (:evidence-url cfg "http://localhost:7073")
      :record/id (str (:evidence_id turn) "/body/text")
      :schema/authority :futon1b/evidence-text
      :valid-time (or (:created_at turn) "unknown")
      :revision/digest (digest/sha256-hex (.getBytes ^String (:source_text turn) "UTF-8"))})))

(defn headers-for-turn
  "Read and validate the complete stored Hou header(s) for one turn. Returns a
  typed pending result when publication is absent or its fixed-arity links do
  not agree; it never derives focus from turn text."
  [cfg turn]
  (try
    (if-let [ref (source-reference-from-turn cfg turn)]
      (let [headers (->> (reverse-records cfg [:hou/focus 1] :evidence (:id ref))
                         (filter #(= (:id turn) (get-in % [:asserted-content :hou/turn])))
                         (sort-by (juxt #(get-in % [:asserted-content :hou/at]) :id))
                         vec)]
        (if (empty? headers)
          {:pending (cond (not (settled? turn)) :turn-not-settled
                          (not (reply-landed? turn)) :reply-not-landed
                          :else :header-absent)}
          (let [checked (mapv (fn [h]
                                (require! (hou/header-links-agree? h (complete-header-links cfg h))
                                          :invalidEvidence)
                                h) headers)]
            {:headers checked})))
      {:pending :source-reference-unavailable})
    (catch Exception e
      {:pending (if (= :invalidEvidence (:reason (ex-data e)))
                  :incomplete-header-links :header-read-failed)})))

(defn newest-headers
  "Discover via turn-field evidence, not item links: otherwise an empty newer
  focus would incorrectly resurrect an older focus. The bounded Arxana API has
  no global type scan. Walk turn inventory newest-first and stop at the newest
  turn with headers. Ordering is turn created_at then header id, deterministic
  on replay (not the time a backfill happens to write). No per-target fallback."
  [cfg]
  (loop [ts (seq (turns/list-records (:turn-store cfg) :limit Long/MAX_VALUE))]
    (when-let [{:keys [id record]} (first ts)]
      (let [ref (:reference (writer/field-evidence cfg (assoc record :id id) :source_text))
            headers (when ref (reverse-records cfg [:hou/focus 1] :evidence (:id ref)))]
        (if (seq headers) headers (recur (next ts)))))))

(defn- source-fresh? [cfg {:keys [kind path id dir sha256]}]
  (= sha256
     (case kind
       :evidence (digest/sha256-hex (.getBytes ^String (writer/evidence-text cfg id) "UTF-8"))
       :file (sha-file path)
       :arxana (some-> (:ok (store/get-entity (:graph-store cfg) id)) digest/canonical-digest)
       :turn (some-> (turns/read-record {:dir dir} id)
                     (select-keys [:source_text :reply_text :mission_id :analysis_status :created_at :origin]) digest/canonical-digest)
       nil)))

(defn newest-focus-input
  "Newest turn focus, restricted to TARGETS only after selecting its header. A stale/malformed newest header gives nil,
  never fallback to an older interpretation. Complete links and every source
  pin are checked; truncated reverse queries are typed refusal internally.
  No integration into retained-focus-input is performed in this packet."
  ([targets] (newest-focus-input (default-config) targets))
  ([cfg targets]
   (try
     (let [headers (newest-headers cfg)
           h (last (sort-by (juxt #(get-in % [:asserted-content :hou/at]) :id) headers))]
       (when (and h (hou/header-links-agree? h (complete-header-links cfg h))
                  (every? #(source-fresh? cfg %) (get-in h [:asserted-content :hou/sources]))
                  ;; Recompute against the current snapshot, so all pin entries
                  ;; (including reading records and the run input) are audited.
                  (= h (:written (hou/hou-run
                                  (snapshot cfg (assoc (turns/read-record (:turn-store cfg)
                                                       (get-in h [:asserted-content :hou/turn]))
                                                       :id (get-in h [:asserted-content :hou/turn])))))))
         {:relations (into {} (filter #(contains? (set targets) (first %)))
                           (get-in h [:asserted-content :hou/relations]))
          :evidence-pin (get-in h [:asserted-content :hou/evidence-pin])}))
     (catch Exception _ nil))))
