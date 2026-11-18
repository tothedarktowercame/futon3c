(ns futon3c.arxana.giraffe-needs
  "Project accepted Giraffe reading edges into [:needs 1] and read them back."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.item-inventory :as inventory]
            [futon3c.arxana.pattern-proposals :as proposals]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.giraffe.run-data :as run-data])
  (:import [java.util.concurrent Callable ExecutorService Executors ThreadFactory]))

(defn- sha-file [path]
  (digest/sha256-hex (java.nio.file.Files/readAllBytes (.toPath (io/file path)))))
(defn- id-text [x] (if (keyword? x) (name x) (str x)))
(defn- require! [ok reason]
  (when-not ok (throw (ex-info "Giraffe needs evidence refused" {:reason reason}))))

(defn default-config []
  (merge (mentions/default-config)
         {:party-path (str (System/getProperty "user.home") "/storage/gcc-preview/party.json")
          :runs-dir (str (System/getProperty "user.home") "/storage/giraffe")
          :pause-ms 200}))

(defn party-run
  "The Giraffe run the party (Mojira's GCC pages) is built from."
  [cfg]
  (let [run (get-in (json/parse-string (slurp (:party-path cfg)) true) [:sources :giraffe_run])]
    (require! (and (string? run) (= run (.getName (io/file run)))) :invalidEvidence)
    run))

(defn current-run
  "The run to write: :giraffe-run when given (the reread service writes a
  freshly minted run before the party is repointed at it), else the party's."
  [cfg]
  (let [run (or (:giraffe-run cfg) (party-run cfg))
        _ (require! (and (string? run) (= run (.getName (io/file run)))) :invalidEvidence)
        dir (io/file (:runs-dir cfg) run)
        manifest (edn/read-string (slurp (io/file dir "manifest.edn")))
        input-path (str (io/file dir "input.edn"))
        input-sha (sha-file input-path)
        _ (require! (= input-sha (:sha256 (first (filter #(= "input.edn" (:file %))
                                                         (:files manifest)))))
                    :invalidEvidence)
        input (run-data/read-run-file dir "input.edn")
        members (into {} (map (fn [m] [(id-text (:id m)) m]))
                      (get-in input [:graph-coverage :members]))]
    {:run run :manifest manifest :input input :input-path input-path
     :input-sha input-sha :members members}))

(declare run-attestations)

(defn plan [cfg]
  (let [{:keys [run manifest input input-path input-sha members]} (current-run cfg)
        item-set (set (:item-ids cfg))
        generation (get-in manifest [:readings-generation :path])
        accepted (remove #(= :alternative (:class %)) (:edges input))
        grouped (sort-by key (group-by (comp id-text :reading-member) accepted))
        seen (atom {})
        existing? (fn [claim member pin]
                    (let [k [(:id claim) member (:sha256 pin)]
                          found (if (contains? @seen k)
                                  (get @seen k)
                                  (let [atts (run-attestations cfg claim {member pin})]
                                    (swap! seen assoc k atts)
                                    atts))]
                      (seq found)))]
    {:run run :input-sha input-sha
     :readings
     (mapv
      (fn [[member edges]]
        (let [pin (:record-pin (get members member))
              batch (str (io/file generation (.getName (io/file (:batch pin)))))
              line (nth (str/split (slurp batch) #"\n") (dec (:line pin)))
              _ (require! (= (:sha256 pin)
                             (digest/sha256-hex (.getBytes line "UTF-8"))) :evidenceChanged)
              written-at (or (:written-at cfg) (str (java.time.Instant/now)))
              planned (mapv
                       (fn [edge]
                         (let [from (id-text (:from edge)) to (id-text (:to edge))]
                           (if (and (item-set from) (item-set to))
                             (let [from-ref (inventory/item-record from)
                                   to-ref (inventory/item-record to)
                                   claim (records/make-clusion
                                          {:type-revision [:needs 1]
                                           :endpoints [{:role :needing :kind :external-ref
                                                        :target {:ref/id (:id from-ref)}}
                                                       {:role :supplier :kind :external-ref
                                                        :target {:ref/id (:id to-ref)}}]
                                           :asserted-content {:class (:class edge)}})
                                   att (records/make-attestation
                                        {:subject-clusion-id (:id claim) :producer "Giraffe"
                                         :basis :model :standing :unverified :coverage :complete
                                         :method {:id "giraffe/reading" :version 1}
                                         :content {:reading-member member
                                                   :record-pin (assoc pin :batch batch)
                                                   :run run :input-path input-path
                                                   :input-sha256 input-sha :written-at written-at}
                                         :source-refs [{:path batch :line (:line pin)
                                                        :sha256 (:sha256 pin)}]})]
                               (if (and (:graph-store cfg) (existing? claim member pin))
                                 {:unchanged [from to]}
                                 {:edge edge
                                  :rows [{:operation :clusion/assert :record claim}
                                         {:operation :evidence/append :record att}]}))
                             {:skipped [from to]})))
                       edges)
              rows (vec (mapcat :rows planned))]
          {:member member :rows rows
           :written (count (filter :rows planned))
           :unchanged (count (filter :unchanged planned))
           :skipped (mapv :skipped (filter :skipped planned))
           :message-id (when (seq rows)
                         (proposals/batch-message-id
                          "giraffe-needs/" {:id (str run "/" member)} rows))}))
      grouped)}))

(defn write! [cfg]
  (let [p (plan cfg)
        results (mapv
                 (fn [{:keys [rows message-id written unchanged skipped member]}]
                   (if (empty? rows)
                     {:member member :written 0 :unchanged unchanged :skipped skipped}
                     (let [r (http/append-batch! (:graph-store cfg) message-id rows)]
                       (when (pos? (or (:pause-ms cfg) 0))
                         (Thread/sleep (long (:pause-ms cfg))))
                       (if (:ok r) {:member member :written written :unchanged unchanged
                                    :skipped skipped}
                           {:member member :failed :needs-write-refused
                            :unchanged unchanged :skipped skipped}))))
                 (:readings p))]
    {:run (:run p) :readings results
     :written (reduce + (map #(or (:written %) 0) results))
     :unchanged (reduce + (map #(or (:unchanged %) 0) results))
     :skipped (reduce + (map #(count (:skipped %)) results))}))

(defn- run-attestations
  "CLAIM's attestations matching MEMBER-PINS, latest per reading member. Two readings
  can assert the same edge: one clusion, one attestation each, and each
  reading keeps it (its incident items depend on it)."
  [cfg claim member-pins]
  (let [q (loop [tries 0]
            ;; A lagging index answers :stale-index with no ids. Read as "no
            ;; attestations" it silently dropped readings from the projection
            ;; (CL-46: 70 or 85 of 92), and priority bands flipped.
            ;; The attestations query caps at 200 and does not flag
            ;; truncation; every run appends one attestation per reading per
            ;; claim, so a full page means the party run's may be cut off
            ;; (seen live: 58 on one claim, limit 50 dropped a reading).
            (let [limit 200
                  r (store/bounded-query! (:graph-store cfg)
                                          {:query/type :attestations
                                           :subject-id (:id claim) :limit limit})
                  status (get-in r [:ok :status])]
              (if (and (= :stale-index status) (< tries 3))
                (do (Thread/sleep 50) (recur (inc tries)))
                (do (require! (and (nil? (:error r)) (= :ok status)
                                   (not (get-in r [:ok :truncated?]))
                                   (< (count (get-in r [:ok :attestation-ids])) limit))
                              :needs-unavailable)
                    (:ok r)))))]
    (->> (:attestation-ids q)
         (keep #(-> (store/get-entity (:graph-store cfg) %) :ok))
         (filter (fn [att]
                   (let [member (get-in att [:content :reading-member])
                         expected (get member-pins member)]
                     (or (nil? member-pins)
                         (and expected
                              (= (:sha256 expected)
                                 (get-in att [:content :record-pin :sha256])))))))
         (group-by #(get-in % [:content :reading-member]))
         vals
         (map #(last (sort-by (fn [a] (str (get-in a [:content :written-at]))) %))))))

(defn- query-needing [cfg item-id]
  (let [ref-id (:id (inventory/item-record item-id))
        q (:ok (store/bounded-query! (:graph-store cfg)
                                     {:query/type :role-reverse :edge-type [:needs 1]
                                      :endpoint-role :needing :endpoint-id ref-id
                                      :limit 200 :order :asc}))]
    (require! (and (= :ok (:status q)) (not (:truncated? q))) :invalidEvidence)
    (:edge-ids q)))

(defonce ^:private ^ExecutorService read-executor
  (Executors/newFixedThreadPool
   20
   (reify ThreadFactory
     (newThread [_ r] (doto (Thread. ^Runnable r "giraffe-needs-read")
                        (.setDaemon true))))))

(defn expected-members
  "Reading members RUN's needs writer publishes: accepted edges whose two
  endpoints are in the writer's item set (plan skips the rest). This is the
  completeness census for a needs read of RUN; a member the writer skips
  (e.g. an edge to an item no longer in the inventory) is not expected."
  [cfg run item-ids]
  (let [input (run-data/read-run-file (io/file (:runs-dir cfg) run) "input.edn")
        item-set (set item-ids)]
    (->> (:edges input)
         (remove #(= :alternative (:class %)))
         (filter #(and (item-set (id-text (:from %))) (item-set (id-text (:to %)))))
         (map (comp id-text :reading-member))
         distinct sort vec)))

(defn- census
  "Expected members of RUN for a read that observed OBSERVED. The writer's
  item set is the verified inventory (mentions/inventory: 30 s for the whole
  census), so start from the file census (35 ms) and verify graph records
  only for the endpoints of members that look missing."
  [cfg run observed]
  (let [ids (or (:writer-item-ids cfg) (mapv :id (inventory/current-items cfg)))
        candidate (expected-members cfg run ids)
        missing (remove (set observed) candidate)]
    (if (or (empty? missing) (:writer-item-ids cfg))
      candidate
      (let [input (run-data/read-run-file (io/file (:runs-dir cfg) run) "input.edn")
            missing-set (set missing)
            endpoints (->> (:edges input)
                           (filter #(missing-set (id-text (:reading-member %))))
                           (mapcat (juxt (comp id-text :from) (comp id-text :to)))
                           distinct)
            verified (set (mentions/inventory (dissoc cfg :item-ids) (str/join " " endpoints)))
            unverified (set (remove verified endpoints))]
        (expected-members cfg run (remove unverified ids))))))

(defn- read-needs-gcc* [cfg run]
  (let [run-context (when (:runs-dir cfg) (current-run (assoc cfg :giraffe-run run)))
        member-pins (or (:member-pins cfg)
                        (when run-context
                          (into {} (map (fn [[member row]] [member (:record-pin row)]))
                                (:members run-context))))
        items (vec (:item-ids cfg))
        tasks (mapv #(.submit read-executor ^Callable (fn [] (query-needing cfg %))) items)
        ids (->> tasks (mapcat #(.get %)) distinct vec)]
    (when (empty? ids)
      (throw (ex-info "GCC needs unavailable" {:reason :needs-unavailable})))
    (let [item-by-ref (into {} (map (fn [id] [(:id (inventory/item-record id)) id])) items)
          claims (keep #(-> (store/get-entity (:graph-store cfg) %) :ok) ids)
          ;; A run selects the attestation matching each reading's current
          ;; record pin. The attestation may have been written by an earlier
          ;; run when the reading line is unchanged.
          rows (mapcat (fn [claim]
                         (let [from (item-by-ref (get-in claim [:endpoints 0 :target :ref/id]))
                               to (item-by-ref (get-in claim [:endpoints 1 :target :ref/id]))]
                           (when (and from to)
                             (for [att (run-attestations cfg claim member-pins)]
                               {:claim claim :att att :from from :to to})))) claims)
          _ (when (empty? rows)
              (throw (ex-info "GCC needs unavailable for the party's run"
                              {:reason :needs-unavailable :run run})))
          by-member (sort-by key (group-by #(get-in % [:att :content :reading-member]) rows))
          sources (atom [])
          readings
          (mapv
           (fn [[member rs]]
             (let [latest-stamp (last (sort (map #(str (get-in % [:att :content :written-at])) rs)))
                   rs (filterv #(= latest-stamp (str (get-in % [:att :content :written-at]))) rs)
                   att (:att (first rs))
                   pin (get-in att [:content :record-pin])
                   batch (:batch pin)
                   line (nth (str/split (slurp batch) #"\n") (dec (:line pin)))
                   _ (require! (= (:sha256 pin)
                                  (digest/sha256-hex (.getBytes line "UTF-8"))) :evidenceChanged)
                   raw (json/parse-string line true)
                   pins (cons (:source raw) (keep :evidence (:edges raw)))
                   incident (vec (distinct (mapcat (fn [e] [member (id-text (:to e))])
                                                   (remove #(= "alternative"
                                                               (or (:intended_class %)
                                                                   (:intended-class %)))
                                                           (:edges raw)))))]
               (doseq [p pins]
                 (require! (and (string? (:path p)) (string? (:sha256 p))) :invalidEvidence)
                 (let [current (sha-file (:path p))]
                   (swap! sources conj
                          (cond-> {:kind :file :path (:path p) :sha256 (:sha256 p)}
                            (not= current (:sha256 p))
                            (assoc :stale-source (:path p) :read-sha (:sha256 p)
                                   :current-sha current)))))
               (swap! sources conj {:kind :file :path batch :sha256 (sha-file batch)})
               {:record {:id (str "giraffe-reading/" member)
                         :reading raw :line-sha256 (:sha256 pin)}
                :needs (mapv (juxt :from :to) rs)
                :incident-items incident}))
           by-member)
          first-att (:att (first rows))
          input-sha (or (:input-sha run-context) (get-in first-att [:content :input-sha256]))
          input-path (or (:input-path run-context) (get-in first-att [:content :input-path]))]
      {:readings readings
       ;; nil without :runs-dir (unit fixtures); every production config
       ;; (default-config, hou-store) sets it.
       :expected-members (when (:runs-dir cfg) (census cfg run (map key by-member)))
       :basis-records [{:id (str "giraffe-input/" run) :sha256 input-sha}]
       :sources (vec (distinct (conj @sources {:kind :file :path input-path
                                               :sha256 input-sha})))})))

(defonce ^:private read-cache (atom {}))

(defn read-needs-gcc
  "Read the current needs projection. Successful production reads are cached
  by store and inventory census. Typed unavailability is cached for five
  seconds only, so repeated per-turn reads stay cheap without hiding backfill."
  [cfg]
  (let [run (party-run cfg)
        ;; Key by the store's identity, not the HttpStore object: every
        ;; default-config builds a new HttpClient, so object equality missed
        ;; the cache on each call (CL-55: 45.9 s cold, 2 ms on a hit).
        store (:graph-store cfg)
        store-key (if (and (map? store) (contains? store :base-url))
                    [(:base-url store) (:principal store)]
                    store)
        k [store-key (hash (vec (:item-ids cfg))) run]
        now (System/currentTimeMillis)
        cached (get @read-cache k)]
    (cond
      (and (:cache-needs? cfg) (:result cached)) (:result cached)
      (and (:cache-needs? cfg) (:error cached) (< (- now (:at cached)) 5000))
      (throw (:error cached))
      :else
      (try
        (let [result (read-needs-gcc* cfg run)]
          ;; One party run at a time: drop entries for runs the party has
          ;; moved past, so the cache does not grow with every mint.
          (when (:cache-needs? cfg)
            (swap! read-cache (fn [m] (assoc (into {} (filter #(= run (peek (key %)))) m)
                                             k {:result result :at now}))))
          result)
        (catch clojure.lang.ExceptionInfo e
          (when (and (:cache-needs? cfg) (= :needs-unavailable (:reason (ex-data e))))
            (swap! read-cache assoc k {:error e :at now}))
          (throw e))))))

(defn write-run!
  "Write RUN's needs (the reread service calls this for each minted run,
  before the party is repointed at it). Never throws."
  ([run] (write-run! (assoc (default-config) :pause-ms 50) run))
  ([cfg run]
   (try (write! (assoc cfg :giraffe-run run :item-ids (mentions/inventory cfg nil)))
        (catch Exception e {:run run :failed (or (:reason (ex-data e)) :needs-write-exception)
                            :detail (.getMessage e)}))))

(defn backfill! [cfg {:keys [write?] :or {write? false}}]
  (let [p (plan cfg)
        eligible (filterv #(seq (:rows %)) (:readings p))]
    (if write?
      (write! cfg)
      {:mode :dry-run :run (:run p) :readings (count (:readings p))
       :outcomes {:would-write (count eligible)
                  :empty (- (count (:readings p)) (count eligible))}
       :edges (reduce + (map :written (:readings p)))
       :skipped (reduce + (map #(count (:skipped %)) (:readings p)))})))

(defn -main [& args]
  (when-not (every? #{"--dry-run" "--write"} args)
    (throw (ex-info "usage: --dry-run (default) | --write" {})))
  (when (= #{"--dry-run" "--write"} (set args))
    (throw (ex-info "choose dry-run or write" {})))
  (let [base (default-config)
        cfg (assoc base :item-ids (mentions/inventory base nil))]
    (try (prn (backfill! cfg {:write? (boolean (some #{"--write"} args))}))
         (finally (shutdown-agents)))))
