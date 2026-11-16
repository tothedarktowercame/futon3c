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
            [futon3c.arxana.turn-mention-writer :as mentions])
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
        input (edn/read-string (slurp input-path))
        members (into {} (map (fn [m] [(id-text (:id m)) m]))
                      (get-in input [:graph-coverage :members]))]
    {:run run :manifest manifest :input input :input-path input-path
     :input-sha input-sha :members members}))

(defn plan [cfg]
  (let [{:keys [run manifest input input-path input-sha members]} (current-run cfg)
        item-set (set (:item-ids cfg))
        generation (get-in manifest [:readings-generation :path])
        accepted (remove #(= :alternative (:class %)) (:edges input))
        grouped (sort-by key (group-by (comp id-text :reading-member) accepted))]
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
                               {:edge edge
                                :rows [{:operation :clusion/assert :record claim}
                                       {:operation :evidence/append :record att}]})
                             {:skipped [from to]})))
                       edges)
              rows (vec (mapcat :rows planned))]
          {:member member :rows rows
           :written (count (filter :rows planned))
           :skipped (mapv :skipped (filter :skipped planned))
           :message-id (when (seq rows)
                         (proposals/batch-message-id
                          "giraffe-needs/" {:id (str run "/" member)} rows))}))
      grouped)}))

(defn write! [cfg]
  (let [p (plan cfg)
        results (mapv
                 (fn [{:keys [rows message-id written skipped member]}]
                   (if (empty? rows)
                     {:member member :written 0 :skipped skipped}
                     (let [r (http/append-batch! (:graph-store cfg) message-id rows)]
                       (when (pos? (or (:pause-ms cfg) 0))
                         (Thread/sleep (long (:pause-ms cfg))))
                       (if (:ok r) {:member member :written written :skipped skipped}
                           {:member member :failed :needs-write-refused :skipped skipped}))))
                 (:readings p))]
    {:run (:run p) :readings results
     :written (reduce + (map #(or (:written %) 0) results))
     :skipped (reduce + (map #(count (:skipped %)) results))}))

(defn- run-attestations
  "CLAIM's attestations from RUN, the latest per reading member. Two readings
  can assert the same edge: one clusion, one attestation each, and each
  reading keeps it (its incident items depend on it)."
  [cfg claim run]
  (let [q (:ok (store/bounded-query! (:graph-store cfg)
                                      {:query/type :attestations
                                       :subject-id (:id claim) :limit 50}))]
    (->> (:attestation-ids q)
         (keep #(-> (store/get-entity (:graph-store cfg) %) :ok))
         (filter #(= run (get-in % [:content :run])))
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

(defn- read-needs-gcc* [cfg run]
  (let [items (vec (:item-ids cfg))
        tasks (mapv #(.submit read-executor ^Callable (fn [] (query-needing cfg %))) items)
        ids (->> tasks (mapcat #(.get %)) distinct vec)]
    (when (empty? ids)
      (throw (ex-info "GCC needs unavailable" {:reason :needs-unavailable})))
    (let [item-by-ref (into {} (map (fn [id] [(:id (inventory/item-record id)) id])) items)
          claims (keep #(-> (store/get-entity (:graph-store cfg) %) :ok) ids)
          ;; Giraffe mints a new run every few minutes; only attestations
          ;; from the party's run are edges of the readings it shows.
          rows (mapcat (fn [claim]
                         (let [from (item-by-ref (get-in claim [:endpoints 0 :target :ref/id]))
                               to (item-by-ref (get-in claim [:endpoints 1 :target :ref/id]))]
                           (when (and from to)
                             (for [att (run-attestations cfg claim run)]
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
          run (get-in first-att [:content :run])
          input-sha (get-in first-att [:content :input-sha256])
          input-path (get-in first-att [:content :input-path])]
      {:readings readings
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
        k [(:graph-store cfg) (hash (vec (:item-ids cfg))) run]
        now (System/currentTimeMillis)
        cached (get @read-cache k)]
    (cond
      (and (:cache-needs? cfg) (:result cached)) (:result cached)
      (and (:cache-needs? cfg) (:error cached) (< (- now (:at cached)) 5000))
      (throw (:error cached))
      :else
      (try
        (let [result (read-needs-gcc* cfg run)]
          (when (:cache-needs? cfg) (swap! read-cache assoc k {:result result :at now}))
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
