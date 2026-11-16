(ns futon3c.arxana.item-inventory
  "Current file census through the same readers as Giraffe. No stored slice.
  missions.clj:62-86 supplies M status; excursions.clj:13-80 uses the shared
  excursion/ticket readers and live predicates. run.clj:481-486 enables tickets
  for checked missions-excursions runs. Closed identities remain addressable."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon2.aif.mission-registry :as registry]
            [futon2.aif.task-requisition :as requisition]
            [futon3c.giraffe.missions :as missions]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http])
  (:import [java.time Instant]))

(defn item-record [id]
  (records/make-clusion {:type-revision [:workplan/item 1] :endpoints []
                        :asserted-content {:item/id id}}))

(defn- stamps [code-root repos]
  ;; Only the shallow directories the readers admit; include additions/deletions.
  (->> (concat (map io/file repos) (or (.listFiles (io/file code-root)) []))
       (mapcat #(map (fn [d] (io/file % d)) ["holes" "holes/missions" "holes/excursions" "holes/tickets"]))
       (mapcat #(or (.listFiles %) []))
       (filter #(and (.isFile %) (re-matches #"[MET]-.*\.md" (.getName %))))
       (map #(vector (.getAbsolutePath %) (.lastModified %) (.length %))) distinct sort vec))

(defonce ^:private cache (atom nil))

(defn current-items
  ([] (current-items {}))
  ([{:keys [code-root repos] :or {code-root registry/default-code-root repos missions/default-repos}}]
   (let [stamp [code-root repos (System/getenv "FUTON_WM_TICKET_REPOS") (stamps code-root repos)]]
     (if (= stamp (:stamp @cache)) (:items @cache)
       (let [ms (:missions (registry/load-missions-from-repos repos))
             es (remove requisition/requisition-record? (:excursions (registry/load-excursions code-root)))
             ts (:tickets (registry/load-tickets code-root))
             rows (for [[kind entries live?] [[:mission ms registry/live-mission?]
                                              [:excursion es registry/live-excursion?]
                                              [:ticket ts registry/live-ticket?]]
                        e entries
                        :let [live (live? e)
                              group (if live
                                      (if (and (= kind :mission) (= :started (missions/status->group (:status-line e))))
                                        :started :unstarted)
                                      (if (= :complete (:status-class e)) :completed :cancelled))]]
                    {:id (:id e) :kind kind :path (:path e) :source (:source e)
                     :status-line (:status-line e) :status-class (:status-class e)
                     :state-group group :state (if live :open :closed)})
             items (vec (sort-by :id rows))]
         (when-not (= (count items) (count (distinct (map :id items))))
           (throw (ex-info "census has duplicate ids" {})))
         (reset! cache {:stamp stamp :items items}) items)))))

(defn missing-items [graph-store items]
  (filterv (fn [{:keys [id]}]
             (let [r (item-record id) answer (store/get-entity graph-store (:id r))]
               (cond (= r (:ok answer)) false
                     (= :missing (get-in answer [:error :error/category])) true
                     :else (throw (ex-info "item inventory read refused" {:item id :result answer}))))) items))

(defn- git! [root & args]
  (let [r (apply shell/sh "git" "-C" root args)]
    (when-not (zero? (:exit r)) (throw (ex-info "git provenance unavailable" {:root root :args args})))
    (str/trim (:out r))))

(defn- source-root [path]
  (first (str/split (str path) #"/holes/")))

(defn- batch-message-id [prefix rows]
  (str prefix
       (digest/sha256-hex
        (.getBytes (str/join "," (map #(get-in % [:record :id]) rows)) "UTF-8"))))

(defn- roots-and-commits [items]
  (into {} (for [root (distinct (map #(source-root (:path %)) items))
                 :when (seq root)]
             [root (git! root "rev-parse" "HEAD")])))

(defn- superseded? [item]
  (boolean (re-find #"(?i)^\s*SUPERSEDED\b" (or (:status-line item) ""))))

(defn- declared-successors
  "The sole mission-prose parser for supersession. GCC readers never open an
  item file."
  [item]
  (->> (re-seq #"(?i)superseded by\s+`?([MET]-[A-Za-z0-9_-]+)"
               (slurp (:path item)))
       (map second) distinct vec))

(defn supersession-plan
  "Plan exact [:supersedes 1] claims for CHANGED-ITEMS against INVENTORY."
  [inventory changed-items]
  (let [by-id (into {} (map (juxt :id identity)) inventory)
        candidates (filter superseded? changed-items)
        commits (roots-and-commits candidates)
        planned
        (mapv
         (fn [{:keys [id path source] :as item}]
           (let [successors (declared-successors item)]
             (cond
               (empty? successors) {:reason :no-successor :item id}
               (> (count successors) 1) {:reason :ambiguous :item id}
               (nil? (by-id (first successors)))
               {:reason :unknown-successor :item id :successor (first successors)}
               :else
               (let [successor (first successors)
                     claim (records/make-clusion
                            {:type-revision [:supersedes 1]
                             :endpoints [{:role :successor :kind :clusion
                                          :target {:clusion/id (:id (item-record successor))}}
                                         {:role :predecessor :kind :clusion
                                          :target {:clusion/id (:id (item-record id))}}]
                             :asserted-content {}})
                     root (source-root path)
                     commit (commits root)
                     sha (:sha256 source)
                     written-at (str (Instant/ofEpochMilli (.lastModified (io/file path))))
                     att (records/make-attestation
                          {:subject-clusion-id (:id claim) :basis :exact
                           :standing :verified :coverage :complete
                           :producer "futon3c/item-inventory"
                           :method {:name :supersession-census :version 1}
                           :content {:path path :git/commit commit :file/sha256 sha
                                     :written-at written-at}
                           :source-refs [{:path path :git/commit commit :sha256 sha}]})]
                 {:item id :successor successor
                  :rows [{:operation :clusion/assert :record claim}
                         {:operation :evidence/append :record att}]}))))
         candidates)
        rows (vec (mapcat :rows planned))
        counts (frequencies (keep :reason planned))]
    {:rows rows
     :written (count (filter :rows planned))
     :no-successor (get counts :no-successor 0)
     :ambiguous (get counts :ambiguous 0)
     :unknown-successor (get counts :unknown-successor 0)
     :message-id (when (seq rows)
                   (batch-message-id "item-supersession/inventory/" rows))}))

(defn publication-plan [items]
  (let [roots (roots-and-commits items)
        rows (mapcat
              (fn [{:keys [id path source]}]
                (let [r (item-record id) root (source-root path)
                      a (records/make-attestation
                         {:subject-clusion-id (:id r) :basis :exact :standing :verified :coverage :complete
                          :producer "futon3c/item-inventory" :method {:name :file-census :version 1}
                          :source-refs [{:path path :git/commit (roots root) :sha256 (:sha256 source)
                                         :worktree-dirty? (not (str/blank? (git! root "status" "--porcelain" "--" path)))}]})]
                  [{:operation :clusion/assert :record r} {:operation :evidence/append :record a}])) items)]
    (mapv (fn [rs] {:message-id (str "item-inventory/" (digest/canonical-digest rs)) :records (vec rs)})
          (partition-all 500 rows))))

(defn sync-inventory!
  "Dry-run is the default; it prints precisely the batches --write will send."
  [{:keys [graph-store write?] :as cfg}]
  (let [items (current-items cfg) missing (missing-items graph-store items)
        plan (publication-plan missing)
        supersession (supersession-plan items items)]
    (when write?
      (doseq [{:keys [message-id records]} plan]
        (let [r (http/append-batch! graph-store message-id records)]
          (when-not (:ok r) (throw (ex-info "inventory publication refused" {:result r})))))
      (when-let [message-id (:message-id supersession)]
        (let [r (http/append-batch! graph-store message-id (:rows supersession))]
          (when-not (:ok r)
            (throw (ex-info "supersession publication refused" {:result r}))))))
    {:items (count items) :missing (mapv :id missing) :batches plan
     :supersession (dissoc supersession :rows :message-id)
     :supersession-batch (select-keys supersession [:message-id :rows])
     :write? (boolean write?)}))

(defn sync-items!
  "Check only ITEMS and publish all missing records plus exact attestations in
  one atomic batch. Intended for a caller that remembers confirmed IDs."
  [{:keys [graph-store inventory-items inventory-items-fn] :as cfg} items]
  (let [inventory (or inventory-items
                      (when inventory-items-fn (inventory-items-fn cfg))
                      items)
        missing (missing-items graph-store items)
        plan (publication-plan missing)
        supersession (supersession-plan inventory items)]
    (when (> (count plan) 1)
      (throw (ex-info "incremental inventory exceeds one batch"
                      {:items (mapv :id missing) :batches (count plan)})))
    (let [rows (into (vec (mapcat :records plan)) (:rows supersession))
          message-id (when (seq rows)
                       (batch-message-id "item-inventory-sync/inventory/" rows))]
      (when (> (count rows) 500)
        (throw (ex-info "incremental inventory exceeds one batch"
                        {:records (count rows)})))
      (when message-id
        (let [r (http/append-batch! graph-store message-id rows)]
          (when-not (:ok r)
            (throw (ex-info "inventory publication refused" {:result r}))))))
    {:checked (mapv :id items) :written (mapv :id missing)
     :batch-count (if (or (seq plan) (:message-id supersession)) 1 0)
     :supersession (dissoc supersession :rows :message-id)}))

(defn -main [& args]
  (when-not (every? #{"--dry-run" "--write"} args)
    (throw (ex-info "usage: --dry-run (default) | --write" {})))
  (when (= #{"--dry-run" "--write"} (set args))
    (throw (ex-info "choose dry-run or write" {})))
  (try (prn (sync-inventory! {:graph-store (http/http-store "http://localhost:7073" "arxana-service")
                         :write? (boolean (some #{"--write"} args))}))
       (finally (shutdown-agents))))
