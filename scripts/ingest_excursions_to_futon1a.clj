#!/usr/bin/env -S clojure -M
;; One-off substrate-2 bootstrap for `code/v05/excursion-doc` vertices and
;; `code/v05/excursion→parent-mission` edges.
;;
;; Canonical path (T-A2):
;;   The enduring source of truth is the watcher-integrated excursion ingest
;;   path in `futon3c.watcher.file-ingest/ingest-excursion-doc!`, triggered by
;;   writes to `holes/missions/E-*.md`.
;;
;; Takeover proof (T-A2):
;;   The watcher has taken over once re-ingesting an excursion doc emits the
;;   expected excursion vertex and parent edge without rerunning this script.
;;
;; Invalidation moment (T-A2):
;;   After that watcher proof, this script is no longer canonical. Any later
;;   need to rerun it is a VERIFY failure to investigate, not routine upkeep.
;;
;; Usage:
;;   clojure scripts/ingest_excursions_to_futon1a.clj --dry-run
;;   clojure scripts/ingest_excursions_to_futon1a.clj --write
;;
;; Default mode is dry-run. `--write` performs live POSTs and should only be
;; used with operator approval.

(require '[clojure.java.io :as io]
         '[clojure.pprint :refer [pprint]]
         '[clojure.string :as str]
         '[futon3c.peripheral.mission-control-backend :as mcb]
         '[futon3c.watcher.file-ingest :as ingest])

(def ^:private home
  (System/getProperty "user.home"))

(def ^:private repo-specs
  [{:label "futon0-d" :root (str home "/code/futon0")}
   {:label "futon1-d" :root (str home "/code/futon1")}
   {:label "futon1a-d" :root (str home "/code/futon1a")}
   {:label "futon2-d" :root (str home "/code/futon2")}
   {:label "futon3-d" :root (str home "/code/futon3")}
   {:label "futon3a-d" :root (str home "/code/futon3a")}
   {:label "futon3b-d" :root (str home "/code/futon3b")}
   {:label "futon3c-d" :root (str home "/code/futon3c")}
   {:label "futon4-d" :root (str home "/code/futon4")}
   {:label "futon5-d" :root (str home "/code/futon5")}
   {:label "futon5a-d" :root (str home "/code/futon5a")}
   {:label "futon6-d" :root (str home "/code/futon6")}
   {:label "futon7-d" :root (str home "/code/futon7")}
   {:label "futon7a-d" :root (str home "/code/futon7a")}
   {:label "vsat-d" :root (str home "/vsat")}
   {:label "vsat.wiki-d" :root (str home "/vsat.wiki")}
   {:label "npt-d" :root (str home "/npt")}])

(defn- parse-args
  [argv]
  (loop [args argv
         opts {:mode :dry-run}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (case arg
          "--dry-run" (recur (next args) (assoc opts :mode :dry-run))
          "--write" (recur (next args) (assoc opts :mode :write))
          (throw (ex-info "unknown arg" {:arg arg :argv argv})))))))

(defn- label->repo-keyword
  [label]
  (-> label
      (str/replace #"-d$" "")
      keyword))

(defn- excursion-paths
  [root]
  (let [missions-dir (io/file root "holes" "missions")]
    (->> (if (.isDirectory missions-dir)
           (file-seq missions-dir)
           [])
         (filter #(.isFile ^java.io.File %))
         (map #(.getAbsolutePath ^java.io.File %))
         (filter #(re-find #"/E-[^/]+\.md$" (str/replace % "\\" "/")))
         sort
         vec)))

(defn- excursion-plan
  [{:keys [label]} path]
  (let [excursion (mcb/parse-excursion-md path (label->repo-keyword label))]
    (merge {:path path
            :label label
            :excursion excursion}
           (ingest/build-excursion-docs {:path path
                                         :label label
                                         :excursion excursion}))))

(defn- mapvcat
  [f coll]
  (vec (mapcat f coll)))

(defn- collect-plans
  []
  (mapvcat (fn [repo]
             (mapv #(excursion-plan repo %) (excursion-paths (:root repo))))
           repo-specs))

(defn- dry-run-report
  []
  (let [plans (collect-plans)
        sample (first plans)]
    {:mode :dry-run
     :excursion-count (count plans)
     :parent-edge-count (reduce + 0 (map #(count (:edge-docs %)) plans))
     :skipped-parent-edge-count (count (filter :skipped-parent-edge? plans))
     :sample (when sample
               (select-keys (:vertex-doc sample) [:hx-type :endpoints :props]))}))

(defn- write-report
  []
  (let [plans (collect-plans)
        vertex-stats (reduce (fn [acc {:keys [vertex-doc]}]
                               (let [resp (ingest/post-hyperedge! (:hx-type vertex-doc)
                                                                  (:endpoints vertex-doc)
                                                                  (:labels vertex-doc)
                                                                  (:props vertex-doc))]
                                 (update acc (if (:ok? resp) :emitted :failed) inc)))
                             {:emitted 0 :failed 0}
                             plans)
        entity-stats (reduce (fn [acc {:keys [vertex-doc excursion]}]
                               (let [resp (ingest/post-entity!
                                           {:name (first (:endpoints vertex-doc))
                                            :type "excursion/doc"
                                            :source "excursion-doc-backfill"
                                            :external-id (str "E-" (:excursion/id excursion))
                                            :props {"excursion/id" (:excursion/id excursion)
                                                    "excursion/title" (:excursion/title excursion)
                                                    "excursion/status" (when-let [s (:excursion/status excursion)]
                                                                         (name s))
                                                    "excursion/parent-mission" (:excursion/parent-mission excursion)}})]
                                 (update acc (if (:ok? resp) :emitted :failed) inc)))
                             {:emitted 0 :failed 0}
                             plans)
        edge-docs (vec (mapcat :edge-docs plans))
        edge-stats (reduce (fn [acc doc]
                             (let [resp (ingest/post-hyperedge-doc! doc)]
                               (update acc (if (:ok? resp) :emitted :failed) inc)))
                           {:emitted 0 :failed 0}
                           edge-docs)]
    {:mode :write
     :excursion-count (count plans)
     :vertex-emitted (:emitted vertex-stats)
     :vertex-failed (:failed vertex-stats)
     :entity-emitted (:emitted entity-stats)
     :entity-failed (:failed entity-stats)
     :parent-edge-count (:emitted edge-stats)
     :parent-edge-failed (:failed edge-stats)
     :skipped-parent-edge-count (count (filter :skipped-parent-edge? plans))
     :sample (when-let [sample (first plans)]
               (select-keys (:props (:vertex-doc sample))
                            ["excursion/id" "excursion/title" "excursion/status" "excursion/parent-mission"]))}))

(defn -main
  [& argv]
  (let [{:keys [mode]} (parse-args argv)
        report (if (= mode :write)
                 (write-report)
                 (dry-run-report))]
    (pprint report)
    (when (and (= mode :write)
               (or (pos? (:vertex-failed report 0))
                   (pos? (:entity-failed report 0))
                   (pos? (:parent-edge-failed report 0))))
      (throw (ex-info "excursion backfill had failures" report)))))

(apply -main *command-line-args*)
