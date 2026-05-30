#!/usr/bin/env -S clojure -M
;; One-off substrate-2 bootstrap for `code/v05/file→mission` edges.
;;
;; Canonical path (T-A2):
;;   The enduring source of truth is the watcher-integrated mission-doc ingest
;;   path in `futon3c.watcher.file-ingest/ingest-mission-doc!`, which projects
;;   `:mission/code-paths` into `code/v05/file→mission` edges.
;;
;; Takeover proof (T-A2):
;;   The watcher has taken over once re-ingesting a mission doc emits the
;;   expected `code/v05/file→mission` edges without rerunning this script.
;;
;; Invalidation moment (T-A2):
;;   After that watcher proof, this script is no longer canonical. Any later
;;   need to rerun it is a VERIFY failure to investigate, not routine upkeep.
;;
;; Usage:
;;   clojure scripts/ingest_mission_code_paths_to_futon1a.clj --dry-run
;;   clojure scripts/ingest_mission_code_paths_to_futon1a.clj --write
;;
;; Default mode is dry-run. `--write` performs live POSTs and should only be
;; used with operator approval.

(require '[babashka.http-client :as http]
         '[clojure.edn :as edn]
         '[clojure.pprint :refer [pprint]]
         '[futon3c.watcher.file-ingest :as ingest])

(def ^:private futon1a
  (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))

(def ^:private penholder
  (or (System/getenv "FUTON1A_PENHOLDER") "api"))

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
         opts {:mode :dry-run
               :limit 250}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (case arg
          "--dry-run" (recur (next args) (assoc opts :mode :dry-run))
          "--write" (recur (next args) (assoc opts :mode :write))
          "--limit" (recur (nnext args) (assoc opts :limit (Long/parseLong (second args))))
          (throw (ex-info "unknown arg"
                          {:arg arg
                           :argv argv})))))))

(defn- fetch-mission-docs
  [limit]
  (reduce (fn [acc {:keys [label root]}]
            (let [mission-dir (java.io.File. (str root "/holes/missions"))
                  paths (->> (if (.isDirectory mission-dir)
                               (file-seq mission-dir)
                               [])
                             (filter #(.isFile ^java.io.File %))
                             (map #(.getAbsolutePath ^java.io.File %))
                             (filter #(re-find #"/M-[^/]+\.md$" %))
                             sort
                             (take limit))]
              (reduce (fn [repo-acc source-file]
                        (let [url (str futon1a "/api/alpha/hyperedges?type="
                                       (java.net.URLEncoder/encode "code/v05/mission-doc" "UTF-8")
                                       "&repo="
                                       (java.net.URLEncoder/encode label "UTF-8")
                                       "&source-file="
                                       (java.net.URLEncoder/encode source-file "UTF-8"))
                              resp (http/get url {:headers {"Accept" "application/edn"
                                                            "X-Penholder" penholder}
                                                  :throw false
                                                  :timeout 10000})]
                          (if-not (= 200 (:status resp))
                            (update repo-acc :unreadable conj
                                    {:repo label
                                     :source-file source-file
                                     :reason (str "http-status-" (:status resp))})
                            (try
                              (update repo-acc :mission-docs into
                                      (vec (or (:hyperedges (edn/read-string (:body resp))) [])))
                              (catch Exception e
                                (update repo-acc :unreadable conj
                                        {:repo label
                                         :source-file source-file
                                         :reason (.getMessage e)}))))))
                      acc
                      paths)))
          {:mission-docs [] :unreadable []}
          repo-specs))

(defn- prop-get
  [props k]
  (or (get props k)
      (get props (keyword k))
      (get props (str k))))

(defn- mission-plan
  [hx]
  (let [props (:hx/props hx)
        mission-endpoint (first (:hx/endpoints hx))
        mission-id (prop-get props :mission/id)
        mission-repo-label (prop-get props :repo)
        source-file (prop-get props :source-file)
        mission-code-paths (or (prop-get props :mission/code-paths) [])]
    (merge {:mission-endpoint mission-endpoint
            :mission-id mission-id}
           (ingest/build-file-to-mission-edge-docs
            {:path source-file
             :label mission-repo-label
             :mission-id mission-id
             :mission-endpoint mission-endpoint
             :mission-code-paths mission-code-paths}))))

(defn- dry-run-report
  [limit]
  (let [{:keys [mission-docs unreadable]} (fetch-mission-docs limit)
        plans (mapv mission-plan mission-docs)]
    {:mode :dry-run
     :mission-doc-count (count mission-docs)
     :unreadable-mission-docs (count unreadable)
     :planned-edges (reduce + 0 (map #(count (:edge-docs %)) plans))
     :unresolved-code-paths (reduce + 0 (map #(count (:unresolved-code-paths %)) plans))
     :sample-edge (select-keys (first (mapcat :edge-docs plans))
                               [:hx-type :endpoints :labels])}))

(defn- write-report
  [limit]
  (let [{:keys [mission-docs unreadable]} (fetch-mission-docs limit)
        plans (mapv mission-plan mission-docs)
        edge-docs (vec (mapcat :edge-docs plans))
        unresolved (vec (mapcat :unresolved-code-paths plans))
        results (reduce (fn [acc doc]
                          (let [resp (ingest/post-hyperedge-doc! doc)]
                            (update acc (if (:ok? resp) :emitted :failed) inc)))
                        {:emitted 0 :failed 0}
                        edge-docs)]
    {:mode :write
     :mission-doc-count (count mission-docs)
     :unreadable-mission-docs (count unreadable)
     :planned-edges (count edge-docs)
     :emitted (:emitted results)
     :failed (:failed results)
     :unresolved-code-paths (count unresolved)}))

(defn -main
  [& argv]
  (let [{:keys [mode limit]} (parse-args argv)
        report (if (= mode :write)
                 (write-report limit)
                 (dry-run-report limit))]
    (pprint report)
    (when (and (= mode :write) (pos? (:failed report 0)))
      (throw (ex-info "file→mission backfill had failures" report)))))

(apply -main *command-line-args*)
