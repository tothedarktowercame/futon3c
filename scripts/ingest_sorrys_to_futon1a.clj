#!/usr/bin/env -S clojure -M
;; One-off substrate-2 bootstrap for the canonical sorry registry.
;;
;; Canonical path (T-A2):
;;   The enduring source of truth is the watcher-integrated ingest path in
;;   `futon3c.watcher.file-ingest/ingest-sorry-registry!`, triggered by writes
;;   to `futon2/data/sorrys.edn`.
;;
;; Takeover proof (T-A2):
;;   The watcher has taken over once it has populated `code/v05/sorry` from the
;;   live registry AND one subsequent edit to `sorrys.edn` reaches substrate-2
;;   without rerunning this script.
;;
;; Invalidation moment (T-A2):
;;   After that takeover proof, this script is no longer the canonical path.
;;   Any later need to rerun it is a VERIFY failure to investigate, not a normal
;;   maintenance step.
;;
;; Usage:
;;   clojure scripts/ingest_sorrys_to_futon1a.clj --fixture
;;   clojure scripts/ingest_sorrys_to_futon1a.clj --dry-run
;;   clojure scripts/ingest_sorrys_to_futon1a.clj --write
;;
;; Default mode is dry-run. `--write` performs live POSTs and should only be
;; used with operator approval.

(require '[clojure.edn :as edn]
         '[clojure.pprint :refer [pprint]]
         '[futon3c.watcher.file-ingest :as ingest])

(def ^:private default-path
  "/home/joe/code/futon2/data/sorrys.edn")

(def ^:private default-label
  "futon2")

(defn- parse-args
  [argv]
  (loop [args argv
         opts {:path default-path
               :label default-label
               :mode :dry-run
               :fixture? false}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (case arg
          "--path" (recur (nnext args) (assoc opts :path (second args)))
          "--label" (recur (nnext args) (assoc opts :label (second args)))
          "--fixture" (recur (next args) (assoc opts :fixture? true))
          "--dry-run" (recur (next args) (assoc opts :mode :dry-run))
          "--write" (recur (next args) (assoc opts :mode :write))
          (throw (ex-info "unknown arg"
                          {:arg arg
                           :argv argv})))))))

(defn- fixture-resolver
  [registry]
  (into {}
        (map (fn [mission-name]
               [(subs (str mission-name) 2)
                (str "fixture/mission/" (subs (str mission-name) 2))]))
        (distinct
         (mapcat :related-missions (or (:sorrys registry) [])))))

(defn- dry-run-plan
  [{:keys [path label]}]
  (let [registry (edn/read-string (slurp path))
        plan (ingest/build-sorry-registry-docs
              {:path path
               :label label
               :registry registry
               :mission-resolver (fixture-resolver registry)})]
    {:mode :dry-run
     :path path
     :label label
     :planned-vertices (count (:vertex-docs plan))
     :planned-edges (count (:edge-docs plan))
     :unresolved-related (count (:unresolved-related plan))
     :sample-vertex (select-keys (first (:vertex-docs plan))
                                 [:hx-type :endpoints :labels])}))

(defn -main
  [& argv]
  (let [{:keys [path label mode fixture?] :as opts} (parse-args argv)
        report (cond
                 fixture?
                 (merge {:mode mode
                         :path path
                         :label label}
                        (ingest/fixture-sorry-roundtrip {:path path
                                                         :label label}))

                 (= mode :write)
                 (merge {:mode :write
                         :path path
                         :label label}
                        (ingest/ingest-sorry-registry! {:path path
                                                        :label label}))

                 :else
                 (dry-run-plan opts))]
    (pprint report)
    (when (and fixture? (not (:pass? report)))
      (throw (ex-info "fixture failed" report)))))

(apply -main *command-line-args*)
