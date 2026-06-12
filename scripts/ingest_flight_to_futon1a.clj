#!/usr/bin/env -S clojure -M
(ns scripts.ingest-flight-to-futon1a
  "Bootstrap one WM flight record into the Arxana essay projection. Dry-run is
  the default; --write is intentionally double-gated for the operator step.
  Thin CLI — the write logic lives (and is tested) in
  futon3c.watcher.flight-ingest."
  (:require [clojure.pprint :as pp]
            [futon3c.watcher.flight-ingest :as ingest]
            [futon3c.watcher.projections.flight :as flight]))

(def default-record
  "data/repl-traces/live-df706c45-d0b4-402a-9f0f-5222d42ab470.flight.edn")

(defn- parse-args [args]
  (loop [args args
         opts {:path default-record
               :label "futon3c"
               :write? false}]
    (if (empty? args)
      opts
      (let [[a b & more] args]
        (case a
          "--path" (recur more (assoc opts :path b))
          "--label" (recur more (assoc opts :label b))
          "--dry-run" (recur (if b (cons b more) more) (assoc opts :write? false))
          "--write" (recur (if b (cons b more) more) (assoc opts :write? true))
          (throw (ex-info "Unknown argument" {:arg a})))))))

(defn- dry-run-summary [projection label path]
  {:mode :dry-run
   :path path
   :label label
   :flight-id (get-in projection [:essay :props :flight/id])
   :essay-id (get-in projection [:essay :id])
   :planned-entities (+ 1 (count (:sections projection)))
   :planned-annotations (count (:annotations projection))
   :sample-essay (select-keys (:essay projection) [:id :name :type])
   :sample-section (select-keys (first (:sections projection)) [:id :name :type])
   :sample-annotation (select-keys (first (:annotations projection)) [:id :hx-type :endpoints])})

(defn -main [& args]
  (let [{:keys [path label write?]} (parse-args args)
        projection (flight/collect-file path)
        result (if write?
                 (ingest/ingest-projection! projection label)
                 (dry-run-summary projection label path))]
    (pp/pprint result)
    (when write?
      (println "Live flight ingest completed."))))

(apply -main *command-line-args*)
