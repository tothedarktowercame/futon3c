#!/usr/bin/env -S clojure -M
(ns scripts.ingest-flight-to-futon1a
  "Bootstrap one WM flight record into the Arxana essay projection. Dry-run is
  the default; --write is intentionally double-gated for the operator step."
  (:require [clojure.pprint :as pp]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [futon3c.watcher.projections.flight :as flight])
  (:import (java.net URLEncoder)))

(def futon1a-url (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))

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

(defn- url-encode [s]
  (URLEncoder/encode (str s) "UTF-8"))

(defn- fetch-existing-entity [entity-id]
  (let [resp (http/get (str futon1a-url "/api/alpha/entity/" (url-encode entity-id))
                       {:headers {"Accept" "application/json"}
                        :throw false})]
    (when (= 200 (:status resp))
      (:entity (json/parse-string (:body resp) true)))))

(defn- annotation-doc [annotation base-props labels]
  (-> annotation
      (update :props #(merge base-props (or % {})))
      (assoc :labels labels)))

(defn ingest-projection!
  "Live substrate write. This is intentionally gated; do not call in reviews
  unless the operator has set FUTON3C_FLIGHT_INGEST_WRITE=1."
  [projection label]
  (when-not (= "1" (System/getenv "FUTON3C_FLIGHT_INGEST_WRITE"))
    (throw (ex-info "Refusing live flight ingest without operator gate"
                    {:required-env "FUTON3C_FLIGHT_INGEST_WRITE=1"})))
  (require 'futon3c.watcher.file-ingest)
  (let [post-entity! (requiring-resolve 'futon3c.watcher.file-ingest/post-entity!)
        post-hyperedge-doc! (requiring-resolve 'futon3c.watcher.file-ingest/post-hyperedge-doc!)
        labels ["v05" "phase-4.5" label "wm-flight"]
        source-file (get-in projection [:essay :source-file])
        base-props {"repo" label
                    "phase" "phase-4.5"
                    "source-file" source-file
                    "essay/id" (get-in projection [:essay :id])}
        entities (cons (:essay projection) (:sections projection))]
    (doseq [entity entities]
      ;; post-entity! is single-arity {:id :name :type :props}; entities carry no
      ;; labels (labels live on the hyperedge docs, per the watcher's own essay
      ;; ingest). Passing `labels` here threw ArityException on live --write.
      (post-entity! (flight/merge-entity-for-upsert (fetch-existing-entity (:id entity)) entity)))
    (doseq [annotation (:annotations projection)]
      (post-hyperedge-doc! (annotation-doc annotation base-props labels)))
    {:mode :write
     :entities (count entities)
     :annotations (count (:annotations projection))}))

(defn -main [& args]
  (let [{:keys [path label write?]} (parse-args args)
        projection (flight/collect-file path)
        result (if write?
                 (ingest-projection! projection label)
                 (dry-run-summary projection label path))]
    (pp/pprint result)
    (when write?
      (println "Live flight ingest completed."))))

(apply -main *command-line-args*)
