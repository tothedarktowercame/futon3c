(ns futon3c.watcher.flight-ingest
  "The live --write half of the exit-4 flight ingest (M-first-flights).
   Extracted from scripts/ingest_flight_to_futon1a.clj at the code gate: the
   script executes -main on load, which is what made this path untestable —
   and the untested path is exactly where the post-entity! arity bug hid
   (claude-3's root-cause flag, 8d5d094). The script is now a thin CLI over
   this ns; test/futon3c/watcher/flight_ingest_test.clj covers the call
   shapes, the no-labels-on-entities regression, and merge idempotency.

   The operator gate is ABSOLUTE in code: ingest-projection! refuses unless
   FUTON3C_FLIGHT_INGEST_WRITE=1 — the live --write belongs to Joe."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [futon3c.watcher.projections.flight :as flight])
  (:import (java.net URLEncoder)))

(def futon1a-url (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))

(defn assert-write-gate!
  "Throw unless the operator has armed the live write. Separate fn so the
   write-path test can exercise the call shapes with the gate explicitly
   stubbed, while this gate itself is tested to refuse."
  []
  (when-not (= "1" (System/getenv "FUTON3C_FLIGHT_INGEST_WRITE"))
    (throw (ex-info "Refusing live flight ingest without operator gate"
                    {:required-env "FUTON3C_FLIGHT_INGEST_WRITE=1"}))))

(defn- url-encode [s]
  (URLEncoder/encode (str s) "UTF-8"))

(defn fetch-existing-entity
  "Fetch the current substrate entity (nil when absent) — the fetch half of
   fetch-merge-upsert (futon1a upsert REPLACES props; see
   flight/merge-entity-for-upsert)."
  [entity-id]
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
  "Live substrate write, operator-gated (assert-write-gate!). Entities go
   through fetch-merge-upsert and carry NO labels (post-entity! is
   single-arity {:id :name :type :props}; labels live on the hyperedge docs,
   per the watcher's own essay ingest — the 8d5d094 regression)."
  [projection label]
  (assert-write-gate!)
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
      (post-entity! (flight/merge-entity-for-upsert (fetch-existing-entity (:id entity)) entity)))
    (doseq [annotation (:annotations projection)]
      (post-hyperedge-doc! (annotation-doc annotation base-props labels)))
    {:mode :write
     :entities (count entities)
     :annotations (count (:annotations projection))}))
