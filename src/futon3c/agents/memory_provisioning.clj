(ns futon3c.agents.memory-provisioning
  "Configuration-backed memory domains and write capabilities for agent seats."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn load-seats
  []
  (let [resource (io/resource "memory-seats.edn")]
    (when-not resource
      (throw (ex-info "memory-seats.edn not found on classpath" {})))
    (:seats (edn/read-string (slurp resource)))))

(defn seat
  [agent-id]
  (get (load-seats) (str agent-id)))

(defn domain-for
  [agent-id]
  (:domain (seat agent-id)))

(defn tool-enabled?
  [agent-id tool]
  (contains? (or (:tools (seat agent-id)) #{}) tool))
