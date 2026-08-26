(ns futon3c.inbox-zero.followup-validity
  "Delivery-time staleness checks for durable inbox-zero followups."
  (:require [futon3.inbox-zero.projection :as projection]
            [futon3.inbox-zero.state :as state]
            [futon3c.dev.config :as config])
  (:import [java.util Date]))

(def default-state-path "/home/joe/code/storage/inbox-zero/state.edn")

(defn- value [m k]
  (or (get m k) (get m (name k))))

(defn validator
  "Build a per-request validator, loading and projecting state at most once."
  [options]
  (let [loaded (delay
                 (let [load! (or (:load-state-fn options) state/load-state)
                       now ((or (:now-fn options) #(Date.)))
                       path (or (:state-path options)
                                (config/env "FUTON3C_INBOX_ZERO_STATE_PATH")
                                default-state-path)]
                   (projection/project-dirty-sets (load! path) now)))
        print! (or (:print-fn options) println)]
    (fn [item]
      (try
        (let [metadata (:metadata item)
              proposal-type (value metadata :proposal/type)
              route-tier (value metadata :route-tier)]
          (cond
            (#{:inbox-zero/attribution "inbox-zero/attribution"} proposal-type)
            (let [path-key (value metadata :path/key)
                  worktree (value path-key :worktree/id)
                  path (value path-key :path)]
              (boolean (some #(and (= worktree (:worktree/id %))
                                   (= path (:path %)))
                             (:unattributed @loaded))))

            (= 1 route-tier)
            (let [seat (str "seat:" (:agent item) ":" (:session item))
                  worktree (value metadata :worktree-id)]
              (boolean (some #(and (= seat (:seat/id %))
                                   (= worktree (:worktree/id %))
                                   (pos? (:count %)))
                             (:dirty-sets @loaded))))

            :else true))
        (catch Throwable error
          (print! (str "[inbox-zero] followup validity check failed: "
                       (.getMessage error)))
          true)))))

(defn still-current?
  "Return whether ITEM still describes current inbox-zero work."
  ([item] (still-current? item {}))
  ([item options] ((validator options) item)))
