(ns futon3c.inbox-zero.followup-validity
  "Delivery-time staleness checks for durable inbox-zero followups."
  (:require [clojure.edn :as edn]
            [futon3.inbox-zero.projection :as projection]
            [futon3.inbox-zero.state :as state]
            [futon3c.dev.config :as config]
            [futon3c.inbox-zero.sweeper :as sweeper]
            [futon3c.watcher.roots :as roots])
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
            (= :apm-store-repair (:type item))
            (let [path (value metadata :queue-state-path)
                  id (value metadata :hold-id)
                  queue ((or (:load-queue-fn options) #(edn/read-string (slurp %))) path)]
              (and (string? id) (seq id) (= id (get-in queue [:store-read/hold :hold/id]))))

            (#{:inbox-zero/attribution "inbox-zero/attribution"} proposal-type)
            (let [path-key (value metadata :path/key)
                  worktree (value path-key :worktree/id)
                  path (value path-key :path)]
              (boolean (some #(and (= worktree (:worktree/id %))
                                   (= path (:path %)))
                             (:unattributed @loaded))))

            (#{:inbox-zero/commit-notice "inbox-zero/commit-notice"} proposal-type)
            ;; A commit notice is about a repo's dirt right now, not when the
            ;; notice was written. Delivered after the repo was cleaned it is
            ;; noise, and it names files that no longer exist. Re-count.
            (let [label (value metadata :repo-id)
                  threshold (or (:threshold options) sweeper/default-threshold)
                  root (some #(when (= label (:label %)) (:path %))
                             (or (:roots options) roots/watch-roots))
                  git-fn (or (:git-fn options) sweeper/git-dirty)]
              (boolean (and root (>= (count (git-fn root)) threshold))))

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
          ;; A store repair must not run on an unverifiable or released hold.
          (not= :apm-store-repair (:type item)))))))

(defn still-current?
  "Return whether ITEM still describes current inbox-zero work."
  ([item] (still-current? item {}))
  ([item options] ((validator options) item)))
