(ns futon3c.dev.shadow
  "Embedded shadow-cljs management for futon3c.

   Runs CLJS watches inside the futon3c JVM (one process for war-machine
   + webarxana) instead of spawning standalone `npx shadow-cljs' JVMs.
   Satisfies CLAUDE.md I-0 (one JVM at rest) while still permitting
   live CLJS hot-reload when actively editing CLJS source.

   Configuration lives in /home/joe/code/futon3c/shadow-cljs.edn.
   Driven from Emacs via emacs/shadow-control.el (M-x start-shadow,
   M-x stop-shadow, M-x shadow-status)."
  (:require [shadow.cljs.devtools.api    :as shadow]
            [shadow.cljs.devtools.server :as server]))

(defonce ^:private !server-started? (atom false))
(defonce ^:private !watching        (atom #{}))

(defn ensure-server!
  "Boot the shadow-cljs server if not already running. Returns :started
   on a fresh boot, :already-running otherwise."
  []
  (if @!server-started?
    :already-running
    (do
      (server/start!)
      (reset! !server-started? true)
      :started)))

(defn start!
  "Start a watch on each BUILD-IDS (keywords or strings).
   Returns a map of build-id → status (:started, :already-watching,
   or {:error <msg>}). The shadow-cljs server is booted on first call."
  [& build-ids]
  (ensure-server!)
  (into {}
        (for [id build-ids
              :let [id (keyword id)]]
          [id (try
                (if (contains? @!watching id)
                  :already-watching
                  (do (shadow/watch id)
                      (swap! !watching conj id)
                      :started))
                (catch Throwable t
                  {:error (.getMessage t)}))])))

(defn stop!
  "Stop watches.
   With no args, stops every watch this ns started AND shuts the
   embedded shadow-cljs server down.
   With BUILD-IDS, stops only those watches and leaves the server up
   (useful when iterating on one build while another stays live)."
  [& build-ids]
  (if (seq build-ids)
    (into {}
          (for [id build-ids
                :let [id (keyword id)]]
            [id (try
                  (shadow/stop-worker id)
                  (swap! !watching disj id)
                  :stopped
                  (catch Throwable t {:error (.getMessage t)}))]))
    (do
      (doseq [id @!watching]
        (try (shadow/stop-worker id) (catch Throwable _ nil)))
      (reset! !watching #{})
      (when @!server-started?
        (try (server/stop!) (catch Throwable _ nil))
        (reset! !server-started? false))
      {:server :stopped :workers :stopped})))

(defn status
  "Return current embedded shadow-cljs status."
  []
  {:server-started? @!server-started?
   :watches         (vec @!watching)})
