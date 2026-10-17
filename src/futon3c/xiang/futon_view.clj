(ns futon3c.xiang.futon-view
  "Keeps FUTON's view (scripts/futon_view.py) current per turn event.

   The view answers, at every turn, what work the turn is on, what FUTON
   knows that bears on it at each zoom, and what the turn teaches the
   mission -> pattern links (E-prove-memory-useful, 'What real-time
   inference FUTON's view supports'). Each answer must be current before the
   next turn, so it is updated per event, never on a timer: when 象's reading
   of a turn lands (:reading) or the turn settles (:settled), that one
   session's contribution is recomputed (~1 s) and the aggregate re-summed;
   then the session's frames are rebuilt, so the stepper re-reads the view.

   Runs off the caller's path on one daemon thread; events for a session
   already queued are coalesced."
  (:require [clojure.java.shell :as shell]
            [futon3c.xiang.frames-cache :as frames-cache]
            [futon3c.xiang.turn-service :as turn-service])
  (:import [java.util.concurrent Executors ExecutorService ThreadFactory]))

(def script "/home/joe/code/futon3c/scripts/futon_view.py")

(defonce ^:private ^ExecutorService executor
  (Executors/newSingleThreadExecutor
   (reify ThreadFactory
     (newThread [_ r] (doto (Thread. ^Runnable r "futon-view") (.setDaemon true))))))

(defonce ^:private !queued (atom #{}))
(defonce !last (atom nil))

(defn- run-script [session]
  (shell/sh "timeout" "120" "python3" "-I" script "--session" session "--publish"))

(defonce ^{:doc "The process boundary, swappable so tests never start python
  (an atom, not a dynamic var: the work runs on the executor's thread)."}
  !run (atom run-script))

(defonce ^{:doc "How the session's frames are rebuilt; swappable for tests."}
  !invalidate
  (atom (fn [session]
          (when-let [opts (some-> (resolve 'futon3c.transport.http/frames-cache-opts) deref (apply []))]
            (frames-cache/invalidate! session opts)))))

(defn refresh!
  "Queue SESSION's view update (coalesced). Returns true when newly queued."
  [session]
  (when (and session (not (contains? @!queued session)))
    (swap! !queued conj session)
    (.submit executor
             ^Runnable
             (fn []
               (swap! !queued disj session)
               (let [t0 (System/currentTimeMillis)
                     r (try (@!run session) (catch Throwable e {:exit -1 :err (str e)}))]
                 (reset! !last {:session session :exit (:exit r)
                                :ms (- (System/currentTimeMillis) t0)
                                :err (some-> (:err r) (subs 0 (min 300 (count (:err r)))))})
                 (try (@!invalidate session) (catch Throwable _ nil)))))
    true))

(turn-service/add-turn-listener!
 ::futon-view
 (fn [{:keys [event session-id]}]
   (when (#{:reading :settled} event)
     (refresh! session-id))))
