(ns futon3c.xiang.ma-observer
  "馬 on turn finalisation: when a turn's finalising row is stored
   (futon3c.agency.turn-events :post), run scripts/ma_observe.py once for
   that session, if its agent is listed in ~/code/data/minus40/ma-enabled.txt.
   Replaces the 20 s polling watcher (Joe, 2026-10-17: everything event
   driven). One daemon thread, so 馬's model calls (40-90 s) never pile up;
   events for a session already queued are coalesced, and a run picks up
   every finished, unobserved turn of the session anyway."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.agency.turn-events :as turn-events])
  (:import [java.util.concurrent Executors ExecutorService ThreadFactory]))

(def script "/home/joe/code/futon3c/scripts/ma_observe.py")
(def enabled-file (str (System/getProperty "user.home") "/code/data/minus40/ma-enabled.txt"))

(defonce ^:private ^ExecutorService executor
  (Executors/newSingleThreadExecutor
   (reify ThreadFactory
     (newThread [_ r] (doto (Thread. ^Runnable r "ma-observer") (.setDaemon true))))))

(defonce ^:private !queued (atom #{}))
(defonce !last (atom nil))

(defn enabled-agents []
  (let [f (io/file enabled-file)]
    (if (.exists f)
      (->> (str/split-lines (slurp f)) (map str/trim) (remove #(or (str/blank? %) (str/starts-with? % "#"))) set)
      #{})))

(defn- run-script [session agent]
  (shell/sh "timeout" "600" "python3" "-I" "-u" script "once" session "--agent" agent))

(defonce ^{:doc "The process boundary; swappable so tests never start python."}
  !run (atom run-script))

(defn observe!
  "Queue 馬 for SESSION of AGENT (coalesced per session)."
  [session agent]
  (when (and session agent (not (contains? @!queued session)))
    (swap! !queued conj session)
    (.submit executor
             ^Runnable
             (fn []
               (swap! !queued disj session)
               (let [t0 (System/currentTimeMillis)
                     r (try (@!run session agent) (catch Throwable e {:exit -1 :err (str e)}))]
                 (reset! !last {:session session :agent agent :exit (:exit r)
                                :ms (- (System/currentTimeMillis) t0)
                                :out (some-> (:out r) str/trim (#(subs % (max 0 (- (count %) 400)))))}))))
    true))

(turn-events/add-listener!
 ::ma :post
 (fn [{:keys [session-id author]}]
   (let [agent (some-> author str)]
     (when (contains? (enabled-agents) agent)
       (observe! session-id agent)))))
