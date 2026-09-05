(ns futon3c.apm.library-lane-smoke
  "Isolated library-lane smoke, the L-lane counterpart to C-square.

  The lane STEP is programmatic; phase progression, drift detection, ruling
  interpretation, coordinator scheduling and the semantic-progress watchdog are
  the production implementations. Run this before loading a change that touches
  the lane, rather than restarting library-lane:t00J02 and watching.

  KNOWN GAP, deliberately not papered over: the library lane never calls
  campaign-trace/issue-combined-trace-receipt!, so unlike C-square there is no
  Lean trace acceptance to assert here. The Lean checker has no jurisdiction
  over this lane at all. `result` reports that as :lean-trace-acceptance
  :absent rather than reporting a pass that was never checked."
  (:require [futon3c.apm.durable-coordinator :as coordinator]
            [futon3c.apm.library-lane-coordinator :as lane]
            [futon3c.apm.live-preflight-runtime :as persistence]
            [futon3c.apm.live-regulator :as regulator]
            [futon3c.apm.semantic-progress-watchdog :as watchdog]
            [clojure.java.io :as io])
  (:import [java.nio.file Path]))

(def lane-id "library-lane-smoke-v1")
(def coordinator-id lane-id)
(def root (str "data/apm-lane/smoke/" lane-id))
(def registry-path (str root "/registry.edn"))
(def coordinator-state-path (str root "/coordinator.edn"))
(def transcript-path (str root "/transcript.edn"))

;; The lane's declared phase order. The production adapter owns the successor
;; map; this is what a correct run must therefore visit, in order.
(def expected-phases [:preflight :solve :verify :bank])

(defn- persist! [path value]
  (persistence/atomic-persist! (Path/of ^String path (make-array String 0)) value))

(defn- read-transcript []
  (or (persistence/read-state
       (Path/of ^String transcript-path (make-array String 0)))
      {:state/type :library-lane-smoke :visited [] :steps 0}))

(defn- programmatic-step
  "Walk the lane's real phase sequence, certifying each phase once and closing
  on :bank. Records what the production adapter actually asked for, so a
  regression in phase progression shows up as a wrong transcript rather than a
  silent pass."
  [{:keys [phase]}]
  (let [transcript (read-transcript)
        visited (conj (vec (:visited transcript)) phase)
        armed? (boolean (watchdog/running?
                         (str "semantic-progress:" coordinator-id)))]
    (persist! transcript-path
              (assoc transcript :visited visited
                     :steps (inc (or (:steps transcript) 0))
                     :watchdog/armed-at-each (conj (vec (:watchdog/armed-at-each
                                                         transcript))
                                                   armed?)))
    (if (= :bank phase)
      {:ok true :ruling :closed :phase phase}
      {:ok true :ruling :phase-certified :phase phase})))

;; A distinct adapter key, so the programmatic step is supplied at CONSTRUCTION
;; rather than through config. Coordinator config is persisted durably, and a
;; function in it serialises as #object[...] which no reader can read back.
(def adapter-key :apm/library-lane-smoke)

(defn adapter-constructor [config]
  (lane/adapter-constructor (assoc config :lane/step-fn programmatic-step)))

(defn register-adapter! []
  (coordinator/register-adapter! adapter-key adapter-constructor))

(defn start!
  "Start only the smoke lane, through the production coordinator registry."
  []
  (register-adapter!)
  (.mkdirs (io/file root))
  (when-not (.exists (io/file coordinator-state-path))
    (persist! coordinator-state-path (regulator/initial-state coordinator-id)))
  (persist! transcript-path {:state/type :library-lane-smoke
                             :visited [] :steps 0})
  (let [registered
        (coordinator/register!
         {:registry-path registry-path :coordinator-id coordinator-id
          :adapter adapter-key
          :config {:problem-id "smoke-t00J02"}
          :state-path coordinator-state-path
          :period-ms 250})]
    (if (:ok registered)
      (coordinator/start-registered! registry-path coordinator-id)
      registered)))

(defn result []
  (let [transcript (read-transcript)
        status (coordinator/status registry-path coordinator-id)
        visited (vec (:visited transcript))
        armed (vec (:watchdog/armed-at-each transcript))
        regulator-status (get-in status [:durable-state :regulator/status])
        pass? (and (= expected-phases visited)
                   (seq armed)
                   (every? true? armed)
                   (= :complete regulator-status))]
    {:ok (boolean pass?)
     :status (if pass? :lane-complete :failed)
     :phases/expected expected-phases
     :phases/visited visited
     :watchdog/armed-at-each armed
     :coordinator/status regulator-status
     ;; Stated, not assumed. See the namespace docstring.
     :lean-trace-acceptance :absent}))
