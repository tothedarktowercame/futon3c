(ns futon3c.diagramprover.wm-wire-loop-entry-r1-outer-cascade-trigger-test
  "Wire [:loop-entry :r1-outer-cascade :trigger]: which clock fired the run
  reaching the outer cascade.

  Both ends exist. The writer: wm-scheduled-run/-main's :trigger is
  (trigger-from-env) — FUTON_WM_TRIGGER as a keyword, else :unspecified —
  which the flight path (flight-plan!) passes to outer-loop/plan-from-field!
  and so to the reader. -main itself is not drivable hermetically (it runs
  a scheduled tick: trace writes, evidence emit), so the writer's value is
  observed by a real call of wm-scheduled-run/trigger-from-env's 1-arity —
  the exact form -main calls. The reader is outer-cascade/select (built by
  H-T-CALLER-I, futon2 3b449beb..b66369d3, after the map at 06d451d6 marked
  the box :not-built): it records the given :trigger on
  [:target-selection :trigger], with {:absent :no-trigger} when none is
  given.

  No live record carries this wire's :trigger: the :trigger keys on the
  spike's records are the full-loop-runner's phase-event provenance
  (:duree-click-on-demand), not the scheduled run's, and the cascade was
  not built when they were written (see live-records-read, each pinned and
  read). So the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.outer-cascade :as oc]
            [futon3c.diagramprover.wm-wire :as w]
            [wm-scheduled-run]))

(defn- trigger-from-env [getenv]
  (@(ns-resolve 'wm-scheduled-run 'trigger-from-env) getenv))

(def field
  "A minimal field with one eligible target (select's input shape)."
  {:considered [{:target "M-a" :kind :mission}]
   :feasible [{:target "M-a" :kind :mission :next-step :ready :eligible true}]
   :exclusions []})

(defn observe
  "trigger-from-env (writer, as -main calls it) with FUTON_WM_TRIGGER
  \"wallclock-cron\", then select (reader) over FIELD with the writer's
  value as :trigger; TRIGGER-OPT edits what select is handed (the bad
  cases). {:writer the trigger keyword, :reader the value the reader
  records under [:target-selection :trigger]}."
  ([] (observe ::written))
  ([trigger-opt]
   (let [w (trigger-from-env (constantly "wallclock-cron"))
         handed (if (= ::written trigger-opt) w trigger-opt)
         r (oc/select {:field field :seed 1 :trigger handed})]
     {:writer w
      :reader (get-in r [:target-selection :trigger])})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "carries no :wm-version stamp at all, so no scheduled-run :trigger; the :trigger keys it carries are guard triggers, not the loop entry's clock"}
     {:path (p "flight-ada87008/repair-occ-036463620c0c032c9e46aa44b6a6d6b35ed3e0ceb27dc58030f07d8fc2e6c747.edn")
      :sha256 "95bbb9c5476aedaadc50125068fff5f0a7e773c29a7a7494d84be58a57d893ad"
      :why "its :trigger values are the full-loop-runner's phase-event provenance (:duree-click-on-demand), a different carrier than the scheduled run's :trigger; and the outer cascade was not built when it was written"}]))

(def wire
  {:wire [:loop-entry :r1-outer-cascade :trigger]
   :kind :witnessed-hermetically
   :test `the-trigger-reaches-the-cascade
   :check check
   :live-records-read live-records-read})

(deftest the-trigger-reaches-the-cascade
  (let [o (check)]
    (is (= :wallclock-cron (:writer o))
        "the writer's end, from a real trigger-from-env call as -main makes it")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  ;; select records {:absent :no-trigger} when nothing is handed
  (let [o (observe nil)]
    (is (= {:absent :no-trigger} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-trigger-fails-the-wire
  (let [o (observe :duree-click-on-demand)]
    (is (some? (:reader o)))
    (is (not (w/received? o))
        "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-loop-trigger
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (let [r (w/read-record (:path (first live-records-read)))]
    (is (nil? (get-in r [:wm-version :trigger]))))
  (let [r (w/read-record (:path (second live-records-read)))
        triggers (keep :trigger (get-in r [:backtrace :phase-events]))]
    (is (seq triggers))
    (is (every? #(= :duree-click-on-demand %) triggers)
        "the only :trigger carrier on the record is the runner's phase provenance")))
