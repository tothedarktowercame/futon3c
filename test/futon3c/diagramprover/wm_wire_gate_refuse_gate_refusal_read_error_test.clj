(ns futon3c.diagramprover.wm-wire-gate-refuse-gate-refusal-read-error-test
  "Wire [:gate-refuse :gate-refusal-read :error]: the decision gate's typed
  refusal marker reaching the runner's gate-refusal translator
  (WM-GATE-REFUSAL-I).

  The writer is decision-gate/refuse!, which throws ex-info \"Inadmissible
  decision\" with ex-data {:error :inadmissible-decision :reason r :detail
  d}. The reader is full-loop-runner/gate-refusal, which reads (:error
  (ex-data e)) and translates only when it is :inadmissible-decision.

  No live record carries the reader's end: gate-refusal exists since
  futon2 6dd779ca, after all eight flights; the fifth flight's finding
  (live-records-read, pinned) carries the writer's end verbatim as
  :failure-data {:error :inadmissible-decision ...} but closed
  :untyped-failure, the translator unread. So the wire is
  WITNESSED-HERMETICALLY: refuse! is called (the writer's var at its site)
  and gate-refusal (the reader's var) is driven on the thrown exception."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.decision-gate :as gate]
            [futon2.aif.full-loop-runner :as runner]
            [futon3c.diagramprover.wm-wire :as w]))

(def ^:private refuse!* @#'gate/refuse!)

(def live-record
  {:path (str w/spike-dir "/flight-7f89646a-repair-finding.edn")
   :sha256 "758eaeb64b3c0f0ef3733a1725c9a8a54f8a23701698a26373fc44b29279e264"
   :why "the fifth flight's finding: the writer's end carried verbatim as [:failure-data :error] :inadmissible-decision; the reader's end absent (the flight predates WM-GATE-REFUSAL-I and closed :untyped-failure)"})

(defn observe
  "refuse! throws (the writer); TAMPER edits the exception's ex-data before
  gate-refusal reads it (the bad cases). {:writer the :error refuse! wrote,
  :reader the :error gate-refusal read, or {:absent :not-a-gate-refusal}
  when gate-refusal does not read it}."
  ([] (observe identity))
  ([tamper]
   (let [we (try (refuse!* :missing-observation-locators
                           {:target "M-t" :missing-tokens [["M-t" :exit/h1]]})
                 (catch clojure.lang.ExceptionInfo x x))
         re (ex-info (ex-message we) (tamper (ex-data we)))]
     {:writer (:error (ex-data we))
      :reader (if (runner/gate-refusal re "M-t")
                (:error (ex-data re))
                {:absent :not-a-gate-refusal})})))

(defn check [] (observe))

(def wire
  {:wire [:gate-refuse :gate-refusal-read :error]
   :kind :witnessed-hermetically
   :test `the-gates-error-reaches-gate-refusal
   :check check
   :live-records-read [live-record]})

(deftest the-gates-error-reaches-gate-refusal
  (let [o (check)]
    (is (= :inadmissible-decision (:writer o)))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(assoc % :error {:absent :no-error}))]
    (is (= {:absent :not-a-gate-refusal} (:reader o))
        "gate-refusal does not read an :error that is not :inadmissible-decision")
    (is (not (w/received? o)))))

(deftest a-different-error-fails-the-wire
  ;; refuse! writes exactly one value, :inadmissible-decision; any other
  ;; keyword under :error is not the gate's refusal and gate-refusal reads
  ;; nothing (nil), observed here as the typed absence
  (let [o (observe #(assoc % :error :not-the-gates-error))]
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-record-carries-only-the-writers-end
  (is (= (:sha256 live-record) (w/sha256-file (:path live-record))))
  (let [r (w/read-record (:path live-record))]
    (is (= :inadmissible-decision (get-in r [:failure-data :error])))
    (is (= :missing-observation-locators (get-in r [:failure-data :reason])))
    (is (= :untyped-failure (:failure-kind r))
        "the fifth flight closed untyped: gate-refusal was never read on it")))
