(ns futon3c.peripheral.war-machine-pilot-guardrails-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.decision-gate :as futon2-gate]
            [futon2.aif.policy :as futon2-policy]
            [futon3c.peripheral.war-machine-pilot :as pilot])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-file []
  (let [dir (Files/createTempDirectory "wm-pilot-guardrails-test"
                                       (into-array FileAttribute []))]
    (.getAbsolutePath (io/file (str dir) "needs-you.edn"))))

(defn- delete-tree! [path]
  (let [root (io/file path)]
    (when (.exists root)
      (doseq [^File f (reverse (file-seq root))]
        (.delete f)))))

;; H3 (SPEC-flat-removal-and-cascade-decision, 2026-09-17): the live
;; judgement carries a cascade decision built with the REAL
;; futon2.aif.policy/select-action-cascades over receipted candidates (it
;; passes futon2.aif.decision-gate/emit!). Each candidate's :precedence
;; carries step-shaped acting actions, so the differential the pilot walks is
;; the posterior marginals over first acting patterns. A single-candidate
;; decision with a one-step precedence is the minimal honest fixture.

(defn- cascade-action [cascade-id type target]
  {:kind :cascade-candidate
   :cascade-id cascade-id
   :precedence [{:type type :target target}]
   :construction-receipt {:cascade-id cascade-id :moves 1}
   :interpretation-receipts [{:pattern type :admitted true}]})

(defn- judgement [entries]
  {:mode :base-case
   :decision (futon2-gate/emit!
              (futon2-policy/select-action-cascades entries {:beta 0.25}))})

(defn- ranked [rank type target g]
  {:action (cascade-action (keyword (str "C" rank)) type target)
   :controller-score g})

(defn- scheduler-resolve [sym]
  (case sym
    futon3c.wm.scheduler/status (fn [] {:tick-count 17})
    futon3c.wm.scheduler/request-tick! (fn [] {:queued? true})
    (throw (ex-info "unexpected requiring-resolve" {:sym sym}))))

(defmacro with-tmp-runs-dir
  "Bind the pilot's begin-state dir to a temp dir for the body — fixture
   begins must never write into the real traces dir (calibration-evidence
   pollution, 2026-06-11)."
  [& body]
  `(let [dir# (Files/createTempDirectory "wm-pilot-runs-test"
                                         (into-array FileAttribute []))]
     (try
       (binding [pilot/*live-runs-dir* (str dir#)]
         ~@body)
       (finally (delete-tree! (str dir#))))))

(deftest default-begin-live-cycle-rides-guardrails-test
  ;; The DEFAULT flight path rides guardrails (pilot cycle #1, 2026-06-10):
  ;; operator-only top action is stepped past + NAGged; v = first :autonomous.
  (let [path (temp-file)]
    (try
      (with-redefs [futon3c.peripheral.war-machine-pilot/live-judgement
                    (fn []
                      (judgement [(ranked 1 :learn-action-class :open-mission -9.0)
                                  (ranked 2 :address-sorry "sorry/foo" -1.0)]))
                    clojure.core/requiring-resolve scheduler-resolve]
        (let [result (with-tmp-runs-dir (pilot/begin-live-cycle! {:tick? false
                                               :needs-you-path path}))]
          (is (= true (:ok result)))
          (is (= {:type :address-sorry :target "sorry/foo"} (:v result)))
          (is (= 1 (:needs-you-emitted result)))))
      (finally
        (delete-tree! (.getParent (io/file path)))))))

(deftest raw-mode-keeps-top-action-test
  ;; :guardrails? false = explicit opt-out, raw field read, no classification.
  (with-redefs [futon3c.peripheral.war-machine-pilot/live-judgement
                (fn []
                  (judgement [(ranked 1 :learn-action-class :open-mission -9.0)
                              (ranked 2 :address-sorry "sorry/foo" -1.0)]))
                clojure.core/requiring-resolve scheduler-resolve]
    (let [result (with-tmp-runs-dir (pilot/begin-live-cycle! {:tick? false :guardrails? false}))]
      (is (= true (:ok result)))
      (is (= {:type :learn-action-class :target :open-mission} (:v result)))
      (is (not (contains? result :needs-you-emitted)))
      (is (not (contains? result :needs-you-path))))))

(deftest guardrails-mode-steps-to-first-autonomous-action-test
  (let [path (temp-file)]
    (try
      (with-redefs [futon3c.peripheral.war-machine-pilot/live-judgement
                    (fn []
                      (judgement [(ranked 1 :learn-action-class :open-mission -9.0)
                                  (ranked 2 :address-sorry "sorry/foo" -1.0)]))
                    clojure.core/requiring-resolve scheduler-resolve]
        (let [result (with-tmp-runs-dir (pilot/begin-live-cycle! {:guardrails? true
                                               :tick? false
                                               :needs-you-path path}))]
          (is (= true (:ok result)))
          (is (= {:type :address-sorry :target "sorry/foo"} (:v result)))
          (is (= 1 (:needs-you-emitted result)))
          (let [items (edn/read-string (slurp path))]
            (is (= 1 (count items)))
            (is (= :learn-action-class (:wm-action-class (first items))))
            (is (= :orchestration/pattern-warranted-choice-point
                   (get-in (first items) [:pattern-warrant :pattern-id])))
            (is (:unblock-action (first items))))))
      (finally
        (delete-tree! (.getParent (io/file path)))))))

(deftest chosen-target-selects-from-live-differential-test
  ;; cycle-5 apparatus: :target picks a specific ranked entry; guardrails are
  ;; consulted + recorded but an operator-directed choice proceeds; a target
  ;; absent from the field THROWS (predicted-G must come from the field).
  (with-redefs [futon3c.peripheral.war-machine-pilot/live-judgement
                (fn []
                  (judgement [(ranked 1 :address-sorry "sorry/foo" -1.0)
                              (ranked 2 :advance-mission "M-zeta" -4.0)]))
                clojure.core/requiring-resolve scheduler-resolve]
    (let [result (with-tmp-runs-dir
                   (pilot/begin-live-cycle!
                    {:tick? false :target "M-zeta"
                     :guardrails-ctx {:mission-status-fn
                                      (fn [_] {:open? true :open-hole-count 0})}}))]
      (is (= true (:ok result)))
      (is (= {:type :advance-mission :target "M-zeta"} (:v result)))
      (is (number? (:predicted-discharge result)))
      (is (pos? (:predicted-discharge result))
          "predicted = the enacted step's posterior marginal from the field")
      (is (= :operator-directed (:v-attribution result)))
      (is (= :needs-operator (:guardrails/classification result))
          "classification recorded honestly even though operator direction proceeds")
      (is (= :open-mission-no-holes (:guardrails/rule result))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not in the live differential"
                          (with-tmp-runs-dir
                            (pilot/begin-live-cycle! {:tick? false :target "M-not-ranked"}))))))

(deftest guardrails-mode-soft-stops-when-no-autonomous-action-test
  (let [path (temp-file)]
    (try
      (with-redefs [futon3c.peripheral.war-machine-pilot/live-judgement
                    (fn []
                      (judgement [(ranked 1 :learn-action-class :open-mission -9.0)
                                  (ranked 2 :open-mission "M-net-new" -8.0)]))]
        (let [result (with-tmp-runs-dir (pilot/begin-live-cycle! {:guardrails? true
                                               :tick? false
                                               :needs-you-path path
                                               :guardrails-ctx
                                               {:mission-status-fn
                                                (fn [_] {:open? false :open-hole-count 1})}}))]
          (is (= false (:ok result)))
          (is (= :no-autonomous-action (:reason result)))
          (is (= 2 (:needs-you-emitted result)))
          (let [items (edn/read-string (slurp path))]
            (is (= 2 (count items)))
            (is (every? :unblock-action items))
            (is (= #{:orchestration/pattern-warranted-choice-point
                     :war-machine/advanceability}
                   (set (map #(get-in % [:pattern-warrant :pattern-id])
                             items)))))))
      (finally
        (delete-tree! (.getParent (io/file path)))))))
