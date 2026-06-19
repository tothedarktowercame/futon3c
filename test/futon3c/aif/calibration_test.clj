(ns futon3c.aif.calibration-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.aif.calibration :as calibration]
            [futon3c.aif.discipline-events :as discipline]))

(defn- tmp-dir []
  (doto (java.io.File/createTempFile "calibration-" "")
    (.delete)
    (.mkdirs)))

(defn- write-edn [path x]
  (spit path (pr-str x))
  path)

(defn- paths-fixture []
  (let [root (tmp-dir)
        traces (doto (io/file root "traces") (.mkdirs))
        pilots (io/file root "PILOTS.md")
        folds (io/file root "closure-folds.edn")
        ch2 (io/file root "ch2.edn")
        discipline (io/file root "discipline.edn")]
    {:root root
     :paths {:traces-dir (.getPath traces)
             :pilots-log (.getPath pilots)
             :closure-folds (.getPath folds)
             :ch2-events (.getPath ch2)
             :discipline-events (.getPath discipline)}}))

(deftest load-evidence-normalizes-all-kinds
  (let [{:keys [root paths]} (paths-fixture)]
    (try
      (write-edn (io/file (:traces-dir paths) "g.edn")
                 {:date "2026-06-10"
                  :run-id "run-g"
                  :trace [{:prediction-error 0.25
                           :predicted-discharge -4.0
                           :realised-discharge -3.75}]})
      (spit (:pilots-log paths)
            (str "## Turn 1 — fixture\n"
                 "**READ.** Live WM recommended **`open-mission M-y`** (G=−5.69).\n"
                 "**PRINT / FOUND.** predicted G=-2.0; realised G=-1.5\n"))
      (write-edn (:closure-folds paths)
                 [{:scope "s1" :used [] :success true}])
      (spit (:ch2-events paths)
            (str (pr-str {:ch2/discharge-event true
                          :move/id "move-1"
                          :discharged? true
                          :at "t"}) "\n"))
      (discipline/append-event! {:discipline/event :guardrail-trip
                                 :run-id "run-d"
                                 :at "t2"
                                 :action {:type :open-mission :target "M-y"}
                                 :predicted -1.0}
                                (:discipline-events paths))
      (let [evidence (calibration/load-evidence paths)
            by-kind (group-by :kind evidence)]
        (is (= #{:gamma-frame :pilots-log-turn :closure-fold :ch2-discharge :discipline-event}
               (set (keys by-kind))))
        (is (= 0.25 (:error (first (:gamma-frame by-kind)))))
        (is (= 2 (count (:pilots-log-turn by-kind))))
        (is (= 0.5 (:error (first (filter :realised (:pilots-log-turn by-kind))))))
        (is (= :operator-gate (:witness-class (first (:pilots-log-turn by-kind)))))
        (is (true? (:success (first (:closure-fold by-kind)))))
        (is (= :build-discharge (:witness-class (first (:closure-fold by-kind)))))
        (is (true? (:success (first (:ch2-discharge by-kind)))))
        (is (= :build-discharge (:witness-class (first (:ch2-discharge by-kind)))))
        (is (= -1.0 (:predicted (first (:discipline-event by-kind))))))
      (finally
        (doseq [f (reverse (file-seq root))] (.delete f))))))

(deftest degenerate-detection-true-and-calibratable-false
  (let [degenerate (repeat 11 {:kind :gamma-frame :predicted 1.0 :realised 1.0
                               :error 0.0 :independent? true :realised-source :measured})
        nondegenerate (for [i (range 11)]
                        {:kind :gamma-frame :predicted 1.0 :realised (+ 2.0 i)
                         :independent? true :realised-source :measured})]
    (is (= :degenerate (:verdict (calibration/calibration-report (vec degenerate)))))
    (is (true? (:degenerate? (calibration/calibration-report (vec degenerate)))))
    (is (= :calibratable (:verdict (calibration/calibration-report (vec nondegenerate)))))
    (is (false? (:degenerate? (calibration/calibration-report (vec nondegenerate)))))))

(deftest insufficient-evidence-when-fewer-than-ten-pairs
  (let [evidence [{:kind :gamma-frame :predicted 1.0 :realised 1.0
                   :error 0.0 :independent? true :realised-source :measured}]]
    (is (= :insufficient-evidence
           (:verdict (calibration/calibration-report evidence))))))

(deftest non-independent-volume-cannot-flip-the-verdict
  ;; The anti-laundering core: any volume of self-referential (proposal-mode)
  ;; pairs leaves the verdict at :insufficient-evidence — only pairs tagged
  ;; :independent? true (realised from an EXECUTED action) count toward
  ;; clearance. The gate that diagnoses degeneracy must not be clearable by
  ;; more degeneracy.
  (let [self-referential (repeat 100 {:kind :gamma-frame :predicted 1.0
                                      :realised 1.0 :error 0.0})
        report (calibration/calibration-report (vec self-referential))]
    (is (= 100 (:paired-count report)))
    (is (= 0 (:independent-paired-count report)))
    (is (= :insufficient-evidence (:verdict report)))
    (is (true? (:degenerate? report)) "diagnosis still reports the degeneracy")
    ;; and a healthy independent set alongside them clears on ITS merits only
    (let [mixed (into (vec self-referential)
                      (for [i (range 11)]
                        {:kind :gamma-frame :predicted 1.0 :realised (+ 2.0 i)
                         :independent? true :realised-source :measured}))]
      (is (= :calibratable (:verdict (calibration/calibration-report mixed)))))))

(deftest fallback-realised-cannot-count-as-independent
  ;; A vanished-target close copies predicted as realised (error 0.0 by
  ;; construction — a censored observation, not a measurement). Tagged
  ;; :target-absent-fallback — or untagged, on pre-tagging frames — such
  ;; pairs are excluded from the verdict even when :independent? true.
  (let [fallback (repeat 11 {:kind :gamma-frame :predicted -4.0 :realised -4.0
                             :error 0.0 :independent? true
                             :realised-source :target-absent-fallback})
        untagged (repeat 11 {:kind :gamma-frame :predicted -4.0 :realised -4.0
                             :error 0.0 :independent? true})
        report-f (calibration/calibration-report (vec fallback))
        report-u (calibration/calibration-report (vec untagged))]
    (is (= 0 (:independent-paired-count report-f)))
    (is (= :insufficient-evidence (:verdict report-f)))
    (is (= 0 (:independent-paired-count report-u)) "strict: only explicit :measured counts")
    (is (= :insufficient-evidence (:verdict report-u)))))

(deftest missing-sources-become-warnings-not-throws
  (let [evidence (calibration/load-evidence {:traces-dir "/tmp/no-such-traces-dir-for-calibration"
                                             :pilots-log "/tmp/no-such-pilots-log-for-calibration.md"
                                             :closure-folds "/tmp/no-such-closure-folds.edn"
                                             :ch2-events "/tmp/no-such-ch2-events.edn"
                                             :discipline-events "/tmp/no-such-discipline-events.edn"})]
    (is (= [] evidence))
    (is (= 4 (count (:warnings (meta evidence)))))))
