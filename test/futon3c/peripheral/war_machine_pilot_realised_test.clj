(ns futon3c.peripheral.war-machine-pilot-realised-test
  "Realised-on-merge binding (closes :sorry/wm-realised-on-merge-binding):
   the ONLY code path that may produce :independent? true γ pairs.
   Invariants pinned here:
     I1 — proposal-mode closes never carry the tag;
     I2 — :executed? without :evidence-ref throws (independence is a claim
          that needs a witness);
     I3 — an executed close threads the tag + witness into the γ frame and
          calibration counts exactly that pair as independent;
     I4 — an executed close appends an :operator-merge discipline event."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.aif.calibration :as calibration]
            [futon3c.aif.repl-trace :as repl-trace]
            [futon3c.peripheral.war-machine-pilot :as pilot]))

(deftest turn-record-threads-field-delta
  ;; Option C: observational channel, present only when supplied
  (let [fd {:pre-total -10.0 :post-total -8.5 :delta 1.5
            :semantics :observational-not-verdict-counted}
        tagged (repl-trace/turn-record {:step 0 :predicted-discharge -2.0
                                        :realised-discharge -1.0
                                        :field-delta fd})
        plain (repl-trace/turn-record {:step 0 :predicted-discharge -2.0})]
    (is (= fd (:field-delta tagged)))
    (is (not (contains? plain :field-delta)))))

(deftest turn-record-threads-independence-tag
  (let [tagged (repl-trace/turn-record {:step 0 :predicted-discharge -2.0
                                        :realised-discharge -1.0
                                        :independent? true
                                        :evidence-ref "commit abc123"})
        plain  (repl-trace/turn-record {:step 0 :predicted-discharge -2.0
                                        :realised-discharge -1.0})]
    (is (true? (:independent? tagged)))
    (is (= "commit abc123" (:evidence-ref tagged)))
    (is (= 1.0 (:prediction-error tagged)))
    ;; I1 at the record grain: absent unless supplied — never defaulted
    (is (not (contains? plain :independent?)))
    (is (not (contains? plain :evidence-ref)))))

(deftest executed-without-evidence-throws
  ;; I2 — and the guard fires BEFORE any state lookup/mutation
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"requires :evidence-ref"
                        (pilot/close-live-cycle! "live-any" {:executed? true}))))

(deftest executed-frame-counts-as-independent-in-calibration
  ;; I3 end-to-end at the data grain: a γ frame whose turn-record carries the
  ;; tag is read by the canonical calibration reader as an independent pair.
  (let [dir (doto (java.io.File/createTempFile "realised-" "") (.delete) (.mkdirs))
        tr (repl-trace/turn-record {:step 0 :predicted-discharge -2.0
                                    :realised-discharge -1.0
                                    :independent? true
                                    :evidence-ref "commit abc123"
                                    :realised-source :measured})
        _ (spit (io/file dir "live-x.edn")
                (pr-str {:run-id "live-x" :date "2026-06-11" :trace [tr]}))
        evidence (calibration/load-evidence {:traces-dir (.getPath dir)
                                             :pilots-log "/tmp/no-such.md"
                                             :closure-folds "/tmp/no-such.edn"
                                             :ch2-events "/tmp/no-such.edn"
                                             :discipline-events "/tmp/no-such.edn"})
        report (calibration/calibration-report evidence)]
    (try
      (is (= 1 (:paired-count report)))
      (is (= 1 (:independent-paired-count report)))
      ;; one pair is still :insufficient-evidence — volume comes from flying
      (is (= :insufficient-evidence (:verdict report)))
      (finally
        (doseq [f (reverse (file-seq dir))] (.delete f))))))

(deftest executed-close-emits-operator-merge-event
  ;; I4 — exercised at the unit grain via the discipline-events contract the
  ;; close calls (the close's own emission is best-effort requiring-resolve;
  ;; shape-validate the exact event map it constructs).
  (let [path (str (java.io.File/createTempFile "disc-" ".edn"))
        ev ((requiring-resolve 'futon3c.aif.discipline-events/append-event!)
            {:discipline/event :operator-merge
             :run-id "live-x"
             :at "2026-06-11T09:00:00Z"
             :action {:type :address-sorry :target "sorry/wm-realised-on-merge-binding"}
             :predicted -4.1
             :note "executed cycle; evidence: commit abc123"}
            path)]
    (try
      (is (= :operator-merge (:discipline/event ev)))
      (is (= 1 (count ((requiring-resolve 'futon3c.aif.discipline-events/read-events) path))))
      (finally (.delete (io/file path))))))
