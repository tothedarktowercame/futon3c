(ns futon3c.diagramprover.wm-wire-r0-wc-grain-gate-test
  "Wire [:r0-enact-step :wc-checker :grain-gate]: the gate's result on the
  grain attempt reaching the W_c checker.

  The writer is flight-runner/enact-fn (:r0-enact-step's site), writing the
  gate's result on the grain attempt under :grain-gate. The reader is the
  W_c checker (proof2a_check.clj's gc-failures, X_c(d)): a gated attempt is
  one that (contains? % :grain-gate), and the checker re-runs the gate on
  the enactment's :grain against the attempt's :grain, comparing the
  re-run to the recorded (:grain-gate a) — a recorded pass is evidence only
  when the gate still passes on the files as they are, and a recorded
  refusal the re-run contradicts is echoed in the verdict
  (\"attempt N records G_c <value> but the gate passes on re-run\").

  No live record carries both ends (live-records-read), so the wire is
  WITNESSED-HERMETICALLY: enact-fn writes a real enactment record for
  click-001's :cand/a-registry-first with the grain attempt gated and
  passing, the checker runs on that record with --wc --edn, and its
  verdict carries no G_c discrepancy — the recorded (:grain-gate a) it
  read agreed with the re-run. The writer's value is the grain attempt's
  :grain-gate from the returned enactment; the reader's is the same field
  of the record file the checker was given. The bad cases put a value the
  writer did not write under :grain-gate and the checker's verdict echoes
  exactly that value."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-enact-driver :as d]))

(defn- with-grain-gate [r v]
  (update r :attempts (fn [as] (assoc-in (vec as) [0 :grain-gate] v))))

(defn observe
  "The writer's grain-attempt :grain-gate, the reader's (from the record
  file the checker is given), and the checker's verdict. TAMPER edits the
  record file before it is read and checked (the bad cases)."
  ([] (observe identity))
  ([tamper]
   (let [{:keys [enactment record-path]} (d/enact-wc)]
     (d/rewrite record-path tamper)
     (let [{:keys [exit verdict] :as r} (d/run-wc record-path)]
       {:writer (:grain-gate (first (:attempts enactment)))
        :reader (:grain-gate (first (:attempts (w/read-record record-path))))
        :exit exit :verdict verdict :err (:err r)}))))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "its one enactment is {:absent :no-dispatch-configured}: enact-fn never ran the gate, and no flight ever ran the checker on a real record"}
     {:path (:path d/click-001)
      :sha256 (:sha256 d/click-001)
      :why "the click record the checker reads here; hand-authored (claude-1)"}
     {:path (:path d/click-001-enactment)
      :sha256 (:sha256 d/click-001-enactment)
      :why "hand-authored (claude-10, 2026-09-24): no :grain-gate key anywhere (the checker's hand shape puts the gate under an attempt's :check instead); not enact-fn's output"}]))

(defn- wc-failures
  "The verdict's failures: the vector itself, or :failures of the typed
  join-unverifiable map."
  [verdict]
  (if (map? verdict) (:failures verdict) verdict))

(def wire
  {:wire [:r0-enact-step :wc-checker :grain-gate]
   :kind :witnessed-hermetically
   :test `the-gate-pass-reaches-the-wc-checker
   :check check
   :live-records-read live-records-read})

(deftest the-gate-pass-reaches-the-wc-checker
  (let [o (check)]
    (is (= {:status :pass} (:writer o)))
    (is (= 0 (:exit o)) (str (:err o)))
    (is (= [] (:failures (:verdict o)))
        "the checker re-ran the gate and the recorded value it read agreed")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [absence {:status :absent :reason :gate-not-run}
        o (observe #(with-grain-gate % absence))]
    (is (= absence (:reader o)))
    (is (= 0 (:exit o)))
    (is (some #(re-find #"records G_c \{:status :absent, :reason :gate-not-run\}" %)
              (wc-failures (:verdict o)))
        "the checker's verdict echoes the typed absence it read")
    (is (not (w/received? o)))))

(deftest a-recorded-refusal-the-gate-contradicts-fails-the-wire
  ;; the refusal is the gate's real answer to a grain mismatch (provider
  ;; grain planned against the role grain), not an invented constant
  (let [refusal (:grain-gate (first (:attempts (:enactment
                                                (d/enact {:candidate :cand/g :precedence d/r5-precedence
                                                          :interps (d/r5-interps)
                                                          :planned-grain (d/provider-grain)})))))
        o (observe #(with-grain-gate % refusal))]
    (is (= :grain-mismatch (:reason refusal)))
    (is (= refusal (:reader o)))
    (is (some #(re-find #"records G_c .* but the gate passes on re-run" %)
              (wc-failures (:verdict o)))
        (pr-str (:verdict o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-machine-grain-gate
  (doseq [{:keys [path sha256 why]} live-records-read]
    (is (= sha256 (w/sha256-file path)) why))
  (let [[f278 _clk ex] (map #(w/read-record (:path %)) live-records-read)]
    (is (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments (:flight f278)))))
    (is (not-any? #(contains? % :grain-gate) (:attempts ex)))))
