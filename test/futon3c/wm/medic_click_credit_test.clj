(ns futon3c.wm.medic-click-credit-test
  "Joe, 2026-10-31, stated twice: medic clicks do not take credits.

  Tests the refund at the point the runtime disposition is recorded -- the
  real completion path (close-click! calls result-summary, which both
  persists the binding and records :runtime-disposition) -- with a temp
  ordinary-click ledger. Fixtures are the producer shapes of
  futon2.aif.meta-self-heal-production/open-out-of-band-route!: the
  discharged leg carries {:status :discharged :route :open-out-of-band-medic
  ...} (:1857) and every held leg carries {:status :held :phase
  :open-out-of-band ...} (:1852-1870)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.wm.ordinary-click-budget :as budget]
            [futon3c.wm.runner-service :as service]))

(defn- temp-dir [child]
  (str (java.nio.file.Files/createTempDirectory
        child (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- ledger-lines [path]
  (when (.exists (io/file path))
    (remove str/blank? (str/split-lines (slurp path)))))

(def discharged-result
  {:status :discharged :route :open-out-of-band-medic
   :injury-id "inj-1" :fixed-by (apply str (repeat 40 "a"))
   :out-of-band-receipt-pin {:path "/tmp/x.edn" :sha256 "ff"}
   :ordinary-selection-called? false})

(def held-result
  {:status :held :reason :out-of-band/acceptance-run-unavailable
   :phase :open-out-of-band
   :out-of-band-receipt-pin {:path "/tmp/x.edn" :sha256 "ff"}
   :ordinary-selection-called? false})

(def ordinary-result
  {:outcome :grounded-change :run/id "r" :status :ran})

(defn- run-completion
  "Call the real completion-path summary with temp binding dirs."
  [root click-id result]
  (binding [service/*click-run-binding-dir* (str root "/bindings")
            service/*run-record-dir* (str root "/runs")
            service/*run4-terminal-projection-dir* (str root "/terminal")
            service/*run4-historical-projection-dir* (str root "/historical")]
    (#'service/result-summary click-id result "attempt-1")))

(deftest medic-discharge-refunds-once-through-completion-path
  (let [root (temp-dir "medic-credit")
        ledger (str root "/ledger.jsonl")]
    (binding [budget/*ledger-path* ledger]
      (budget/consume! "click-med" "2026-10-31T00:00:00Z" "test")
      (is (= 1 (:consumed (budget/availability))))
      (let [summary (run-completion root "click-med" discharged-result)]
        (is (= :refunded (get-in summary [:medic-refund :status])))
        (is (= 0 (:consumed (budget/availability))))
        ;; exactly one refund line, exactly-once on the second call
        (is (= 1 (count (filter #(str/includes? % "\"event\":\"refund\"")
                                (ledger-lines ledger)))))
        (let [again (run-completion root "click-med" discharged-result)]
          (is (= :already-refunded (get-in again [:medic-refund :status])))
          (is (= 1 (count (filter #(str/includes? % "\"event\":\"refund\"")
                                  (ledger-lines ledger))))))))))

(deftest held-medic-refunds-through-completion-path
  (let [root (temp-dir "medic-credit-held")
        ledger (str root "/ledger.jsonl")]
    (binding [budget/*ledger-path* ledger]
      (budget/consume! "click-held" "2026-10-31T00:00:00Z" "test")
      (let [summary (run-completion root "click-held" held-result)]
        (is (= :refunded (get-in summary [:medic-refund :status])))
        (is (= 0 (:consumed (budget/availability))))))))

(deftest ordinary-click-takes-no-refund
  (let [root (temp-dir "medic-credit-ordinary")
        ledger (str root "/ledger.jsonl")]
    (binding [budget/*ledger-path* ledger]
      (budget/consume! "click-ord" "2026-10-31T00:00:00Z" "test")
      (let [summary (run-completion root "click-ord" ordinary-result)]
        (is (nil? (:medic-refund summary)))
        (is (= 1 (:consumed (budget/availability))))
        (is (= 0 (count (filter #(str/includes? % "\"event\":\"refund\"")
                                (ledger-lines ledger)))))))))

(deftest rescue-issues-and-refunds-at-zero-credit
  (let [root (temp-dir "medic-credit-rescue")
        ledger (str root "/ledger.jsonl")]
    (binding [budget/*ledger-path* ledger]
      ;; exhaust the ration with ordinary charges
      (dotimes [i budget/allocated]
        (budget/consume! (str "click-fill-" i) "2026-10-31T00:00:00Z" "test"))
      (is (= 0 (:available (budget/availability))))
      ;; an ordinary issue still refuses
      (is (thrown-with-msg? Throwable #"budget exhausted"
                            (budget/consume! "click-nope" "2026-10-31T00:00:00Z" "test")))
      ;; the medic rescue issues past exhaustion and refunds immediately
      (let [rescue (#'service/medic-rescue-issue!
                    "click-rescue" "2026-10-31T00:00:00Z" "test")]
        (is (= :refunded (get-in rescue [:refund :status])))
        (is (= 0 (:available (budget/availability))))
        ;; the pair is in the ledger verbatim
        (is (= 1 (count (filter #(and (str/includes? % "click-rescue")
                                      (str/includes? % "\"event\":\"refund\"")
                                      (str/includes? % "medic rescue"))
                                (ledger-lines ledger)))))))))

(deftest rescue-admissibility-fails-closed-without-stores
  (let [root (temp-dir "medic-credit-admiss")]
    ;; no ledger, no acceptance root: an ordinary click's exhaustion refusal
    ;; must stand, so admissibility is false
    (is (false? (service/medic-rescue-admissible?
                 {:meta-injury-ledger-path (str root "/absent-ledger.edn")
                  :meta-acceptance-root (str root "/absent-acceptances")})))))
