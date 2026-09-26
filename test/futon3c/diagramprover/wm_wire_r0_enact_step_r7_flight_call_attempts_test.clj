(ns futon3c.diagramprover.wm-wire-r0-enact-step-r7-flight-call-attempts-test
  "Wire [:r0-enact-step :r7-flight-call :attempts]: the enactment step's
  attempts reaching the flight's W_c call.

  The writer is flight-runner/enact-fn (:r0-enact-step's site,
  flight_runner.clj:901): `:attempts attempts` on the enactment record.
  The reader is flight-runner/wc-verdict-fn (:r7-flight-call,
  flight_runner.clj:948): its default :identity-fn reads
  `:shown (mapv :pattern (:attempts e))` into the policy key it hands to
  enactment-habit/increment — the reader's produced value under the field
  is the shown pattern list of that policy key.

  No live record carries both ends (live-records-read): no spike record
  carries :attempts at all, and the M-futon-seams exemplar enactment is
  hand-authored, not enact-fn's :wm/enactment-v1 output. So the wire is
  WITNESSED-HERMETICALLY: wm-wire-enact-driver/enact-wc drives the REAL
  enact-fn into a real enactment of click-001's :cand/a-registry-first,
  and the REAL wc-verdict-fn runs on it (the real checker over the record
  file, exit 0) with the increment seam captured. The bad cases tamper
  :attempts on the enactment at wc-verdict-fn's door: absent, and a
  different pattern list."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.flight-runner :as fr]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-enact-driver :as d]))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "its one enactment is {:absent :no-dispatch-configured}; the census finds :attempts in no spike record (all fourteen checked), so no live flight ever carried the writer's end"}
     {:path (:path d/click-001-enactment)
      :sha256 (:sha256 d/click-001-enactment)
      :why "carries :attempts, but hand-authored (claude-10, 2026-09-24): schema :m-futon-seams/proof2a-enactment-v1, not enact-fn's :wm/enactment-v1; its :attempts were not written by the writer"}
     {:path (:path d/click-001)
      :sha256 (:sha256 d/click-001)
      :why "the click record the checker reads here; carries no selection-law :candidate, so the live join would be the typed :join-unverifiable"}]))

(defn observe
  "Drive the real writer (enact-fn via enact-wc), then call the real
   wc-verdict-fn on the enactment — TAMPERed at the reader's door — with
   the increment seam captured. The writer's value is the patterns of the
   attempts enact-fn wrote; the reader's is the policy key's :shown."
  ([] (observe identity))
  ([tamper]
   (let [{:keys [enactment record-path]} (d/enact-wc)
         captured (atom nil)
         f (fr/wc-verdict-fn {:checker d/checker-path
                              :click-record-path (constantly (:path d/click-001))
                              :increment! (fn [_e identity verdict]
                                            (reset! captured {:identity identity :verdict verdict})
                                            :increment-captured)})
         out (f {:target "M-t"} {:enactment (tamper enactment)
                                 :record-path record-path})]
     {:writer (mapv :pattern (:attempts enactment))
      :reader (nth (:identity @captured) 2)
      :wc (:wc out) :verdict (:verdict @captured)})))

(defn check [] (observe))

(def wire
  {:wire [:r0-enact-step :r7-flight-call :attempts]
   :kind :witnessed-hermetically
   :test `the-attempts-reach-the-flights-wc-call
   :check check
   :live-records-read live-records-read})

(deftest the-attempts-reach-the-flights-wc-call
  (let [o (check)]
    (is (= 7 (count (:writer o))))
    (is (= d/wc-precedence (:writer o))
        "enact-fn wrote one attempt per candidate pattern, in order")
    (is (= {:status :join-unverifiable :failures []}
           (select-keys (get-in o [:wc :verdict]) [:status :failures]))
        "the real checker ran on the record file (exit 0)")
    (is (w/received? o))))

(deftest absent-attempts-at-the-reader-fail-the-wire
  (let [o (observe #(assoc % :attempts nil))]
    (is (= [] (:reader o))
        "no default pattern list is substituted: :shown is empty")
    (is (not (w/received? o)))))

(deftest different-attempts-at-the-reader-fail-the-wire
  (let [o (observe #(assoc % :attempts [{:pattern :p/x}]))]
    (is (= [:p/x] (:reader o))
        "the policy key's :shown moves with what wc-verdict-fn reads")
    (is (not (w/received? o)))))
