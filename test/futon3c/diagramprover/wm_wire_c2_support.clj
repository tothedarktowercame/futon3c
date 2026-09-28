(ns futon3c.diagramprover.wm-wire-c2-support
  "Shared hermetic drivers for PROOF-2a-PLAN <2>3 lane C2 (E-kimi-task-74):

  1. chosen precedence into enact-fn: the REAL full-loop-runner/chosen-summary
     over a real select-action-cascades decision (wm-wire-selection-out-support's
     setup), through the REAL flight-runner/enact-fn with a recording
     :dispatch-step!.
  2./3. sourced-rates :status / :rates into measured-a-version: the REAL
     observation-rates/sourced-rates over ten admitted :C4 labels reaching the
     REAL war-machine/measured-a-version, exactly the call
     flight-conditioning-step-test's produced-measured-a makes, the producer's
     return tampered at the reader's door by with-redefs around the real var.
  4./5. the two decision-refusal test boxes, driven by the real writer: the new
     futon2 deftests (WIRE-23-C2, futon2 7a3b61eda) whose judge-fn calls the
     REAL war-machine/cascade-decision so the refusal is thrown inside the
     tick; this side wraps the writer var (tampering the thrown ex-data's
     field) and reads the value the test consumed from its clojure.test
     report, wm-wire-kernel-out-support/order-read's technique."
  (:require [clojure.test :as t]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.gate-refusal-abstention-test :as gate-test]
            [futon2.aif.judge-refusal-abstention-test :as judge-test]
            [futon2.aif.observation-rates :as rates]
            [futon2.report.war-machine :as wm] [futon2.aif.wm.cascade-decision :as wm-cd]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-selection-out-support :as selout]))

;; ---------------------------------------------------------------------------
;; Live-record pins. Census (kimi-4, 2026-09-27): of the 14 spike + 9 exemplar
;; EDNs, [:chosen :precedence] appears only on
;; tick-run-record-2026-09-26-flight-278b6988-click-1.edn (writer end of wire 1
;; live) and no record carries an enact-fn :attempts reader end (the only
;; :attempts carriers are the hand-authored M-futon-seams exemplar enactments).
;; No record carries :measured-a at all: sourced-rates' return is not persisted
;; and neither is measured-a-version's product (the requisition's expectation
;; that tick records carry [:decision :measured-a :rates] does not hold of
;; these records). The refusal test boxes consume no record.
;; ---------------------------------------------------------------------------

(def precedence-live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "Carries the writer end [:chosen :precedence] (7 patterns), but no record carries the reader end: enact-fn's :attempts are absent from every spike flight record (enactment recorded as a typed absence)."}
   {:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn"
    :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
    :why "The same click's flight record: no :attempts anywhere; the reader's produced value is not persisted live."}])

(def measured-live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "No :measured-a, :rates or sourced-rates return: the producer's value is not persisted apart from the reader's, and the reader's product is absent too."}
   {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-7f89646a-click-1.edn"
    :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
    :why "Same: no :measured-a on this tick record; neither end of the sourced-rates -> measured-a-version wires persists live."}])

(def refusal-live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "The reader is a test box (:box/kind :test): no record carries a test's consumption. The writer's refusal :kind appears live only under [:decision :abstention], written by the runner from the thrown ex-data."}
   {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-7f89646a-click-1.edn"
    :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
    :why "Same: its abstention kind is the runner's record of the refusal, not the test box's read."}])

(defn assert-live-records
  [pins absent-key]
  (doseq [{:keys [path sha256]} pins]
    (t/is (= sha256 (w/sha256-file path)) (str "live pin moved: " path))
    (let [r (w/read-record path)]
      (t/is (not-any? #(and (map? %) (contains? % absent-key))
                      (tree-seq coll? seq r))
            (str path " still lacks " absent-key)))))

;; ---------------------------------------------------------------------------
;; Wire 1: [:run-chosen-summary :r0-enact-step [:precedence {:record :chosen}]]
;; ---------------------------------------------------------------------------

(defn chosen-precedence
  "The REAL chosen-summary over the real decision of
  wm-wire-selection-out-support (its setup pins the click-001 exemplar by
  sha256 and selects with the real select-action-cascades), its :precedence
  tampered per MUTATION, through the REAL flight-runner/enact-fn with a
  recording :dispatch-step!. The reader's produced values: the patterns
  enact-fn dispatched (:attempted) and the enactment's (mapv :pattern
  (:attempts e)) (:reader)."
  [mutation]
  (let [{:keys [decision]} @selout/produced
        summary (runner/chosen-summary decision)
        value (:precedence summary)
        changed (case mutation
                  :none value
                  :absent nil
                  :different [:different/pattern-a :different/pattern-b])
        chosen (if (nil? changed)
                 (dissoc summary :precedence)
                 (assoc summary :precedence changed))
        seen (atom [])
        r ((fr/enact-fn {:dispatch-step! (fn [s]
                                           (swap! seen conj (:pattern s))
                                           {:failed {:reason :offline}})})
           {:target "offline" :flight/id "offline"}
           {:click-id "offline" :chosen chosen})]
    {:writer value
     :reader (mapv :pattern (:attempts (:enactment r)))
     :attempted @seen
     :result r}))

;; ---------------------------------------------------------------------------
;; Wires 2/3: [:r6-sourced-rates :r9-measured-a-version [:status|:rates {:record :sourced-rates}]]
;; ---------------------------------------------------------------------------

(def measured-target "M-f1b-join")

(defn- c4-labels
  "flight-conditioning-step-test's labels shape: ten admitted :C4 subjects,
  false-neg 1/10, false-pos 1/5."
  []
  (vec (concat (for [i (range 10)] {:token-class :C4 :admitted :present :recorded (>= i 1)})
               (for [i (range 5)] {:token-class :C4 :admitted :absent :recorded (< i 1)}))))

(defn measured
  "Drive the REAL war-machine/measured-a-version over one located problem,
  with the REAL observation-rates/sourced-rates wrapped: capture the
  producer's return (:producer) and tamper FIELD (:status or :rates) per
  MUTATION before the reader sees it. The reader's produced value (:reader):
  for :status, :sourced when the record is written (a :sourced producer
  status IS the record) else the absence map's :status; for :rates, the
  qualified rate at [target :t]."
  [field mutation]
  (let [labels (c4-labels)
        real @#'rates/sourced-rates
        written (atom nil)
        result
        (with-redefs [rates/sourced-rates
                      (fn [& args]
                        (let [v (apply real args)]
                          (reset! written v)
                          (case mutation
                            :none v
                            :absent (dissoc v field)
                            :different (case field
                                         :status (assoc v :status :sourcing-refused-differently)
                                         :rates (assoc-in v [:rates :t :false-neg] 1/2)))))]
          (wm-cd/measured-a-version
           [{:target measured-target :cascade-problem {:locators {:t {:class :C4}}}}]
           {measured-target {:labels labels
                             :subjects (frequencies (map :token-class labels))}}))]
    {:writer (case field
               :status (:status @written)
               :rates (get-in @written [:rates :t]))
     :reader (case field
               :status (if (= :wm/measured-a-v1 (:schema result)) :sourced (:status result))
               :rates (get (:rates result) [measured-target :t]))
     :producer @written
     :result result}))

;; ---------------------------------------------------------------------------
;; Wires 4/5: [:r9-decision :gate-refusal-test|:r9-judge-refusal-test :kind]
;; ---------------------------------------------------------------------------

(defn refusal-box
  "Run the named futon2 refusal-box test (its judge-fn calls the REAL
  war-machine/cascade-decision, so the refusal is thrown inside the tick).
  These vars formerly ran in fresh futon2-cwd JVMs because their require chain
  read fixtures relative to the process cwd. Those test fixtures now resolve
  as classpath resources, making it safe to run the real vars in this JVM.
  The writer var is wrapped with the thrown ex-data's field tampered per
  MUTATION (:kind for the judge refusal, :reason for the gate's inadmissible
  decision -- the abstention's :kind is the runner's record of that field),
  and the value the box consumed is read from its clojure.test report
  (order-read's technique). Returns {:writer :reader :report-type}."
  [which mutation]
  (let [field (if (= which :judge) :kind :reason)
        test-var (if (= which :judge)
                   #'judge-test/the-real-judge-refusal-is-the-ticks-typed-abstention
                   #'gate-test/the-real-gate-refusal-is-the-ticks-typed-abstention)
        real wm-cd/cascade-decision
        written (atom nil)
        reports (atom [])]
    (with-redefs [wm-cd/cascade-decision
                  (fn [& args]
                    (try
                      (apply real args)
                      (catch clojure.lang.ExceptionInfo e
                        (reset! written (get (ex-data e) field))
                        (throw (ex-info (ex-message e)
                                        (case mutation
                                          :none (ex-data e)
                                          :absent (dissoc (ex-data e) field)
                                          :different (assoc (ex-data e) field
                                                              :different-refusal-kind)))))))
                  t/report (fn [m] (swap! reports conj m))]
      (test-var))
    (let [report (first (filter #(#{:pass :fail} (:type %)) @reports))
          form (:actual report)
          equality (if (= 'not (first form)) (second form) form)]
      {:writer @written
       :reader (last equality)
       :report-type (:type report)})))
