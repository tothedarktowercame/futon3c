(ns futon3c.diagramprover.wm-wire-plan-support
  "Hermetic plan-mode calls for WIRE-LANE-1B-I. No flight or click runs.
  The scheduled entry's writer is its exact trigger-from-env helper; -main
  itself exits the JVM. Test boxes have no reader var: use their literal
  keyword reads on a real select result, as outer_cascade_test.clj does."
  (:require [clojure.test :refer [is]]
            [futon2.aif.flight-driver :as driver]
            [futon2.aif.flight-runner :as runner]
            [futon2.aif.outer-cascade :as cascade]
            [futon2.aif.outer-loop :as outer]
            [futon3c.diagramprover.wm-wire :as w]
            [wm-scheduled-run :as scheduled]))

(def field
  {:considered [{:target "M-a" :kind :mission :repo "futon2" :path "M-a.md"}
                {:target "M-b" :kind :mission :repo "futon2" :path "M-b.md"}]
   :feasible [{:target "M-a" :kind :mission :eligible true :next-step :ready}
              {:target "M-b" :kind :mission :eligible true :next-step :read-criteria}]
   :exclusions []})

(defn plan-opts []
  {:repo "futon2" :path "M-a.md" :seat "fixture-seat" :id "wire-plan"
   :code-root "/nonexistent" :store (w/tmp-dir "wire-plan-store-")
   :sources {} :read-text (fn [& _] "# Mission\n\nStatus: MAP\n\n## MAP\n\nA survey is pending.\n")
   :observe (constantly false)})

(defn- read-plan [r field]
  (if (w/typed-absence? r) r
      (get-in r [:placement (if (= field :chosen-target) :target field)])))

(defn- refusal [e]
  (if (= :target (:missing (ex-data e))) {:absent :missing-target}
      (throw e)))

(defn observe
  "Run the writer and reader for FIELD. TAMPER changes the carrier before
  the real reader receives it; never edits the observed reader result.
  The loop case wraps select to capture and forward its real result, then
  observes the real plan-from-field! output. External IO ports alone are
  stubbed: text/observations, field load, and the serving-displacement read."
  [reader field-key tamper]
  (with-redefs [runner/latest-displacement (fn [& _] {:absent :hermetic-no-serving-jvm})]
    (let [opts (plan-opts)
          select-real cascade/select
          written (atom nil)
          r (try
              (case reader
                :loop
                (with-redefs [cascade/select
                              (fn [input]
                                (let [s (select-real input)]
                                  (reset! written (get s field-key))
                                  (tamper s)))]
                  (read-plan
                    (:plan (outer/plan-from-field!
                             {:seed 42 :trigger :wallclock-cron :seat "fixture-seat"
                              :load-field-fn (fn [] {:field field :opts opts})
                              :plan-opts (select-keys opts [:id :read-text :observe])})) field-key))
                :trigger
                (let [v (scheduled/trigger-from-env (constantly "wallclock-cron"))
                      _ (reset! written v)
                      r (outer/plan-from-field!
                          (merge {:seed 42 :seat "fixture-seat"
                                  :load-field-fn (fn [] {:field field :opts opts})
                                  :plan-opts (select-keys opts [:id :read-text :observe])}
                                 (tamper {:trigger v})))]
                  (get-in r [:target-selection :trigger]))
                (let [s (select-real {:field field :seed 42 :trigger :wallclock-cron})
                      _ (reset! written (get s field-key))
                      handed (tamper s)]
                  (case reader
                    :entry (get (driver/resolve-target handed) field-key)
                    :plan (read-plan (driver/plan (merge opts handed)) field-key)
                    :test (get handed field-key))))
              (catch clojure.lang.ExceptionInfo e (refusal e)))]
      {:writer @written :reader r})))

(def live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn"
    :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
    :why "hand-placed flight: no selector output, no chosen-target/draw-seed/target-selection"}
   {:path "holes/labs/M-wm-wiring/spike/plan-before-run.edn"
    :sha256 "010541c497d4108d0ed6d35dc1d944cbfa83e1f2b6d5c77355f9e624696ca083"
    :why "hand-placed plan: no selector output or scheduled-entry trigger"}])

(defn assert-live-records
  "Assert each pin BEFORE parsing the record; stop on a moved pin. Survey:
  spike EDN contained no :chosen-target/:draw-seed/:target-selection.
  The requested futon2/holes/labs/M-futon-seams/exemplar directory is absent;
  futon3c's exemplar is hand-authored and supplies no selector witness."
  []
  (doseq [{:keys [path sha256]} live-records-read]
    (let [actual (w/sha256-file path)]
      (is (= sha256 actual) path)
      (when-not (= sha256 actual) (throw (ex-info "live record pin moved" {:path path})))
      (let [r (w/read-record path)]
        (is (not-any? #(and (map? %) (some (set (keys %))
                                         [:chosen-target :draw-seed :target-selection]))
                      (tree-seq coll? seq r)) path)))))
