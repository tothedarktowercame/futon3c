(ns futon3c.diagramprover.wm-wire-rates-support
  "Real rate writers/readers, with wrappers that forward to the original vars.
  R6 test boxes have no var: reproduce click_measurement_test's provenance read.
  F-pi is exercised by calling the kernel with its non-prefix option, because
  the lane normally selects prefix scoring. No flight or click is executed."
  (:require [clojure.test :refer [is]]
            [futon2.aif.observation-admission :as admission]
            [futon2.aif.observation-rates :as rates]
            [futon2.aif.efe :as efe]
            [futon2.aif.cascade-free-energy :as fe]
            [futon2.report.war-machine :as wm]
            [futon3c.diagramprover.wm-wire :as w]))

(def problem
  {:facts {:t/observed true :t/other false} :want [:t/wanted]
   :interpretations {:p/appears {:guard {:needs #{:t/observed} :forbids #{}}
                                :produces #{:t/wanted}}}
   :repository {:patterns #{:p/appears} :stands-on #{}}
   :precedences [[:p/appears]] :horizon-steps 2
   :cascade-spec {:want #{:t/wanted}} :beta 1
   :locators (zipmap [:t/observed :t/other :t/wanted] (repeat {:class :C3}))})

(defn labels []
  (mapv (fn [[finding recorded]]
          (let [subject {:token (name finding) :token-class :C3
                         :application "Hermetic admission for a rate cell"
                         :evidence-pointers [{:repo "fixture" :sha "fixture" :path (name finding)}]
                         :author :none :enactor :none :recorded-verdict recorded
                         :check-mechanism "declared-check" :check-cutoff {:fixture "fixture"}}
                adj (admission/adjudication "recompute" (admission/observer-view subject)
                                            finding {:fixture "fixture"})
                review (admission/mechanical-review "independent" subject adj)
                admitted (admission/admit subject adj review)
                label (admission/label-record subject admitted)]
            (when-not label (throw (ex-info "Fixture admission refused" admitted)))
            label))
        [[:present true] [:absent false]]))

(defn lane []
  (wm/cascade-lane problem {:through :R5
                            :observation-labels {:labels (labels) :subjects {:C3 2}}}))

(defn measurement [lane-result]
  (get-in (first (:ranked lane-result)) [:certificate :rates-provenance :measurement]))

(defn observe
  "TAMPER alters the carrier before the real reader receives it.
  Values returned here come from actual producer calls and reader outputs;
  an invalid rate carrier may be refused, in which case :reader is nil."
  [wire-kind tamper]
  (let [source rates/sourced-rates kernel efe/rank-cascade-actions energy fe/policy-free-energy
        written (atom nil) read-value (atom nil) kernel-args (atom nil)]
    (case wire-kind
      (:measurement :test-measurement :sourced-rates)
      (let [result (with-redefs [rates/sourced-rates
                                (fn [& args]
                                  (let [r (apply source args)
                                        k (if (= wire-kind :sourced-rates) :rates :measurement)]
                                    (reset! written (get r k))
                                    (update r k tamper)))]
                     (lane))]
        {:writer @written
         :reader (if (= wire-kind :sourced-rates)
                   (get-in (meta (:ranked result)) [:cascade-scoring :precision-model :rates])
                   (measurement result))})

      :adjudication-rates
      (do (with-redefs [efe/rank-cascade-actions
                       (fn [state candidates opts]
                         (reset! written (:adjudication-rates opts))
                         (let [r (kernel state candidates (update opts :adjudication-rates tamper))]
                           (reset! read-value (get-in (meta r) [:cascade-scoring :precision-model :rates]))
                           r))]
            (lane))
          {:writer @written :reader @read-value})

      :fpi-rates
      (do
        (with-redefs [efe/rank-cascade-actions
                      (fn [& args] (reset! kernel-args args) (apply kernel args))]
          (lane))
        (let [[state candidates opts] @kernel-args]
          (with-redefs [fe/policy-free-energy
                        (fn [declared]
                          (reset! written (:rates declared))
                          (let [r (energy (update declared :rates tamper))]
                            (reset! read-value (get-in r [:params :rates]))
                            r))]
            (kernel state candidates (dissoc opts :f-prefix-production?))))
        {:writer @written :reader @read-value}))))

(defn different [kind value]
  (if (#{:measurement :test-measurement} kind)
    (update-in value [:t/wanted :false-neg :numerator] inc)
    (into {} (map (fn [[t _]] [t {:false-neg 1/4 :false-pos 1/4}])) value)))

(def live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn"
    :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
    :why "Flight wrapper has no sourced measurement or kernel rates record."}
   {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "Tick record has no measurement map or adjudication-rates field; cannot recover both rate ends."}])

(defn assert-live-records []
  (doseq [{:keys [path sha256]} live-records-read]
    (let [actual (w/sha256-file path)]
      (is (= sha256 actual))
      (when-not (= sha256 actual) (throw (ex-info "Moved live pin" {:path path})))
      (let [r (w/read-record path)]
        (is (not-any? #(and (map? %) (or (contains? % :measurement)
                                        (contains? % :adjudication-rates)))
                      (tree-seq coll? seq r)))))))
