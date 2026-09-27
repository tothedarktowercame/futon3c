(ns futon3c.diagramprover.wm-wire-fold-out-support
  "Judge handoffs: observe arguments at real readers, alter the carrier, and
  inspect reader-produced values. External input ports are hermetic."
  (:require [clojure.edn :as edn] [clojure.java.io :as io]
            [futon2.report.war-machine :as wm]
            [futon2.report.cascade-decision-test :as fixture]
            [futon2.aif.locator-fixtures :as loc]
            [futon2.aif.belief :as belief]
            [futon2.aif.free-energy :as fe]
            [futon2.aif.mission-registry :as mr]
            [futon2.aif.morning-brief :as brief]
            [futon2.aif.anticipation :as anticipation]
            [futon2.aif.sorry-registry :as sorry]
            [futon2.aif.ticket-queue :as tq]
            [futon2.aif.trace :as trace]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.enactment-habit :as habit]
            [futon2.aif.policy-prefix-admission :as prefix]
            [futon2.aif.scoring-input-receipts :as receipts]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-in-support :as fold-in]
            [futon3c.diagramprover.wm-wire-measured-support :as measured]))

(def live-records-read
  (mapv #(assoc % :why "No carried-mu-post, loop-belief, channel-prediction or conditioning-steps. In 278b6988, enactment-fold occurs only as source labels; habit-read receipts are :absent :no-enactment-fold and the decision records zero samples, not a nonempty handoff.")
        fold-in/live-records-read))

(defn read-pin [{:keys [path sha256]}]
  (assert (= sha256 (w/sha256-file path)))
  (w/read-record path))

(defn assert-live-pins []
  (doseq [pin live-records-read]
    (let [r (read-pin pin)]
      (assert (not-any? #(and (map? %) (some (partial contains? %) [:carried-mu-post :loop-belief :channel-prediction :conditioning-steps]))
                        (tree-seq coll? seq r))))))

(defn judge [root scan]
  (with-redefs [mr/load-missions (constantly {:missions []})
                mr/load-tickets (constantly {:tickets []})
                sorry/open-sorrys (constantly [])
                belief/section-ids-from-stack-annotations (constantly ["known"])
                brief/unseen-belief-events (constantly [])
                anticipation/anticipation-snapshot (constantly {:events-loaded? false})]
    (binding [belief/*carry-belief?* true]
      (wm/judge scan
                (merge fixture/live-c-opts
                       {:trace-dir root :step-portfolio? false :eval-invariant-fallback? false
                        :cascade-sources (loc/locate-all fixture/tick-1-sources)
                        :cascade-proposals-dir root :repair-obligations-root root
                        :machine-interpretations-dir root :ticket-queue tq/empty-declaration})))))

(defn simple [kind mutation]
  (let [root (w/tmp-dir "fold-output-") fresh (belief/initial-belief-state ["known"])
        carried (wm/apply-arena-belief-events fresh fold-in/events)
        other (wm/apply-arena-belief-events fresh [(assoc (first fold-in/events) :type :foreclosed)])
        captured (atom []) carry belief/reconcile-belief-carry
        apply-events wm/apply-arena-belief-events prediction fe/channel-prediction-error]
    (try
      (when (#{:carry :belief} kind)
        (trace/write-trace! {:run/id "previous-offline" :belief carried} :dir root :date-str "2026-09-26"))
      (let [result
            (with-redefs
              [belief/reconcile-belief-carry
               (fn [bootstrap value]
                 (if (= kind :carry)
                   (let [r (carry bootstrap (case mutation :none value :absent nil :different other))]
                     (swap! captured conj {:writer value :reader r}) r)
                   (carry bootstrap value)))
               wm/apply-arena-belief-events
               (fn [value events]
                 (if (= kind :belief)
                   (let [changed (case mutation :none value :absent nil :different other)
                         r (apply-events changed events)]
                     ;; Transform the writer's input with the public filter
                     ;; contract, and compare to the reader's actual posterior.
                     (swap! captured conj
                            {:writer (belief/update-belief-batch value events (wm/arena-belief-update-opts))
                             :reader r :input value :events events}) r)
                   (apply-events value events)))
               fe/channel-prediction-error
               (fn [obs channel value & opts]
                 (if (and (= kind :prediction) (= channel :annotation-health))
                   (let [changed (case mutation :none value :absent {:absent :not-carried}
                                       :different (update value :mean #(- 1.0 %)))
                         r (prediction obs channel changed (or (first opts) {}))]
                     (swap! captured conj
                            {:writer (select-keys value [:mean :variance])
                             :reader (when (= :present (:status r))
                                       {:mean (:predicted-mean r) :variance (:predicted-variance r)})
                             :error r}) r)
                   (prediction obs channel value (or (first opts) {}))))]
              (judge root (if (= kind :carry) {} {:annotation-graph {:health 0.9}})))]
        (assoc (first @captured) :result result :fresh fresh))
      (finally (measured/cleanup root)))))

(defn write-flight! [root tick-file record]
  (let [in (measured/inputs record) target (:target in) checked (get-in in [:observation :checked])
        chosen (get-in record [:decision :chosen])
        key (prefix/candidate-key {:target target :precedence (:precedence chosen)})
        f (flight/start {:target target :chosen-because {:kind :requested}}
                        {:kind :operator-declared :wants (vec checked) :declared-by "wire-test"}
                        {:id "offline-fold-output"})
        n (atom 0)
        enact (fr/enact-fn {:dispatch-step! (fn [_] {:commit "offline-fixture" :produced (first checked) :check {:class :C3}})
                            :check-fn (constantly {:observed true})})
        r (flight/run! f {:sources-fn (constantly {}) :max-clicks 1
                          :click-fn (fn [_] {:click-id "offline-fold-output" :chosen chosen})
                          :enact-fn enact
                          :wc-fn (fn [_ enacted]
                                   {:wc {:verdict []}
                                    :increment (habit/increment (:enactment enacted) key [])})
                          :observe-fn (fn [& _] (zipmap checked (repeat (> (swap! n inc) 1))))
                          :fetch-run-record (fn [_] (edn/read-string (slurp tick-file)))})
        path (io/file root "flights" "offline.edn")]
    (assert (= :present (get-in r [:enactments 0 :step :status]))
            (pr-str {:step (get-in r [:enactments 0 :step]) :measured-a (get-in record [:decision :measured-a])}))
    (assert (= 1 (get-in r [:enactments 0 :increment :delta])))
    (io/make-parents path) (spit path (pr-str {:flight r}))
    r))

(defn decision [kind mutation]
  (measured/with-tick
    (fn [root file record]
      (let [f (write-flight! root file record)
            real @#'wm/cascade-decision-admitted
            seen (atom nil) habit-reads (atom [])
            result (binding [receipts/*habit-reads* habit-reads]
                     (with-redefs-fn
                       {#'wm/cascade-decision-admitted
                        (fn [assembled opts]
                          (let [value (get opts kind)
                                changed (case mutation
                                          :none value
                                          :absent nil
                                          :different
                                          (if (= kind :conditioning-steps)
                                            (update-in value [:steps 0 :step :f] inc)
                                            (habit/fold value [(assoc (first (vals (:enactment-records value)))
                                                                      :record-id ["offline-different" :C2])])))
                                r (real assembled (assoc opts kind changed))]
                            (reset! seen {:value value :result r}) r))}
                       #(judge root {})))
            value (:value @seen)
            prefixes (get-in result [:decision :selection-certificate :token-belief-input :policy-prefixes])
            admitted (first (filter #(= :admitted (:conditioning-status %)) (vals prefixes)))
            read (first (filter #(= :joint-selection (:purpose %)) @habit-reads))]
        {:writer (if (= kind :conditioning-steps)
                   (get-in value [:steps 0 :step])
                   {:records (count (:enactment-records value)) :samples (:samples value)})
         :reader (if (= kind :conditioning-steps)
                   (some-> admitted :observation-updates first (dissoc :source))
                   (select-keys (get-in result [:decision :selection-law :e-source]) [:records :samples]))
         :result result :prefixes prefixes :habit-reads @habit-reads :read read :flight f}))))
