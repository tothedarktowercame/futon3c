(ns futon3c.aif.flight-record
  "Compose + persist a flight-as-derivation record (holes/specs/flight.spec.edn,
   :wm-flight v0.4) at close time — M-first-flights build-order step 2.

   The MAP finding this implements: the grounds exist at close time and were
   thrown away after the judgment was recorded. This ns persists them. The
   composition rule, from the seat-read and the witness flight:

   - what the APPARATUS computes (predicted, realised, error, source, the
     merge event, the gauge) composes into terms with their grounds;
   - what the PILOT supplies via the :flight opt (neighbourhood, warrant,
     verification, window, class, plan-sketch, steers, links) passes through;
   - what neither supplies becomes a TYPED SORRY — never a fabricated
     judgment. No class is guessed, no warrant invented, no verification
     asserted: a record missing a pilot judgment fails the verifier
     (scripts/flight_spec_verify.clj F2/F3/F5), and that failure is the
     honest state, not a bug.

   Pure (edn/io/pprint only) so it loads under bb and tests cheaply;
   war_machine_pilot.clj close-live-cycle! calls compose+write best-effort."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(def spec-version "0.4")

(defn- ghost
  ([kind] {:sorry {:kind kind}})
  ([kind blocked-by] {:sorry {:kind kind :blocked-by blocked-by}}))

(defn- g1 [g] (when (number? g) {:g g :g-grain :one-step-action}))

(defn compose-flight-record
  "Pure: begin-state + close-time computation + pilot-supplied :flight map
   -> a spec-shaped flight record. See ns docstring for the composition rule.

   ARGS: {:run-id :begin (the begin-live-cycle! map) :agent
          :predicted :predicted-constant :realised :realised-source
          :executed? :evidence-ref :merge-event {:type :at :note}
          :frame-path :logged-turn
          :flight {:neighbourhood :cascades :plan-sketch :warrant
                   :verification :window :window-ground :class
                   :act-verification :steers :links :counterfactual-note}}"
  [{:keys [run-id begin agent predicted predicted-constant realised
           realised-source executed? evidence-ref merge-event
           frame-path logged-turn flight]}]
  (let [{:keys [neighbourhood cascades plan-sketch warrant verification
                window window-ground class act-verification steers links]} flight
        v (get-in begin [:pre :v])
        dT-count (count (get-in begin [:pre :dT-snapshot]))
        begin-ref (str run-id ".begin.edn")
        events (vec (concat (when merge-event [merge-event]) steers))]
    {:flight/id run-id
     :flight/derivation :full
     :flight/links (vec (or links []))
     :organs
     (cond->
      {:field-read
       {:judgment (cond-> {:gauge {:ref (str begin-ref " :pre :dT-snapshot")
                                   :count dT-count}
                           :neighbourhood
                           (or neighbourhood
                               {:sorry {:kind :not-yet
                                        :note "decision neighbourhood not externalised this flight (the seat-read's articulation cost)"}})}
                    (seq cascades) (assoc :cascades (vec cascades)))
        :ground {:begin-artifact begin-ref
                 :scan-as-of (:scan-as-of begin)}}

       :velocity
       {:judgment (cond-> {:action v}
                    plan-sketch (assoc :plan-sketch (vec plan-sketch)))
        :ground :warrant}

       :warrant (or warrant (ghost :not-yet))

       :verification (or verification (ghost :not-yet))

       :attribution
       {:judgment (:v-attribution begin)
        :ground (str "cycle metadata; cg-id " (:cg-id begin))}

       :prediction
       {:judgment {:scaled (g1 predicted)
                   :constant (g1 predicted-constant)
                   :policy (ghost :not-yet :rollout-engine)}
        :ground "WM forward model; numbers from the field entry at begin (never invented)"}

       :begin-state
       {:judgment {:begin-at (:begin-at begin)
                   :target-g (g1 predicted)
                   :scan-as-of (:scan-as-of begin)}
        :ground begin-ref}

       :act
       (if executed?
         {:judgment {:state :executed
                     :witness (cond-> {:ref evidence-ref
                                       :verified-by agent}
                                act-verification
                                (assoc :verification act-verification))}
          :ground "the evidence-ref close-live-cycle! requires (no payload, no discharge)"}
         (ghost :proposal-mode))

       :measurement
       {:judgment (cond-> {:predicted (g1 predicted)
                           :predicted-constant (g1 predicted-constant)
                           :realised (g1 realised)
                           :error (when (and (number? realised) (number? predicted))
                                    (Math/abs (double (- realised predicted))))
                           ;; the class is the PILOT's judgment; only the
                           ;; censored fallback derives mechanically
                           :class (or class
                                      (when (= :target-absent-fallback realised-source)
                                        :fallback))
                           :realised-source realised-source}
                    window (assoc :window :window))
        :ground (if window :window "no settle window supplied — class :clean would not verify (F2), honestly")}

       :counterfactual
       (if (and (number? realised) (number? predicted-constant))
         {:judgment {:constant-error (Math/abs (double (- realised predicted-constant)))}
          :ground "derived: |realised - predicted-constant|; re-derivable by any reader"}
         (ghost :not-yet))

       :out-of-band
       {:judgment events
        :ground (if (seq events)
                  "discipline-events.edn (merge, verbatim at the same instant) + supplied steers"
                  "no out-of-band events in the flight's span")}

       :self-record
       {:judgment {:gamma-ref frame-path
                   :turn-record-count 1
                   :pilots-log-turn logged-turn}
        :ground "repl-trace write-frame! artifact"}}

       window
       (assoc :window
              {:judgment window
               :ground (or window-ground
                           "pilot settle protocol (close-time labour, kept)")}))}))

(defn write-flight-record!
  "Persist RECORD as <dir>/<run-id>.flight.edn (pretty, no length limits).
   Best-effort contract is the CALLER's (a persist failure must not break a
   close); this throws on IO error so tests see real failures."
  [record dir]
  (let [path (str dir "/" (:flight/id record) ".flight.edn")]
    (io/make-parents path)
    (spit path (binding [*print-length* nil *print-level* nil]
                 (with-out-str (pp/pprint record))))
    path))
