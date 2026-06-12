(ns scripts.flight-spec-verify
  "Verifier for the flight-as-derivation spec (holes/specs/flight.spec.edn,
   :wm-flight v0.2) — M-first-flights Phase-A exit 2, the logic model before
   code.

   Reads ONE flight record (the spec's envelope + :organs map) and asserts
   the nine invariants F1..F9. Per the logic-model-before-code discipline,
   the model verifies the DESIGN, not an implementation: a conforming
   witness record passes with zero violations, and one adversarial record
   per invariant — each taken from a real failure among the first twenty
   flights — is caught by exactly the invariant that names it.

   Run the self-test (witness + nine adversarials + the derivation-thin case):
     bb scripts/flight_spec_verify.clj --self-test

   Verify a record file (e.g. claude-3's conforming witness flight):
     bb scripts/flight_spec_verify.clj holes/specs/traces/flight-witness.edn

   Exit code 0 iff the record conforms (self-test: iff every expectation
   holds). Companion of scripts/repl_spec_verify.clj (V1-V5 over a trace);
   that one checks the operator's steps, this one checks one step's record."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; vocabulary (verbatim from flight.spec.edn)
;; ---------------------------------------------------------------------------

(def sorry-kinds #{:proposal-mode :not-yet :excluded-confound
                   :refused :abandoned :derivation-thin})
(def g-grains #{:one-step-action :policy-rollout})
(def organ-keys [:field-read :velocity :warrant :verification :attribution
                 :prediction :begin-state :act :measurement :counterfactual
                 :out-of-band :self-record])
(def link-types #{:applies-lesson-of :re-measures :confounded-by
                  :supersedes :cites-finding})
(def measurement-classes #{:clean :null :fallback :transient :confound})
(def abandon-reasons #{:superseded :operator-redirect :stale})
(def warrant-ref-kinds #{:standing-contract :doc-next-step :pattern
                         :operator-direction})

;; ---------------------------------------------------------------------------
;; the cell calculus
;; ---------------------------------------------------------------------------

(defn term? [c]
  (and (map? c) (contains? c :judgment) (contains? c :ground)
       (not (contains? c :sorry))))

(defn typed-sorry? [c]
  (and (map? c) (map? (:sorry c))
       (contains? sorry-kinds (get-in c [:sorry :kind]))))

(defn g-value? [g]
  (and (map? g) (number? (:g g)) (contains? g-grains (:g-grain g))))

(defn- organ [record k] (get-in record [:organs k]))

(defn- window-of
  "Resolve a measurement's R3 window: inline map, or the keyword :window
   pointing at the first-class :window organ (both spec-conformant — the
   real witness flight read the spec's layout as a thirteenth cell)."
  [record m]
  (let [w (get-in m [:judgment :window])]
    (if (map? w) w (get-in record [:organs :window :judgment]))))
(defn- abs* [x] (Math/abs (double x)))
(defn- ts<= [a b] (and (string? a) (string? b) (<= (compare a b) 0)))
(defn- ts< [a b] (and (string? a) (string? b) (neg? (compare a b))))

(defn settled-window?
  "The R3 rule, checkable from the record alone: two scans, distinct :as-of,
   both past :threshold, agreement <= epsilon, begin <= commit < threshold,
   and the recorded agreement equals |g1 - g2|."
  [w]
  (let [{:keys [begin commit threshold scans epsilon agreement]} w
        [s1 s2] scans]
    (and (map? w)
         (ts<= begin commit) (ts< commit threshold)
         (= 2 (count scans))
         (apply distinct? (map :as-of scans))
         (every? #(ts< threshold (:as-of %)) scans)
         (g-value? (:g s1)) (g-value? (:g s2))
         (number? epsilon) (number? agreement)
         (<= agreement epsilon)
         (< (abs* (- agreement (abs* (- (get-in s1 [:g :g])
                                        (get-in s2 [:g :g])))))
            1e-6))))

;; ---------------------------------------------------------------------------
;; F1 — term-or-sorry: every organ slot is a term-with-ground or a typed
;;      sorry; the envelope and links are well-formed (R2, R5, R0)
;; ---------------------------------------------------------------------------

(defn- check-f1 [record]
  (let [viols
        (concat
         (when-not (string? (:flight/id record))
           ["envelope: :flight/id missing or not a string"])
         (when-not (#{:full :thin} (:flight/derivation record))
           ["envelope: :flight/derivation must be :full or :thin (R6)"])
         (for [l (:flight/links record)
               :when (not (and (contains? link-types (:type l)) (:to l)))]
           (str "link " (pr-str l) " — :type must be one of " link-types
                " with a :to (R10)"))
         (for [k (distinct (concat organ-keys (keys (:organs record))))
               :let [c (organ record k)]
               :when (not (or (term? c) (typed-sorry? c)))]
           (str (name k) ": neither a term-with-ground nor a typed sorry — "
                (pr-str c))))]
    {:pass (empty? viols) :violations (vec viols)}))

;; ---------------------------------------------------------------------------
;; F2 — no-unwitnessed-settled: a measurement claiming :clean or :null must
;;      carry a window satisfying the settled rule (R3). Canonical
;;      adversarial: the stale-begin confound — a scan not past threshold.
;; ---------------------------------------------------------------------------

(defn- check-f2 [record]
  (let [m (organ record :measurement)
        viols
        (when (and (term? m)
                   (contains? #{:clean :null} (get-in m [:judgment :class])))
          (let [w (window-of record m)]
            (cond
              (nil? w)
              ["measurement: class claims settled but carries no window (R3)"]
              (not (settled-window? w))
              [(str "measurement: window fails the settled rule — two scans, "
                    "distinct as-of, both past threshold, agreement <= epsilon, "
                    "agreement = |g1-g2| — got " (pr-str w))]
              :else nil)))]
    {:pass (empty? viols) :violations (vec viols)}))

;; ---------------------------------------------------------------------------
;; F3 — no-fabricated-zero: a measurement term carries its interpretation
;;      class and a consistent error; an exact realised==predicted copy may
;;      not call itself :clean (the censored 0.0, flight T4) (R1)
;; ---------------------------------------------------------------------------

(defn- check-f3 [record]
  (let [m (organ record :measurement)
        viols
        (when (term? m)
          (let [{:keys [predicted realised error class]} (:judgment m)]
            (concat
             (when-not (contains? measurement-classes class)
               [(str "measurement: interpretation class " (pr-str class)
                     " missing or not one of " measurement-classes)])
             (when (and (g-value? predicted) (g-value? realised))
               (concat
                (when (and (number? error)
                           (> (abs* (- error (abs* (- (:g realised)
                                                      (:g predicted)))))
                              1e-6))
                  [(str "measurement: :error " error
                        " != |realised - predicted| = "
                        (abs* (- (:g realised) (:g predicted))))])
                (when (and (= (:g realised) (:g predicted))
                           (= :clean class))
                  ["measurement: realised is an EXACT copy of predicted yet claims :clean — the censored-0.0 shape; an exact copy is :null (with witness) or :fallback, never :clean"]))))))]
    {:pass (empty? viols) :violations (vec viols)}))

;; ---------------------------------------------------------------------------
;; F4 — act-states-complete: executed+witness / proposal-sorry /
;;      refused+finding / abandoned+reason+superseded-by; nothing else (R2, R8)
;; ---------------------------------------------------------------------------

(defn- check-f4 [record]
  (let [a (organ record :act)
        viols
        (cond
          (typed-sorry? a)
          (when-not (contains? #{:proposal-mode :derivation-thin}
                               (get-in a [:sorry :kind]))
            [(str "act: sorry kind " (pr-str (get-in a [:sorry :kind]))
                  " — an absent act is :proposal-mode (or :derivation-thin on backfill); refusals and abandons are TERMS with payloads")])
          (term? a)
          (let [{:keys [state witness finding reason-class superseded-by]}
                (:judgment a)]
            (case state
              :executed
              (when-not (and (map? witness) (:ref witness)
                             (:verified-by witness) (:verification witness))
                ["act: :executed without a full witness {:ref :verified-by :verification} (R8 — who re-ran what before the sha was trusted)"])
              :refused
              (when-not finding
                ["act: :refused without its :finding payload — a refusal is a complete flight, not a gap"])
              :abandoned
              (when-not (and (contains? abandon-reasons reason-class)
                             superseded-by)
                [(str "act: :abandoned needs :reason-class in " abandon-reasons
                      " and a :superseded-by — else indistinguishable from a crash")])
              [(str "act: state " (pr-str state)
                    " not one of :executed :refused :abandoned (proposal is a typed sorry)")]))
          :else nil)]
    {:pass (empty? viols) :violations (vec viols)}))

;; ---------------------------------------------------------------------------
;; F5 — warranted-velocity: a velocity requires a warrant term —
;;      determined-with-ref, :pilot-synthesis-with-reasoning, or queued (R7)
;; ---------------------------------------------------------------------------

(defn- check-f5 [record]
  (let [v (organ record :velocity)
        w (organ record :warrant)
        viols
        (when (term? v)
          (if-not (term? w)
            ["velocity present but the warrant cell is no term — the cycle-5 shape: a fork flown with no recorded warrant (R7)"]
            (let [{:keys [determined? determined-by queued]} (:judgment w)]
              (cond
                (true? determined?)
                (let [{:keys [kind ref reasoning]} determined-by]
                  (cond
                    (contains? warrant-ref-kinds kind)
                    (when-not ref
                      [(str "warrant: ref kind " kind " without a :ref — the warrant must be findable where it points")])
                    (= :pilot-synthesis kind)
                    (when-not reasoning
                      ["warrant: :pilot-synthesis without the recorded :reasoning — the synthesis IS the warrant; absent reasoning would force an invented ref"])
                    :else
                    [(str "warrant: :determined-by kind " (pr-str kind)
                          " not one of " (conj warrant-ref-kinds :pilot-synthesis))]))
                (false? determined?)
                (when-not (:queue-ref queued)
                  ["warrant: undetermined fork without a :queued :queue-ref — safe default + queue, never an invented warrant"])
                :else
                ["warrant: :determined? must be true or false"]))))]
    {:pass (empty? viols) :violations (vec viols)}))

;; ---------------------------------------------------------------------------
;; F6 — grains-never-mix: every G wears :g-grain; predicted and realised
;;      share a grain per comparison; :policy-rollout is rejected while the
;;      rollout engine is unbuilt (V-f2)
;; ---------------------------------------------------------------------------

(def ^:private g-sites
  [[:prediction :judgment :scaled] [:prediction :judgment :constant]
   [:begin-state :judgment :target-g]
   [:measurement :judgment :predicted] [:measurement :judgment :predicted-constant]
   [:measurement :judgment :realised]])

(defn- check-f6 [record {:keys [rollout-engine?]}]
  (let [viols
        (concat
         (for [[ok & path] g-sites
               :let [c (organ record ok)]
               :when (term? c)
               :let [g (get-in c (vec path))]
               :when (and (some? g) (not (g-value? g)))]
           (str (name ok) " " (pr-str (vec path)) ": " (pr-str g)
                " is not a g-value {:g <num> :g-grain " g-grains "} — no bare G anywhere"))
         (let [p (organ record :prediction)
               pol (get-in p [:judgment :policy])]
           (when (and (term? p) (g-value? pol)
                      (= :policy-rollout (:g-grain pol))
                      (not rollout-engine?))
             ["prediction: claims :policy-rollout grain while the rollout engine is unbuilt — the mixed-grains adversarial; the policy slot is a typed ghost until Phase B arms"]))
         (let [m (organ record :measurement)
               pg (get-in m [:judgment :predicted :g-grain])
               rg (get-in m [:judgment :realised :g-grain])]
           (when (and (term? m) pg rg (not= pg rg))
             [(str "measurement: predicted grain " pg " vs realised grain " rg
                   " — one grain per comparison, grains never mix")])))]
    {:pass (empty? viols) :violations (vec viols)}))

;; ---------------------------------------------------------------------------
;; F7 — mask-derived: no authored trainability or co-pilotedness flags
;;      anywhere; the training projection is a pure function of the record
;;      (R11). The derived mask is reported, never read from the record.
;; ---------------------------------------------------------------------------

(defn training-projection
  "R11: the pure projection of a flight record to a training example.
   mask :in  <=> :full record, measurement is a term, class :clean or :null,
                 window settled.  Everything else masks :out."
  [record]
  (let [m (organ record :measurement)
        cls (get-in m [:judgment :class])
        in? (and (= :full (:flight/derivation record))
                 (term? m)
                 (contains? #{:clean :null} cls)
                 (settled-window? (window-of record m)))]
    {:state-ref (:ground (organ record :begin-state))
     :candidates (get-in (organ record :field-read) [:judgment :neighbourhood])
     :predicted (get-in m [:judgment :predicted])
     :predicted-constant (get-in m [:judgment :predicted-constant])
     :realised (get-in m [:judgment :realised])
     :class cls
     :validity-mask (if in? :in :out)}))

(defn- check-f7 [record]
  (let [authored (for [node (tree-seq coll? seq record)
                       :when (map? node)
                       k [:trainable :co-piloted :mask-in]
                       :when (contains? node k)]
                   (str "authored flag " k " found at " (pr-str (select-keys node [k]))
                        " — trainability and co-pilotedness are DERIVED, never declared"))]
    {:pass (empty? authored) :violations (vec authored)
     :derived-mask (:validity-mask (training-projection record))}))

;; ---------------------------------------------------------------------------
;; F8 — sampler-provenance: every cascade wears its :sampler (checkpoint 6)
;; ---------------------------------------------------------------------------

(defn- check-f8 [record]
  (let [fr (organ record :field-read)
        viols (when (term? fr)
                (for [c (get-in fr [:judgment :cascades])
                      :when (nil? (:sampler c))]
                  (str "cascade " (pr-str (select-keys c [:psi :C]))
                       " without :sampler — sampling methods must be distinguishable, never silently pooled")))]
    {:pass (empty? viols) :violations (vec viols)}))

;; ---------------------------------------------------------------------------
;; F9 — steering-recorded: operator steers are content-bearing events with
;;      timestamps; co-pilotedness is derived from their presence (R12)
;; ---------------------------------------------------------------------------

(defn- check-f9 [record]
  (let [oob (organ record :out-of-band)
        events (when (term? oob) (:judgment oob))
        steers (filter #(= :operator-steer (:type %)) events)
        viols (for [s steers
                    :when (not (and (:at s) (:content s)))]
                (str "operator-steer " (pr-str s)
                     " without :at + :content — a contentless steer re-reads as a solo flight (R12)"))]
    {:pass (empty? viols) :violations (vec viols)
     :co-piloted? (boolean (seq steers))}))

;; ---------------------------------------------------------------------------
;; top-level
;; ---------------------------------------------------------------------------

(defn verify
  "Verify one flight record against F1-F9. OPTS: :rollout-engine? (default
   false) arms the :policy-rollout grain when Phase B lands. Returns a
   report map; :conforms? iff all nine pass."
  ([record] (verify record {}))
  ([record opts]
   (let [fs {:F1-term-or-sorry (check-f1 record)
             :F2-no-unwitnessed-settled (check-f2 record)
             :F3-no-fabricated-zero (check-f3 record)
             :F4-act-states-complete (check-f4 record)
             :F5-warranted-velocity (check-f5 record)
             :F6-grains-never-mix (check-f6 record opts)
             :F7-mask-derived (check-f7 record)
             :F8-sampler-provenance (check-f8 record)
             :F9-steering-recorded (check-f9 record)}
         conforms? (every? :pass (vals fs))
         ;; model finding (self-test run 1): the class+window rule alone lets
         ;; a record that FABRICATES a consistent settled window mask in even
         ;; while F3 catches the fraud — so the mask additionally requires
         ;; conformance: no record failing any invariant may train.
         proj (cond-> (training-projection record)
                (not conforms?) (assoc :validity-mask :out
                                       :masked-out-by :non-conforming))]
     {:spec "wm-flight v0.2"
      :flight (:flight/id record)
      :derivation (:flight/derivation record)
      :invariants fs
      :projection proj
      :conforms? conforms?})))

(defn- format-report [report]
  (let [lines (atom [])
        emit #(swap! lines conj %)]
    (emit (str "Flight spec verifier — " (:spec report)))
    (emit (str "flight: " (:flight report)
               "  (derivation " (name (or (:derivation report) :unknown)) ")"))
    (emit "")
    (doseq [[k {:keys [pass violations] :as r}] (:invariants report)]
      (emit (str (if pass "✅" "❌") "  " (name k)
                 (if pass "  ok" (str "  " (count violations) " violation(s)"))
                 (when-some [m (:derived-mask r)] (str "  [derived mask: " (name m) "]"))
                 (when (contains? r :co-piloted?)
                   (str "  [co-piloted: " (:co-piloted? r) "]"))))
      (doseq [v violations] (emit (str "      - " v))))
    (emit "")
    (emit (str "training projection → validity-mask "
               (name (get-in report [:projection :validity-mask]))))
    (emit (str (if (:conforms? report) "CONFORMS ✅" "DOES NOT CONFORM ❌")))
    (str/join "\n" @lines)))

;; ---------------------------------------------------------------------------
;; self-test: the model's witness + one adversarial per invariant, each from
;; a real failure among the first twenty flights
;; ---------------------------------------------------------------------------

(def witness
  "Hand-authored conforming record, numbers self-consistent, modelled on
   flight T9 (the day-in-the-life flight). claude-3's real witness flight
   replaces this as the canonical witness once flown."
  {:flight/id "model-witness-f1f9"
   :flight/derivation :full
   :flight/links [{:type :applies-lesson-of :to "live-T8"}]
   :organs
   {:field-read
    {:judgment {:gauge {:ref "scan/2026-06-12T09:00:11Z" :count 120}
                :neighbourhood
                {:chosen {:type :advance-mission :target "M-daily-scan"}
                 :rejected [{:action {:type :address-sorry
                                      :target "sorry/pattern-measure-never-target"}
                             :reason :no-measurable-pair}
                            {:action {:type :advance-mission :target "M-chipwitz-corps"}
                             :reason :capped}]
                 :band {:size 22 :why :tied-at-cap}}
                :cascades [{:psi "day counter shows binary flag instead of real day number"
                            :patterns [{:ref "scan-coherence/mission-anchored-scan" :rel 0.369}]
                            :C 0.369
                            :sampler :scan-lane-v1}]}
     :ground {:scan-as-of "2026-06-12T09:00:11Z" :window-id "w-42"}}
    :velocity
    {:judgment {:action {:type :advance-mission :target "M-daily-scan"}
                :plan-sketch ["fix item 368" "commit" "force scan" "settle" "close"]}
     :ground :warrant}
    :warrant
    {:judgment {:determined? true
                :determined-by {:kind :standing-contract
                                :ref "PILOT-ARC-2-QUEUE.md#prefer-measured-pairs"}}
     :ground "ref findable at the named anchor"}
    :verification
    {:judgment {:holes [{:hole "item-368" :class :bounded-doable
                         :evidence "brief Day counter shows 1 / 2+"}
                        {:hole "item-367" :class :operator-gated
                         :evidence "scan-intent is Joe's call"}]
                :chosen-hole "item-368"}
     :ground "grep + read at select-time"}
    :attribution
    {:judgment :pilot-autonomous :ground "no operator events before close"}
    :prediction
    {:judgment {:scaled {:g -4.1233 :g-grain :one-step-action}
                :constant {:g -4.0836 :g-grain :one-step-action}
                :policy {:sorry {:kind :not-yet :blocked-by :rollout-engine}}}
     :ground {:model "target-sensitive-v2" :field-entry 1}}
    :begin-state
    {:judgment {:begin-at "2026-06-12T12:01:42Z"
                :target-g {:g -4.1233 :g-grain :one-step-action}
                :scan-as-of "2026-06-12T12:01:30Z"}
     :ground "model-witness-f1f9.begin.edn"}
    :act
    {:judgment {:state :executed
                :witness {:ref "futon7 701522d"
                          :verified-by "claude-3"
                          :verification "kondo 0/0 + ns loads, re-run before accepting the sha"}}
     :ground "commit + re-run gates"}
    :measurement
    {:judgment {:predicted {:g -4.1233 :g-grain :one-step-action}
                :predicted-constant {:g -4.0836 :g-grain :one-step-action}
                :realised {:g -4.0847 :g-grain :one-step-action}
                :error 0.0386
                :class :clean
                :window {:begin "2026-06-12T12:01:42Z"
                         :commit "2026-06-12T12:08:15Z"
                         :threshold "2026-06-12T12:09:40Z"
                         :scans [{:as-of "2026-06-12T12:14:09Z"
                                  :g {:g -4.0846 :g-grain :one-step-action}}
                                 {:as-of "2026-06-12T12:21:33Z"
                                  :g {:g -4.0847 :g-grain :one-step-action}}]
                         :epsilon 0.005
                         :agreement 0.0001}}
     :ground :window}
    :counterfactual
    {:judgment {:constant-error 0.0011}
     :ground "derived: |realised - predicted-constant|, re-derivable by any reader"}
    :out-of-band
    {:judgment [{:type :operator-merge :at "2026-06-12T12:29:19Z"}]
     :ground "discipline-events.edn"}
    :self-record
    {:judgment {:gamma-ref "γ/model-witness-f1f9" :turn-records 1}
     :ground "repl-trace artifact"}}})

(def thin-record
  "A backfilled pre-schema record: representable, flagged, masked out (R6)."
  {:flight/id "live-d2068cc1-backfill"
   :flight/derivation :thin
   :organs (into {} (for [k organ-keys]
                      [k {:sorry {:kind :derivation-thin
                                  :note "pre-schema record; grounds unrecoverable"}}]))})

(def adversarials
  "[label invariant-expected-to-catch record] — each from a real failure."
  [["bare-scalar prediction (the live-d2068cc1 row)" :F1-term-or-sorry
    (assoc-in witness [:organs :prediction] -4.9225)]
   ["stale-begin confound (DERIVE's self-demonstration): scan not past threshold" :F2-no-unwitnessed-settled
    (assoc-in witness [:organs :measurement :judgment :window :scans]
              [{:as-of "2026-06-12T12:08:20Z" :g {:g -4.0846 :g-grain :one-step-action}}
               {:as-of "2026-06-12T12:21:33Z" :g {:g -4.0847 :g-grain :one-step-action}}])]
   ["censored 0.0 (flight T4): exact copy claiming :clean" :F3-no-fabricated-zero
    (-> witness
        (assoc-in [:organs :measurement :judgment :realised]
                  {:g -4.1233 :g-grain :one-step-action})
        (assoc-in [:organs :measurement :judgment :error] 0.0)
        (assoc-in [:organs :measurement :judgment :window :scans]
                  [{:as-of "2026-06-12T12:14:09Z" :g {:g -4.1233 :g-grain :one-step-action}}
                   {:as-of "2026-06-12T12:21:33Z" :g {:g -4.1234 :g-grain :one-step-action}}]))]
   ["executed with no witness" :F4-act-states-complete
    (assoc-in witness [:organs :act :judgment] {:state :executed})]
   ["the cycle-5 shape: velocity flown, warrant cell empty" :F5-warranted-velocity
    (assoc-in witness [:organs :warrant] {:sorry {:kind :not-yet}})]
   ["mixed grains: policy-rollout claimed while the engine is unbuilt" :F6-grains-never-mix
    (assoc-in witness [:organs :prediction :judgment :policy]
              {:g -4.2 :g-grain :policy-rollout})]
   ["authored trainability flag" :F7-mask-derived
    (assoc-in witness [:organs :measurement :judgment :trainable] true)]
   ["two-sampler pooling hidden: cascade without provenance" :F8-sampler-provenance
    (update-in witness [:organs :field-read :judgment :cascades 0] dissoc :sampler)]
   ["contentless operator steer: the DERIVE flight re-read as solo" :F9-steering-recorded
    (update-in witness [:organs :out-of-band :judgment] conj
               {:type :operator-steer :at "2026-06-12T12:05:00Z"})]])

(defn- self-test []
  (let [w-report (verify witness)
        w-ok (and (:conforms? w-report)
                  (= :in (get-in w-report [:projection :validity-mask])))
        thin-report (verify thin-record)
        thin-ok (and (:conforms? thin-report)
                     (= :out (get-in thin-report [:projection :validity-mask])))
        adv-results
        (for [[label inv rec] adversarials
              :let [r (verify rec)
                    caught? (not (get-in r [:invariants inv :pass]))
                    others (for [[k v] (:invariants r)
                                 :when (and (not= k inv) (not (:pass v)))] k)]]
          {:label label :invariant inv :caught? caught? :also-flagged (vec others)
           :masked-out? (= :out (get-in r [:projection :validity-mask]))})
        all-caught (every? :caught? adv-results)]
    (println "== witness ==")
    (println (format-report w-report))
    (println)
    (println "== derivation-thin backfill record ==")
    (println (str "conforms " (:conforms? thin-report)
                  " · mask " (name (get-in thin-report [:projection :validity-mask]))
                  " (representable but never trainable — R6, and why exit-5's"
                  " stratification fails on thin records)"))
    (println)
    (println "== adversarials (one per invariant, each a real failure) ==")
    (doseq [{:keys [label invariant caught? also-flagged masked-out?]} adv-results]
      (println (str (if caught? "✅ caught" "❌ MISSED") "  " (name invariant)
                    "  — " label
                    (when (seq also-flagged) (str "  (also: " (str/join ", " (map name also-flagged)) ")"))
                    "  mask:" (if masked-out? "out" "in"))))
    (println)
    (let [ok (and w-ok thin-ok all-caught)]
      (println (str (if ok "SELF-TEST PASS ✅" "SELF-TEST FAIL ❌")
                    "  (witness conforms+mask-in: " w-ok
                    " · thin conforms+mask-out: " thin-ok
                    " · " (count (filter :caught? adv-results)) "/"
                    (count adv-results) " adversarials caught)"))
      ok)))

(defn -main [& args]
  (let [arg (first args)]
    (if (or (nil? arg) (= "--self-test" arg))
      (System/exit (if (self-test) 0 1))
      (let [record (edn/read-string (slurp arg))
            report (verify record)]
        (println (format-report report))
        (System/exit (if (:conforms? report) 0 1))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
