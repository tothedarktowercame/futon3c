(ns ^{:clj-kondo/config
      '{:linters
        {:unresolved-symbol {:level :off}}}}
  futon3c.logic.typed-bells-invariants
  "VERIFY model for M-typed-bells — the logic model checked BEFORE the protocol
   and ArSE bridge are written (house discipline: 'check the logic model before
   we write the code'; origin outing_invariants.clj).

   Follows the house idiom (logic/outing-invariants, agency/logic.clj):
   trace -> build-db -> per-invariant goals -> query-violations, over
   clojure.core.logic + pldb. The 'trace' is an ABSTRACT bell-trace: a vector of
   bell-records describing what the implementation produced for each bell, so the
   design's guarantees can be checked against adversarial traces without the live
   transport / ArSE bridge.

   A design is VERIFIED iff:
     (a) the witnessed conforming trace yields ZERO violations, and
     (b) each adversarial trace (one per invariant) is CAUGHT by its own category.

   Invariants encoded (mission M-typed-bells §DERIVE, TB-1..7):
     TB-1 type-validity     accepted bell's effective type is in the allowed set
     TB-2 off-inertness     flag OFF ⇒ no recorded-type / ArSE write / header
                            Type line / graph type (legacy system exactly)
     TB-3 answer-needs-ref  an accepted :answer bell carries a non-blank :ref
     TB-4 arse-iff-qa       an ArSE write ⇒ Q&A type (no over-promotion); and a
                            query-no-ref or an answer ⇒ an ArSE write (no
                            under-population)
     TB-5 ref-fidelity      an :answer's ArSE write uses in-reply-to == :ref
     TB-6 crossing-sound    a typed :answer closes its referent's open edge; no
                            resolved pair survives as a crossing (E-crossed-bells)
     TB-7 idempotent-write  a bell creates at most one ArSE thread (replay-safe)

   OFFLINE-ONLY for VERIFY; the same checks become live mesh-QA probes (cf.
   mesh_qa MQ-*) during INSTANTIATE."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

;; =============================================================================
;; Allowed effective bell types (IAT-seven + :request default). TB-1 oracle.
;; =============================================================================

(def allowed-types
  #{:query :answer :assert :challenge :agree :define :retract :suggest :request})

(def qa-types #{:query :answer})

;; =============================================================================
;; Relations (fact schema over a bell-trace)
;; =============================================================================

(pldb/db-rel bello          b)
(pldb/db-rel acceptedo      b)                 ; the bell was accepted (not 400'd)
(pldb/db-rel typeo          b t)               ; effective dispatch type (flag-on, accepted)
(pldb/db-rel refo           b r)               ; the :ref pointer (referent/thread id)
(pldb/db-rel flag-offo      b)                 ; FUTON3C_TYPED_BELLS off for this bell
(pldb/db-rel recorded-typeo b t)               ; type written into the job map (persistence)
(pldb/db-rel arse-writeo    b op)              ; op: :ask | :answer (an ArSE write happened)
(pldb/db-rel arse-irto      b irt)             ; the in-reply-to the ArSE write used
(pldb/db-rel header-typeo   b)                 ; surface header carried a Type: line
(pldb/db-rel graph-typeo    b t)               ; conversation-graph edge carried a type
(pldb/db-rel openo          b)                 ; bell still open in the graph
(pldb/db-rel resolveso      ans asked)         ; an :answer resolves the asked bell
(pldb/db-rel threads-createdo b n)             ; # ArSE threads this bell created

;; =============================================================================
;; Database construction — a trace is a vector of bell-records
;; =============================================================================
;;
;; bell-record keys (only :bell required; sensible conforming defaults):
;;   :bell id   :type kw|nil   :accepted? (default true)   :ref str|nil
;;   :flag-on? (default true)  :recorded-type kw|nil  :arse-write :ask|:answer|nil
;;   :arse-irt str|nil  :header-type? bool  :graph-type kw|nil  :open? bool
;;   :resolves <asked-bell>|nil  :threads-created int|nil
;;
;; Effective type (asserted via typeo) is materialised ONLY for accepted,
;; flag-on bells, defaulting nil -> :request (untyped ≡ request).
;; =============================================================================

(defn build-db
  [trace]
  (reduce
   (fn [db {:keys [bell type accepted? ref flag-on? recorded-type arse-write
                   arse-irt header-type? graph-type open? resolves threads-created]
            :or {accepted? true flag-on? true}}]
     (let [eff-type (or type :request)]
       (cond-> (pldb/db-fact db bello bell)
         accepted?                    (pldb/db-fact acceptedo bell)
         (and accepted? flag-on?)     (pldb/db-fact typeo bell eff-type)
         ref                          (pldb/db-fact refo bell ref)
         (not flag-on?)               (pldb/db-fact flag-offo bell)
         recorded-type                (pldb/db-fact recorded-typeo bell recorded-type)
         arse-write                   (pldb/db-fact arse-writeo bell arse-write)
         arse-irt                     (pldb/db-fact arse-irto bell arse-irt)
         header-type?                 (pldb/db-fact header-typeo bell)
         graph-type                   (pldb/db-fact graph-typeo bell graph-type)
         open?                        (pldb/db-fact openo bell)
         resolves                     (pldb/db-fact resolveso bell resolves)
         (some? threads-created)      (pldb/db-fact threads-createdo bell threads-created))))
   pldb/empty-db
   trace))

;; helpers: "bell b has some ref / some arse-write / some arse-irt"
(defn- ref-anyo       [b] (l/fresh [r]  (refo b r)))
(defn- arse-write-anyo [b] (l/fresh [op] (arse-writeo b op)))
(defn- arse-irt-anyo  [b] (l/fresh [x]  (arse-irto b x)))

;; =============================================================================
;; Queries — detect violations (empty vector = invariant holds)
;; =============================================================================

(defn q-tb1-type-validity [db]
  ;; accepted bell whose effective type is not in the allowed set
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [b t]
        (acceptedo b) (typeo b t)
        (l/project [t] (if (allowed-types t) l/fail l/succeed))
        (l/== q {:bell b :bad-type t})))))

(defn q-tb2-off-inertness [db]
  ;; flag OFF, yet some typed-path effect leaked through
  (let [leak (fn [effect-goal label]
               (pldb/with-db db
                 (l/run* [q]
                   (l/fresh [b]
                     (flag-offo b) (effect-goal b)
                     (l/== q {:bell b :leak label})))))]
    (vec (concat
          (leak (fn [b] (l/fresh [t] (recorded-typeo b t))) :recorded-type)
          (leak arse-write-anyo                              :arse-write)
          (leak header-typeo                                 :header-type-line)
          (leak (fn [b] (l/fresh [t] (graph-typeo b t)))     :graph-type)))))

(defn q-tb3-answer-needs-ref [db]
  ;; accepted :answer with no ref
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [b]
        (acceptedo b) (typeo b :answer) (l/nafc ref-anyo b)
        (l/== q {:bell b :violation :answer-without-ref})))))

(defn q-tb4-arse-iff-qa [db]
  ;; (a) over-promotion: an ArSE write on a non-Q&A type
  (let [over
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [b op t]
              (arse-writeo b op) (typeo b t)
              (l/project [t] (if (qa-types t) l/fail l/succeed))
              (l/== q {:bell b :violation :arse-write-on-non-qa :type t}))))
        ;; (b) under-population: a query-with-no-ref, or an answer, that did NOT write
        under-query
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [b]
              (acceptedo b) (typeo b :query) (l/nafc ref-anyo b)
              (l/nafc arse-write-anyo b)
              (l/== q {:bell b :violation :query-no-arse-write}))))
        under-answer
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [b]
              (acceptedo b) (typeo b :answer)
              (l/nafc arse-write-anyo b)
              (l/== q {:bell b :violation :answer-no-arse-write}))))]
    (vec (concat over under-query under-answer))))

(defn q-tb5-ref-fidelity [db]
  ;; an :answer ArSE write whose in-reply-to is missing or != :ref
  (let [missing
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [b]
              (arse-writeo b :answer) (l/nafc arse-irt-anyo b)
              (l/== q {:bell b :violation :answer-write-missing-irt}))))
        mismatched
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [b r irt]
              (arse-writeo b :answer) (refo b r) (arse-irto b irt)
              (l/project [r irt] (if (= r irt) l/fail l/succeed))
              (l/== q {:bell b :violation :irt-ref-mismatch :ref r :irt irt}))))]
    (vec (concat missing mismatched))))

(defn q-tb6-crossing-sound [db]
  ;; a resolved asked-bell that is still reported OPEN (would surface as a false
  ;; crossing). Typed replies must close their referent's edge.
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [ans asked]
        (resolveso ans asked) (openo asked)
        (l/== q {:asked asked :resolved-by ans :violation :resolved-but-open})))))

(defn q-tb7-idempotent-write [db]
  ;; a bell that created more than one ArSE thread (replay made a duplicate)
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [b n]
        (threads-createdo b n)
        (l/project [n] (if (> n 1) l/succeed l/fail))
        (l/== q {:bell b :threads n :violation :duplicate-thread})))))

(defn query-violations
  "All TB invariant checks. category -> violating rows (empty = holds)."
  [db]
  {:tb1-type-validity    (q-tb1-type-validity db)
   :tb2-off-inertness    (q-tb2-off-inertness db)
   :tb3-answer-needs-ref (q-tb3-answer-needs-ref db)
   :tb4-arse-iff-qa      (q-tb4-arse-iff-qa db)
   :tb5-ref-fidelity     (q-tb5-ref-fidelity db)
   :tb6-crossing-sound   (q-tb6-crossing-sound db)
   :tb7-idempotent-write (q-tb7-idempotent-write db)})

(defn violations? [violations]
  (some (fn [[_ v]] (seq v)) violations))

;; =============================================================================
;; Fixtures — witnessed conforming trace + one adversarial trace per invariant
;; =============================================================================

(def ^:private q1
  "A conforming :query — fresh ask, creates exactly one ArSE thread, answered."
  {:bell :q1 :type :query :ref nil :arse-write :ask :threads-created 1
   :recorded-type :query :header-type? true :graph-type :query :open? false})

(def ^:private a1
  "A conforming :answer to q1 — references the thread, writes the answer, closes
   q1's edge (resolves q1), creates no new thread."
  {:bell :a1 :type :answer :ref "ask-q1" :arse-write :answer :arse-irt "ask-q1"
   :resolves :q1 :threads-created 0 :recorded-type :answer :graph-type :answer
   :open? false})

(def ^:private r1
  "A conforming untyped/:request task bell — no ArSE involvement, may stay open."
  {:bell :r1 :type :request :recorded-type :request :graph-type :request :open? true})

(def witness-trace [q1 a1 r1])

;; tb6 needs a 2-bell trace where the answer resolves an asked bell left OPEN.
(def ^:private q1-open (assoc q1 :open? true))                    ; asked, but left open

(def adversarial-traces
  "One trace per invariant, each planted to break exactly that invariant."
  {:tb1-type-validity    [(assoc q1 :type :frobnicate)]            ; unknown type, accepted
   :tb2-off-inertness    [(assoc q1 :flag-on? false)]             ; flag off, effects leak
   :tb3-answer-needs-ref [(assoc a1 :ref nil :arse-irt nil)]      ; answer with no ref
   :tb4-arse-iff-qa      [(assoc r1 :arse-write :ask)]            ; non-Q&A type writes to ArSE
   :tb5-ref-fidelity     [(assoc a1 :arse-irt "ask-WRONG")]      ; irt != ref
   :tb6-crossing-sound   [q1-open a1]                             ; resolved asked still open
   :tb7-idempotent-write [(assoc q1 :threads-created 2)]})       ; replay made 2 threads

(defn run-verify
  "VERIFY entrypoint: witness must be clean; each adversarial trace must be
   caught BY ITS OWN category. Returns a report; :verified? is the gate."
  []
  (let [witness-v (query-violations (build-db witness-trace))
        witness-clean? (not (violations? witness-v))
        adversarial
        (into {}
              (map (fn [[cat trace]]
                     (let [v (query-violations (build-db trace))
                           caught? (seq (get v cat))]
                       [cat {:caught? (boolean caught?)
                             :hits (get v cat)}]))
                   adversarial-traces))
        all-caught? (every? :caught? (vals adversarial))]
    {:verified? (and witness-clean? all-caught?)
     :witness {:clean? witness-clean? :violations witness-v}
     :adversarial adversarial
     :summary {:witness-clean? witness-clean?
               :all-adversarial-caught? all-caught?
               :n-invariants (count adversarial-traces)}}))
