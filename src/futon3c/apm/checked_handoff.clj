(ns futon3c.apm.checked-handoff
  "Pure validation for typed checked-handoff verdict events.

   Independence is computed here. A dangling rerun-witness is downgraded to
   :constant-assertion with :r9/rerun-witness-unresolved; it can never receive
   :adjudicator-rerun-witnessed. The resolver is injected so this namespace
   has no evidence-store dependency.")

(def event-type :checked-handoff/verdict)

(def independence-grades
  "Closed vocabulary accepted from typed receipt fields."
  #{:asserted-unverified
    :seat-string-distinctness
    :adjudicator-rerun-witnessed
    :constant-assertion
    :ungradeable-legacy})

(defn verdict-event
  "Construct the declared checked-handoff event shape."
  [{:keys [worker-seat author-seat proposal verdict adjudication]}]
  {:event event-type
   :worker-seat worker-seat
   :author-seat author-seat
   :proposal proposal
   :verdict verdict
   :adjudication adjudication})

(defn- writer-grade-present? [event]
  (or (contains? event :grade)
      (contains? event :independence/grade)))

(defn- rerun-witness-id [event]
  (let [value (get-in event [:adjudication :rerun-witness])]
    (when-not (= :absent value) value)))

(defn validate-verdict-event
  "Validate EVENT and compute its independence grade.

   RESOLVE-WITNESS receives a rerun-witness evidence id and returns its record
   or nil. A resolved witness earns the top grade only when its :proposal is
   exactly the event's proposal. Writer-supplied :grade or
   :independence/grade fields are removed and reported in :notes."
  [event resolve-witness]
  (cond
    ;; Reviewer fix (claude-2): a missing/blank seat must not read as
    ;; "distinct". (= "a" nil) is false, so without this check a malformed
    ;; event with no :worker-seat would sail through to
    ;; :seat-string-distinctness — distinctness manufactured from absence.
    (not (and (string? (:worker-seat event))
              (string? (:author-seat event))
              (seq (:worker-seat event))
              (seq (:author-seat event))))
    {:ok false
     :error/code :r9/verdict-event-malformed
     :event (dissoc event :grade :independence/grade)}

    (= (:author-seat event) (:worker-seat event))
    {:ok false
     :error/code :r9/worker-authored-verdict-refused
     :event (dissoc event :grade :independence/grade)}

    :else
    (let [witness-id (rerun-witness-id event)
          witness (when witness-id
                    (try
                      (resolve-witness witness-id)
                      (catch Throwable _ nil)))
          witnessed? (and witness (= (:proposal event) (:proposal witness)))
          grade (cond
                  witnessed? :adjudicator-rerun-witnessed
                  (nil? witness-id) :seat-string-distinctness
                  :else :constant-assertion)
          notes (cond-> []
                  (writer-grade-present? event) (conj :r9/grade-is-computed)
                  (and witness-id (nil? witness))
                  (conj :r9/rerun-witness-unresolved)
                  (and witness-id witness (not witnessed?))
                  (conj :r9/rerun-witness-proposal-mismatch))]
      {:ok true
       :event (dissoc event :grade :independence/grade)
       :independence/grade grade
       :notes notes})))

(defn grade-receipt
  "Derive an independence grade from a promotion or guide RECEIPT.

   Persisted seat evidence takes precedence over an asserted typed field.
   Legacy booleans never establish independence: without the seats or typed
   field they are permanently :ungradeable-legacy, regardless of their value.
   An unknown typed grade is refused rather than silently becoming a grade."
  [receipt]
  (let [seats-present? (and (contains? receipt :receipt/depositor-seat)
                            (contains? receipt :receipt/reviewer-seat))
        typed-present? (contains? receipt :receipt/independence)
        typed-grade (:receipt/independence receipt)]
    (cond
      seats-present?
      (let [seat-grade (if (= (:receipt/depositor-seat receipt)
                              (:receipt/reviewer-seat receipt))
                         :constant-assertion
                         :seat-string-distinctness)
            notes (cond-> []
                    (= :constant-assertion seat-grade) (conj :r9/same-seat)
                    (and typed-present? (not= seat-grade typed-grade))
                    (conj :r9/persisted-seats-override-typed-field))]
        {:independence/grade seat-grade
         :grade-source :persisted-seats
         :notes notes})

      typed-present?
      (if (contains? independence-grades typed-grade)
        {:independence/grade typed-grade
         :grade-source :typed-field}
        (throw (ex-info "Unknown receipt independence grade"
                        {:error/code :r9/independence-grade-vocabulary-invalid
                         :actual typed-grade
                         :known independence-grades})))

      :else
      {:independence/grade :ungradeable-legacy
       :grade-source :legacy-boolean})))
