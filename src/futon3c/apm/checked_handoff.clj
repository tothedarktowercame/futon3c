(ns futon3c.apm.checked-handoff
  "Pure validation for typed checked-handoff verdict events.

   Independence is computed here. A dangling rerun-witness is downgraded to
   :constant-assertion with :r9/rerun-witness-unresolved; it can never receive
   :adjudicator-rerun-witnessed. The resolver is injected so this namespace
   has no evidence-store dependency.")

(def event-type :checked-handoff/verdict)

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
  (if (= (:author-seat event) (:worker-seat event))
    {:ok false
     :error/code :r9/worker-authored-verdict-refused
     :event (dissoc event :grade :independence/grade)}
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
