(ns futon3c.agency.warrant
  "The warrant rides the handoff (futon3/library/test-registry/
  warrant-rides-the-handoff.flexiarg). Pure namespace — no stack
  dependencies — so its tests run under the :test-pure alias.

  Three pure functions:
  - normalize-warrants: validate the invoke payload's optional
    \"warrants\" vector into a typed {:handoff/warrant-status ...} map,
    refusing malformed elements instead of dropping them.
  - render-warrant-lines: the delivered turn's Warrants header lines,
    so the reviewer meets the warrant without searching.
  - reviewer-lane: rerun-when-the-warrant-fails — the reviewer's lane
    (:full-rerun or :spot-check) with its reason keyword.

  Validation of the registry record itself is the reviewer's `check`
  (bind-warrant-to-the-diff); nothing here fetches evidence records."
  (:require [clojure.string :as str]))

(def ^:private entry-id-pattern #"^test-registry-[0-9a-f]{64}$")
(def ^:private base-sha-pattern #"^[0-9a-f]{7,40}$")
(def lanes #{:routine :pre-push :invariant})

(defn- field
  "Payload maps may arrive with string keys (JSON) or keyword keys."
  [m k]
  (let [v (get m k)]
    (if (nil? v) (get m (name k)) v)))

(defn- refusal [field value]
  {:handoff/refusal :warrant-invalid :field field :value value})

(defn normalize-warrants
  "RAW is the invoke payload's optional \"warrants\" value: a vector of
  maps {entry-id namespace lane base-sha}. Returns
  {:handoff/warrant-status :warranted :warrants [...]} for a non-empty
  valid vector; {:handoff/warrant-status :unwarranted} when absent or
  empty; and a typed refusal {:handoff/refusal :warrant-invalid
  :field ... :value ...} for any malformed element. Never silently
  drops an element."
  [raw]
  (cond
    (or (nil? raw) (and (sequential? raw) (empty? raw)))
    {:handoff/warrant-status :unwarranted}

    (not (vector? raw))
    (refusal :warrants raw)

    :else
    (reduce
     (fn [acc w]
       (if (:handoff/refusal acc)
         (reduced acc)
         (let [entry-id (field w :entry-id)
               ns-id (field w :namespace)
               raw-lane (field w :lane)
               lane (if (keyword? raw-lane) raw-lane
                        (when (string? raw-lane) (keyword raw-lane)))
               base (field w :base-sha)]
           (cond
             (not (and (string? entry-id) (re-find entry-id-pattern entry-id)))
             (refusal :entry-id entry-id)

             (not (and (string? ns-id) (not (str/blank? ns-id))))
             (refusal :namespace ns-id)

             (not (contains? lanes lane))
             (refusal :lane raw-lane)

             (not (and (string? base) (re-find base-sha-pattern base)))
             (refusal :base-sha base)

             :else
             (update acc :warrants conj {:entry-id entry-id
                                         :namespace ns-id
                                         :lane lane
                                         :base-sha base})))))
     {:handoff/warrant-status :warranted :warrants []}
     raw)))

(defn render-warrant-lines
  "Header lines for the delivered turn. WARRANTED is the result of
  normalize-warrants."
  [{:keys [handoff/warrant-status warrants] :as _normalized}]
  (if (= :warranted warrant-status)
    (apply str
           "Warrants: :warranted (" (count warrants) ")\n"
           (map (fn [{:keys [entry-id namespace lane base-sha]}]
                  (str "  " entry-id
                       " ns=" namespace
                       " lane=" (name lane)
                       " base=" base-sha "\n"))
                warrants))
    ;; The only other non-refusal status is :unwarranted; refusals are
    ;; answered with HTTP 400 before a turn is ever delivered.
    "Warrants: :unwarranted — no test-registry warrant; reviewer lane is full rerun of the declared namespaces\n"))

(defn- any-mandatory-lane?
  [warrant]
  (boolean (some #(contains? #{:pre-push :invariant} (:lane %))
                 (:warrants warrant))))

(defn reviewer-lane
  "rerun-when-the-warrant-fails: the reviewer's lane for the namespaces
  covered by WARRANT (normalize-warrants result). CHECK-RESULT is the
  registry `check` outcome (relied on only as {:warrant? true}).
  TESTS-CHANGED? true or nil (undeclared) means the first-run rule
  applies. Returns {:lane ... :reason ...}; :full-rerun when there is
  no warrant, the check is not {:warrant? true}, any warrant declares a
  :pre-push/:invariant lane, or tests changed (or their change-status
  was left undeclared); otherwise :spot-check."
  [warrant check-result tests-changed?]
  (let [reason (cond
                 (or (nil? warrant)
                     (= :unwarranted (:handoff/warrant-status warrant)))
                 :no-warrant

                 (not= {:warrant? true} check-result)
                 :warrant-check-failed

                 (any-mandatory-lane? warrant)
                 :mandatory-lane

                 (or (true? tests-changed?) (nil? tests-changed?))
                 (if (nil? tests-changed?)
                   :tests-changed-undeclared
                   :tests-changed)

                 :else nil)]
    {:lane (if reason :full-rerun :spot-check)
     :reason (or reason :warrant-valid-routine)}))
