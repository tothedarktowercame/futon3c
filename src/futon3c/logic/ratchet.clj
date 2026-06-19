(ns futon3c.logic.ratchet
  "Coverage ratchet for the structural-law inventory.

   Meta-invariant statement (`I-coverage-ratchet`):

     No operational-family entry's `:status` decreases without a
     corresponding `:event :family-demoted` evidence entry pointing
     to that family with the same from→to status pair.

   Status ordering (strongest to weakest):

     :operational
     :operational-but-bypassable
     :operational-when-enabled
     :candidate
     :violated

   Moves DOWN this ordering are demotions and require a recorded
   demotion-event in evidence; moves UP are graduations (good) and
   do not. Demotion events are appended through the evidence boundary
   (`futon3c.evidence.boundary/append!`), so they themselves bind
   `I-evidence-per-turn` and `I-single-boundary`.

   This namespace exists so the bot-evidence-class-of-bug — where an
   agent silently reduced coverage by silencing a noisy site without
   recording the reduction — becomes structurally surfaceable. Any
   inventory mutation that decreases status without an accompanying
   demotion-event is a I-coverage-ratchet violation, surfaced at:

     (a) inventory-load time (boot check)
     (b) pre-commit time (script in scripts/check-coverage-ratchet)
     (c) on-demand validation (`unreconciled-demotions`)

   Mission: M-invariant-queue-unstuck (futon3c/holes/missions/)."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as store]
            [futon3c.logic.inventory :as inventory]))

(def I-coverage-ratchet
  "Canonical statement of the coverage-ratchet invariant. Grep-verifiable."
  (str "I-coverage-ratchet: every reduction in an operational-family entry's "
       ":status must be matched by a :family-demoted evidence entry that "
       "names the family-id, the from-status, and the to-status. Increases "
       "(graduations) are unrestricted. The check runs at inventory-load time, "
       "at pre-commit time, and on demand."))

;; ---------------------------------------------------------------------------
;; Status ordering — strongest to weakest. Index = rank (lower = stronger).
;; ---------------------------------------------------------------------------

(def status-ordering
  "Strength ordering of family :status values, from strongest to weakest."
  [:operational
   :operational-but-bypassable
   :operational-when-enabled
   :candidate
   :violated])

(defn- status-rank
  "Rank a status (lower = stronger). Unknown statuses return -1."
  [status]
  (let [idx (.indexOf ^java.util.List status-ordering status)]
    (if (neg? idx) Long/MAX_VALUE idx)))

(defn status-decreased?
  "True iff NEW-STATUS is weaker than OLD-STATUS (moved down the ordering).
   Returns false when either status is missing or unknown, or when statuses
   are equal. Strict boolean."
  [old-status new-status]
  (boolean
   (and old-status
        new-status
        (not (.equals ^Object old-status new-status))
        (> (status-rank new-status) (status-rank old-status)))))

(defn family-status-map
  "Convert a sequence of family maps into {family-id status}.
   Family maps are produced by `inventory/extract-all-families`."
  [families]
  (->> families
       (keep (fn [{:keys [id status]}]
               (when (and id status)
                 [id status])))
       (into {})))

(defn diff-family-statuses
  "Compare two family-id → status maps. Returns:

     {:demotions [{:family-id :from :to} ...]
      :promotions [{:family-id :from :to} ...]
      :added [{:family-id :status} ...]
      :removed [{:family-id :status} ...]
      :unchanged [family-id ...]}

   Only :demotions are I-coverage-ratchet-relevant; the others are
   informational."
  [old-statuses new-statuses]
  (let [old-keys (set (keys old-statuses))
        new-keys (set (keys new-statuses))
        common (clojure.set/intersection old-keys new-keys)
        added (clojure.set/difference new-keys old-keys)
        removed (clojure.set/difference old-keys new-keys)]
    {:demotions (->> common
                     (keep (fn [k]
                             (let [from (get old-statuses k)
                                   to (get new-statuses k)]
                               (when (status-decreased? from to)
                                 {:family-id k :from from :to to}))))
                     vec)
     :promotions (->> common
                      (keep (fn [k]
                              (let [from (get old-statuses k)
                                    to (get new-statuses k)]
                                (when (and (not (.equals ^Object from to))
                                           (< (status-rank to) (status-rank from)))
                                  {:family-id k :from from :to to}))))
                      vec)
     :added (->> added
                 (mapv (fn [k] {:family-id k :status (get new-statuses k)})))
     :removed (->> removed
                   (mapv (fn [k] {:family-id k :status (get old-statuses k)})))
     :unchanged (->> common
                     (filter (fn [k]
                               (.equals ^Object
                                        (get old-statuses k)
                                        (get new-statuses k))))
                     vec)}))

;; ---------------------------------------------------------------------------
;; Demotion-event evidence shape
;; ---------------------------------------------------------------------------

(defn emit-demotion-event!
  "Emit a demotion-event evidence entry through the boundary.

   Required keys: :family-id :from :to.
   Optional: :reason :commit :authored-by.

   Returns the boundary's delivery-receipt-shaped result. The entry's
   :evidence/tags include [:invariant-queue :coverage-ratchet :family-demoted]
   plus the family-id as a keyword, so query-by-tag finds it."
  [evidence-store {:keys [family-id from to reason commit authored-by]}]
  (boundary/append!
   evidence-store
   {:subject {:ref/type :pattern
              :ref/id (str "structural-law-family/" (name family-id))}
    :type :coordination
    :claim-type :correction
    :author (or authored-by "ratchet")
    :body {:event :family-demoted
           :family-id family-id
           :from from
           :to to
           :reason reason
           :commit commit
           :invariant I-coverage-ratchet}
    :tags (cond-> [:invariant-queue :coverage-ratchet :family-demoted]
            (keyword? family-id) (conj family-id))}))

(defn matches-demotion?
  "True iff EVIDENCE-ENTRY is a :family-demoted event whose body's
   family-id, from, and to match the DEMOTION map."
  [{:keys [family-id from to]} evidence-entry]
  (let [body (:evidence/body evidence-entry)]
    (and (= :family-demoted (:event body))
         (= family-id (:family-id body))
         (= from (:from body))
         (= to (:to body)))))

(defn query-demotion-events
  "Return all :family-demoted evidence entries for FAMILY-ID, newest first."
  [evidence-store family-id]
  (->> (store/query* evidence-store
                     (cond-> {:query/type :coordination
                              :query/claim-type :correction}
                       (keyword? family-id) (assoc :query/tags [family-id])))
       (filter (fn [e]
                 (= :family-demoted (get-in e [:evidence/body :event]))))))

(defn unreconciled-demotions
  "Return the subset of DEMOTIONS that have NO matching :family-demoted
   evidence entry in the store. An empty result means the ratchet holds."
  [evidence-store demotions]
  (->> demotions
       (remove (fn [demotion]
                 (some #(matches-demotion? demotion %)
                       (query-demotion-events evidence-store
                                              (:family-id demotion)))))
       vec))

;; ---------------------------------------------------------------------------
;; Inventory snapshot helpers
;; ---------------------------------------------------------------------------

(defn current-inventory-statuses
  "Read the on-disk inventory (from PATH; default the package resource) and
   return the family-id → status map. Returns nil when the inventory cannot
   be parsed or contains no families."
  ([] (current-inventory-statuses nil))
  ([path]
   (try
     (let [inv (if path
                 (-> path slurp inventory/parse-sexp-string)
                 (inventory/load-inventory))
           parsed (if path inv (:parsed inv))
           families (when parsed (inventory/extract-all-families parsed))]
       (when (seq families)
         (family-status-map families)))
     (catch Throwable _ nil))))

(defn git-baseline-statuses
  "Read the inventory from the given git REF (default HEAD) and return the
   family-id → status map. Returns nil when git can't be invoked or the
   ref doesn't contain the inventory file."
  ([] (git-baseline-statuses "HEAD"))
  ([ref]
   (git-baseline-statuses
    ref
    "docs/structural-law-inventory.sexp"))
  ([ref path]
   (try
     (let [{:keys [exit out]}
           (shell/sh "git" "show" (str ref ":" path))]
       (when (zero? exit)
         (let [parsed (inventory/parse-sexp-string out)
               families (inventory/extract-all-families parsed)]
           (when (seq families)
             (family-status-map families)))))
     (catch Throwable _ nil))))

;; ---------------------------------------------------------------------------
;; Validation entry points
;; ---------------------------------------------------------------------------

(defn validate-against-baseline
  "Compare CURRENT-STATUSES (family-id → status map) against BASELINE-STATUSES.
   Query EVIDENCE-STORE for matching :family-demoted events. Return:

     {:ok? <bool>
      :demotions <all detected status decreases>
      :unreconciled <demotions without a matching evidence entry>
      :promotions :added :removed :unchanged <informational>}

   :ok? is true iff :unreconciled is empty (all demotions have evidence).
   When :ok? is false, :unreconciled lists the I-coverage-ratchet violations."
  [evidence-store baseline-statuses current-statuses]
  (let [diff (diff-family-statuses baseline-statuses current-statuses)
        unreconciled (unreconciled-demotions evidence-store (:demotions diff))]
    (assoc diff
           :ok? (empty? unreconciled)
           :unreconciled unreconciled
           :invariant I-coverage-ratchet)))

(defn check-pre-commit
  "Validate the working-tree inventory against the HEAD baseline using EVIDENCE-STORE.
   Returns the same shape as `validate-against-baseline`."
  [evidence-store]
  (let [baseline (git-baseline-statuses)
        current (current-inventory-statuses
                 "docs/structural-law-inventory.sexp")]
    (cond
      (nil? baseline)
      {:ok? true
       :reason "no-baseline (HEAD has no inventory file or git unavailable)"
       :invariant I-coverage-ratchet}

      (nil? current)
      {:ok? false
       :reason "current inventory could not be parsed"
       :invariant I-coverage-ratchet}

      :else
      (validate-against-baseline evidence-store baseline current))))

(defn print-violation-report
  "Print a structured violation report to *err*. Returns the same MAP it
   was given (so callers can chain). Used by the pre-commit script and
   by the load-time check."
  [{:keys [ok? unreconciled demotions promotions reason] :as result}]
  (binding [*out* *err*]
    (cond
      (and ok? reason)
      (println (str "[ratchet] OK: " reason))

      ok?
      (do (println (str "[ratchet] OK: I-coverage-ratchet holds. "
                        (count demotions) " demotion(s) all reconciled. "
                        (count promotions) " promotion(s)."))
          (doseq [{:keys [family-id from to]} demotions]
            (println (str "         demoted (reconciled): " family-id
                          " " from " → " to))))

      :else
      (do (println (str "[ratchet] I-coverage-ratchet VIOLATION: "
                        (count unreconciled) " unreconciled demotion(s):"))
          (doseq [{:keys [family-id from to]} unreconciled]
            (println (str "         family-id=" family-id
                          " from=" from " to=" to
                          " — no :family-demoted evidence entry found."))))))
  result)

(defn -main
  "Entry point for the pre-commit script. Exits with status 0 (ok) or 1
   (violation). Reads the inventory from the working tree, the baseline
   from git HEAD, and queries the default evidence store."
  [& _args]
  (let [result (check-pre-commit @(requiring-resolve 'futon3c.evidence.store/!store))]
    (print-violation-report result)
    (System/exit (if (:ok? result) 0 1))))

;; ---------------------------------------------------------------------------
;; Load-time check (mode (a) of the I-coverage-ratchet binding)
;; ---------------------------------------------------------------------------

(defn- emit-load-time-fired!
  "Emit a `:family-fired :family-id :coverage-ratchet` evidence entry
   recording the load-time check's outcome. Returns the boundary's receipt.

   Tagged with `:invariant-queue :family-canary :family-fired :load-time`
   plus the outcome and `:coverage-ratchet` so the operational-families
   view surfaces it alongside probe-sweep entries."
  [evidence-store {:keys [outcome detail]}]
  (boundary/append!
   evidence-store
   {:subject {:ref/type :pattern
              :ref/id "structural-law-family/coverage-ratchet"}
    :type :coordination
    :claim-type :observation
    :author "ratchet/check-on-load!"
    :body {:event :family-fired
           :family-id :coverage-ratchet
           :outcome outcome
           :detail (or detail {})
           :invariant I-coverage-ratchet
           :at (str (java.time.Instant/now))}
    :tags (cond-> [:invariant-queue :family-canary :family-fired
                   :load-time :coverage-ratchet]
            (keyword? outcome) (conj outcome))}))

(defn check-on-load!
  "Run the coverage-ratchet check at JVM load time and emit the outcome
   as a `:family-fired` evidence entry through the boundary.

   Mirrors `check-pre-commit` (working-tree vs git HEAD) but emits a
   live-state record instead of printing-and-exiting. The boot path
   surfaces violations loudly via `print-violation-report` but does NOT
   abort boot — the ratchet's job is to make degradation visible, not
   to block startup. (Runtime can still degrade safely; the visible
   :family-fired :violation evidence entry is the surfacing.)

   Options:
     :emit?  default true — set false to skip evidence emission (tests).
     :print? default true — set false to suppress the human-readable banner.

   Returns the validation result map (same shape as `validate-against-baseline`)
   with an extra `:emit-receipt` key when emission was attempted."
  ([evidence-store] (check-on-load! evidence-store {}))
  ([evidence-store {:keys [emit? print?]
                    :or {emit? true print? true}}]
   (let [result (check-pre-commit evidence-store)
         outcome (cond
                   (and (:ok? result) (:reason result)) :inactive
                   (:ok? result) :ok
                   :else :violation)
         detail (cond
                  (= outcome :violation)
                  {:unreconciled-count (count (:unreconciled result))
                   :unreconciled (:unreconciled result)}

                  (= outcome :inactive)
                  {:reason (:reason result)}

                  :else
                  {:demotions-reconciled (count (:demotions result))
                   :promotions (count (:promotions result))
                   :added (count (:added result))
                   :removed (count (:removed result))})
         receipt (when emit?
                   (try
                     (emit-load-time-fired!
                      evidence-store
                      {:outcome outcome :detail detail})
                     (catch Throwable t
                       {:ok false
                        :error/code :emit-failed
                        :error/exception (str (.getName (class t)) ": "
                                              (.getMessage t))})))]
     (when print?
       (cond
         (= outcome :ok)
         (println (str "[ratchet] I-coverage-ratchet load-time check: OK ("
                       (:demotions-reconciled detail) " demotion(s) reconciled, "
                       (:promotions detail) " promotion(s))"))

         (= outcome :inactive)
         (println (str "[ratchet] I-coverage-ratchet load-time check: INACTIVE ("
                       (:reason detail) ")"))

         :else
         (do
           (println "================================================================")
           (println "[ratchet] I-coverage-ratchet LOAD-TIME CHECK FAILED")
           (println (str "          unreconciled demotions: "
                         (:unreconciled-count detail)))
           (doseq [{:keys [family-id from to]} (:unreconciled detail)]
             (println (str "          family-id=" family-id
                           " from=" from " to=" to
                           " — no :family-demoted evidence entry found.")))
           (println "          Boot continues; the violation is recorded as evidence.")
           (println "================================================================"))))
     (cond-> result
       receipt (assoc :emit-receipt receipt)
       true (assoc :outcome outcome)))))
