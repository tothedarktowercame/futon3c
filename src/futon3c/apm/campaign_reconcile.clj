(ns futon3c.apm.campaign-reconcile
  "Pure reconciliation between a certified campaign projection and runtime facts.

  Reconciliation never repairs state. Missing/expired observations yield
  :stale; contradictory observations yield :conflict; only independently
  agreeing current observations yield :valid."
  (:require [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine])
  (:import [java.time Duration Instant]))

(def terminal-job-states #{:done :failed :error :cancelled})

(defn- nonblank? [x]
  (and (string? x) (not (str/blank? x))))

(defn- parse-instant [x]
  (when (string? x)
    (try (Instant/parse x) (catch Throwable _ nil))))

(defn- finding [severity route code & [details]]
  (cond-> {:severity severity :route route :code code}
    details (assoc :details details)))

(defn- semantic-facts [facts]
  ;; Timestamps prove freshness. They are not a semantic state change: a later
  ;; observation of identical facts must not invalidate a step permit.
  (into {} (map (fn [[route fact]] [route (dissoc fact :observed-at)])) facts))

(defn- source-freshness [route source now max-age-ms]
  (cond
    (nil? source) [(finding :stale route :observation-missing)]
    (nil? (parse-instant (:observed-at source)))
    [(finding :stale route :observation-time-invalid)]
    (> (.toMillis (Duration/between (parse-instant (:observed-at source)) now))
       max-age-ms)
    [(finding :stale route :observation-expired
              {:observed-at (:observed-at source) :max-age-ms max-age-ms})]
    :else []))

(defn- value-findings [route expected actual fields]
  (keep (fn [field]
          (when (not= (get expected field) (get actual field))
            (finding :conflict route :value-mismatch
                     {:field field :expected (get expected field)
                      :actual (get actual field)})))
        fields))

(defn- registration-findings [active registration]
  (when (and active registration)
    (concat
     (value-findings :registration
                     {:frame-id (:frame-id active)
                      :problem-id (:problem-id active)
                      :registration-hash (:registration-hash active)
                      :harness-hash (:harness-hash active)}
                     registration
                     [:frame-id :problem-id :registration-hash :harness-hash])
     (when-not (nonblank? (:registration-hash registration))
       [(finding :stale :registration :registration-hash-missing)]))))

(defn- frame-record-findings [active frame-record]
  (when (and active frame-record)
    (concat
     (value-findings :frame-record
                     {:frame-id (:frame-id active)
                      :problem-id (:problem-id active)
                      :block-id (:block-id active)}
                     frame-record [:frame-id :problem-id :block-id])
     (when-not (contains? #{:open :running} (:status frame-record))
       [(finding :conflict :frame-record :active-frame-not-open
                 {:status (:status frame-record)})]))))

(defn- binding-findings [active binding ledger-digest]
  (when binding
    (if active
      ;; An opened frame is intentionally unbound until its preflight phase
      ;; validates and installs runtime bindings. If a binding is asserted it
      ;; must agree completely; absence is not a contradictory observation.
      (when (true? (:bound? binding))
        (concat
         (value-findings :binding {:frame-id (:frame-id active)}
                         binding [:frame-id])
         (cond
           (nil? (:ledger-digest binding))
           [(finding :stale :binding :binding-ledger-digest-missing)]

           (not= ledger-digest (:ledger-digest binding))
           [(finding :conflict :binding :value-mismatch
                     {:field :ledger-digest :expected ledger-digest
                      :actual (:ledger-digest binding)})]

           :else [])))
      (when (true? (:bound? binding))
        [(finding :conflict :binding :inactive-campaign-has-live-binding
                  {:frame-id (:frame-id binding)})]))))

(defn- duplicate-job-findings [items]
  (mapcat
   (fn [[job-id jobs]]
     (when (> (count (set (map #(select-keys % [:frame-id :state :role]) jobs))) 1)
       [(finding :conflict :jobs :job-id-contradiction
                 {:job-id job-id
                  :observations (mapv #(select-keys % [:frame-id :state :role]) jobs)})]))
   (group-by :job-id items)))

(defn- jobs-findings [active jobs]
  (when jobs
    (let [items (:items jobs)
          malformed (filter #(or (not (nonblank? (:job-id %)))
                                 (not (keyword? (:state %)))) items)
          live (remove #(contains? terminal-job-states (:state %)) items)]
      (concat
       (when-not (vector? items)
         [(finding :stale :jobs :job-snapshot-invalid)])
       (when (seq malformed)
         [(finding :stale :jobs :job-observation-invalid
                   {:count (count malformed)})])
       (duplicate-job-findings items)
       (if active
         (for [job live :when (not= (:frame-id active) (:frame-id job))]
           (finding :conflict :jobs :foreign-live-job
                    {:job-id (:job-id job) :frame-id (:frame-id job)}))
         (for [job live]
           (finding :conflict :jobs :live-job-without-active-frame
                    {:job-id (:job-id job) :frame-id (:frame-id job)})))))))

(defn- receipt-findings [active receipts]
  (when (and active receipts)
    (let [items (:items receipts)
          by-kind (group-by :kind items)]
      (mapcat
       (fn [kind]
         (let [matches (get by-kind kind)]
           (cond
             (empty? matches)
             [(finding :stale :receipts :required-receipt-missing {:kind kind})]

             (not= 1 (count matches))
             [(finding :conflict :receipts :required-receipt-ambiguous
                       {:kind kind :count (count matches)})]

             (not= (:frame-id active) (:frame-id (first matches)))
             [(finding :conflict :receipts :receipt-frame-mismatch
                       {:kind kind :expected (:frame-id active)
                        :actual (:frame-id (first matches))})]

             (not (nonblank? (:digest (first matches))))
             [(finding :stale :receipts :receipt-digest-missing {:kind kind})]

             :else [])))
       (:required-receipt-kinds active)))))

(defn reconcile
  "Reconcile PROJECTION with FACTS at NOW.

  FACTS routes are timestamped maps: :registration, :frame-record, :binding,
  :jobs {:items [...]}, and :receipts {:items [...]}. MAX-AGE-MS defaults to
  60 seconds. The returned certificate includes digests of both inputs."
  ([projection facts now] (reconcile projection facts now 60000))
  ([projection facts now max-age-ms]
   (cond
     (not= :valid (:projection/status projection))
     {:reconciliation/status :conflict
      :findings [(finding :conflict :ledger :ledger-projection-invalid
                          {:status (:projection/status projection)
                           :error/code (:error/code projection)})]}

     (not (instance? Instant now))
     {:reconciliation/status :stale
      :findings [(finding :stale :clock :reconciliation-clock-invalid)]}

     :else
     (let [active (:active/frame projection)
           ;; The campaign ledger is the registration and frame-record
           ;; authority. Legacy duplicate files are checked when supplied but
           ;; are not required to certify the ledger's own transition.
           required-routes (cond-> [:binding :jobs]
                             (and active (seq (:required-receipt-kinds active)))
                             (conj :receipts))
           freshness (mapcat #(source-freshness % (get facts %) now max-age-ms)
                             required-routes)
           findings (vec
                     (concat
                      freshness
                      (registration-findings active (:registration facts))
                      (frame-record-findings active (:frame-record facts))
                      (binding-findings active (:binding facts)
                                        (:ledger/digest projection))
                      (jobs-findings active (:jobs facts))
                      (receipt-findings active (:receipts facts))))
           status (cond
                    (some #(= :conflict (:severity %)) findings) :conflict
                    (seq findings) :stale
                    :else :valid)]
       {:reconciliation/status status
        :ledger/digest (:ledger/digest projection)
        :facts/digest (machine/ledger-digest [(semantic-facts facts)])
        :campaign/id (:campaign/id projection)
        :campaign/version (:campaign/version projection)
        :active/frame-id (:frame-id active)
        :observed-at (str now)
        :max-age-ms max-age-ms
        :findings findings}))))
