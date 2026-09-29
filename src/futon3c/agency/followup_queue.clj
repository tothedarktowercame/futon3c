(ns futon3c.agency.followup-queue
  "Durable typed external followups. This is not the parked-turn queue."
  (:require [clojure.string :as str]
            [futon3c.agency.atomic-file :as atomic-file]
            [futon3c.dev.config :as config]
            [futon3c.agency.promise-record :as promise-record]
            [futon3c.agency.promise-history :as history]
            [futon3c.agency.promise-capture :as capture])
  (:import [java.util UUID]))

(def ^:private default-path "/tmp/futon3c-followups.edn")
(def ^:dynamic *path-override* nil)
(def ^:private lease-ms 90000)
(defonce ^:private !state (atom nil))

(defn- path [] (or *path-override* (config/env "FUTON3C_FOLLOWUP_PATH") default-path))
(defn- empty-state [] {:queued {} :leased {} :terminal {} :dedupe {}
                       :history-outbox {}})
(defn- migrate-dedupe [s]
  (let [outstanding (into #{} (concat (map :followup-id (mapcat val (:queued s)))
                                        (keys (:leased s))))]
    (update s :dedupe #(into {} (filter (fn [[_ id]] (contains? outstanding id)) %)))))
(defn- load-state []
  (migrate-dedupe
   (merge (empty-state)
          (atomic-file/load-edn-map! :followup (path) (empty-state)))))
(defn- persist! [s] (atomic-file/write! (path) (pr-str s)) s)
(defn- ensure! []
  (history/capture! :followup (fn []
  (when-not @!state
    (capture/reset-state! :followup !state (persist! (load-state)))
    (history/register-pending! (vals (:history-outbox @!state)))
    (when (seq (:history-outbox @!state)) (history/drain! !state persist!)))
  @!state)))
(defn clear! []
  (history/capture! :followup (fn [] (capture/reset-state! :followup !state (empty-state)) (persist! @!state))))
(defn snapshot [] (ensure!) (dissoc @!state :history-outbox))
(defn- seat-key [agent session] [(str agent) (str session)])
(defn- release-dedupe [s item]
  (if item (update s :dedupe dissoc (:dedupe-key item)) s))

(defn- history-items [state]
  (into {} (concat (for [item (mapcat val (:queued state))]
                     [(:followup-id item) [:queued item]])
                   (for [[id item] (:leased state)] [id [:leased item]])
                   (for [[id item] (:terminal state)] [id [:terminal item]]))))

(defn- update-state! [f & [requeue-at]]
  (let [[old new] (capture/swap-vals-state! :followup !state f)
        now (System/currentTimeMillis)
        before (history-items old)
        requeued (into {} (filter (fn [[_ item]]
                                   (and requeue-at (>= requeue-at (:lease-deadline-ms item))))
                                 (:leased old)))
        allocator (history/allocator-snapshot)
        transitions
        (vec (for [[id [state item]] (history-items new)
                   :let [[prior prior-item] (get before id)]
                   :when (and (or (not= prior state) (not= prior-item item))
                              (not (and (= :queued state) (contains? requeued id))))]
               [(case state
                  :queued (if prior :promise/followup-requeued :promise/followup-enqueued)
                  :leased :promise/followup-dequeued
                  :terminal :promise/followup-terminal)
                item]))
        all-transitions
        (vec (concat
              (for [[_ item] requeued]
                [:promise/followup-requeued item requeue-at])
              (for [[type item] transitions] [type item now])))
        eids (atom [])]
    ;; Staging belongs to the same rollback region as the authoritative write.
    (try
      (doseq [[type item event-ms] all-transitions]
        (swap! eids conj
               (history/stage! !state type item event-ms
                               (select-keys item [:state :reason]))))
      (persist! @!state)
      (catch Throwable e
        (reset! !state old)
        (history/release-reservations! @eids)
        (history/restore-allocator! allocator)
        (capture/drain!)
        (throw e)))
    (when history/*after-outbox-persist* (history/*after-outbox-persist*))
    (history/drain! !state persist!)
    new))

(defn enqueue!
  "Queue a followup with optional validated :beneficiary, :deadline and
   :fulfilment-criterion promise metadata; these do not affect delivery."
  [{:keys [agent session type dedupe-key prompt metadata] :as request}]
  (history/capture! :followup (fn []
  (ensure!)
  (when-not (contains? #{:inbox-zero :apm-store-repair :kimi-work-target} type)
    (throw (ex-info "Unsupported followup type" {:type type})))
  (when-not (and (string? agent) (not (str/blank? agent))
                 (string? session) (not (str/blank? session))
                 (string? prompt) (not (str/blank? prompt)) dedupe-key)
    (throw (ex-info "Followup requires agent, session, prompt, and dedupe-key" {})))
  (let [promise-fields (promise-record/fields request)
        existing (get-in @!state [:dedupe dedupe-key])]
    (if existing
      {:id existing :status :deduplicated}
      (let [id (str "followup-" (UUID/randomUUID))
            item (merge promise-fields
                        {:followup-id id :agent (str agent) :session (str session)
                  :type type :dedupe-key dedupe-key :prompt prompt
                  :metadata metadata :created-at-ms (System/currentTimeMillis)})]
        (update-state! #(-> %
                           (update-in [:queued (seat-key agent session)] (fnil conj []) item)
                           (assoc-in [:dedupe dedupe-key] id)))
        {:id id :status :queued}))))))

(defn cancel! [id reason]
  (history/capture! :followup (fn []
  (ensure!)
  (let [found (atom nil)]
    (update-state!
           (fn [s]
             (let [queued (into {}
                                (map (fn [[k xs]]
                                       [k (vec (remove (fn [x]
                                                        (when (= id (:followup-id x))
                                                          (reset! found x))
                                                        (= id (:followup-id x))) xs))]))
                                (:queued s))
                   leased-item (get-in s [:leased id])
                   item (or @found leased-item)]
               (when leased-item (reset! found leased-item))
               (if item
                 (-> s
                     (assoc :queued queued)
                     (update :leased dissoc id)
                     (release-dedupe item)
                     (assoc-in [:terminal id] (assoc item :state :cancelled :reason reason)))
                 s))))
    (boolean @found)))))

(defn- requeue-expired [s now]
  (reduce (fn [acc [id item]]
            (if (>= now (:lease-deadline-ms item))
              (-> acc
                  (update-in [:queued (seat-key (:agent item) (:session item))]
                             #(into [item] (or % [])))
                  (update :leased dissoc id))
              acc))
          s (:leased s)))

(defn lease-one!
  "Lease one item. VALID? revalidates exact identity immediately before lease;
  invalid items become terminal cancelled records."
  [agent session valid?]
  (history/capture! :followup (fn []
  (ensure!)
  (let [now (System/currentTimeMillis)
        key (seat-key agent session)
        leased (atom nil)]
    (update-state!
           (fn [s]
             (loop [s (requeue-expired s now)]
               (if-let [item (first (get-in s [:queued key]))]
                 (let [rest-items (subvec (get-in s [:queued key]) 1)
                       validity (valid? item)]
                   (if (true? validity)
                    (let [item* (assoc item :lease-deadline-ms (+ now lease-ms))]
                      (reset! leased item*)
                      (-> s (assoc-in [:queued key] rest-items)
                          (assoc-in [:leased (:followup-id item)] item*)))
                    (recur (-> s (assoc-in [:queued key] rest-items)
                               (release-dedupe item)
                               (assoc-in [:terminal (:followup-id item)]
                                         (assoc item :state :cancelled
                                                     :reason (if (keyword? validity)
                                                               validity
                                                               :revalidation-failed)))))))
                 s))) now)
    @leased))))

(defn ack! [id]
  (history/capture! :followup (fn []
  (ensure!)
  (let [item (get-in @!state [:leased id])]
    (when item
      (update-state! #(-> % (update :leased dissoc id)
                           (release-dedupe item)
                           (assoc-in [:terminal id] (assoc item :state :acked))))
      true)))))
