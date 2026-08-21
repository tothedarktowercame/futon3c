(ns futon3c.agency.followup-queue
  "Durable typed external followups. This is not the parked-turn queue."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.dev.config :as config])
  (:import [java.util UUID]))

(def ^:private default-path "/tmp/futon3c-followups.edn")
(def ^:private lease-ms 90000)
(defonce ^:private !state (atom nil))

(defn- path [] (or (config/env "FUTON3C_FOLLOWUP_PATH") default-path))
(defn- empty-state [] {:queued {} :leased {} :terminal {} :dedupe {}})
(defn- load-state []
  (let [f (io/file (path))]
    (if (.exists f)
      (let [x (edn/read-string (slurp f))]
        (if (map? x) (merge (empty-state) x) (empty-state)))
      (empty-state))))
(defn- ensure! [] (when-not @!state (reset! !state (load-state))) @!state)
(defn- persist! [s] (spit (path) (pr-str s)) s)
(defn clear! [] (reset! !state (empty-state)) (persist! @!state))
(defn snapshot [] (ensure!) @!state)
(defn- seat-key [agent session] [(str agent) (str session)])

(defn enqueue!
  [{:keys [agent session type dedupe-key prompt metadata]}]
  (ensure!)
  (when-not (= :inbox-zero type)
    (throw (ex-info "Unsupported followup type" {:type type})))
  (when-not (and (string? agent) (not (str/blank? agent))
                 (string? session) (not (str/blank? session))
                 (string? prompt) (not (str/blank? prompt)) dedupe-key)
    (throw (ex-info "Followup requires agent, session, prompt, and dedupe-key" {})))
  (let [existing (get-in @!state [:dedupe dedupe-key])]
    (if existing
      {:id existing :status :deduplicated}
      (let [id (str "followup-" (UUID/randomUUID))
            item {:followup-id id :agent (str agent) :session (str session)
                  :type type :dedupe-key dedupe-key :prompt prompt
                  :metadata metadata :created-at-ms (System/currentTimeMillis)}]
        (swap! !state #(-> %
                           (update-in [:queued (seat-key agent session)] (fnil conj []) item)
                           (assoc-in [:dedupe dedupe-key] id)))
        (persist! @!state)
        {:id id :status :queued}))))

(defn cancel! [id reason]
  (ensure!)
  (let [found (atom nil)]
    (swap! !state
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
                     (assoc-in [:terminal id] (assoc item :state :cancelled :reason reason)))
                 s))))
    (persist! @!state)
    (boolean @found)))

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
  (ensure!)
  (let [now (System/currentTimeMillis)
        key (seat-key agent session)
        leased (atom nil)]
    (swap! !state
           (fn [s]
             (loop [s (requeue-expired s now)]
               (if-let [item (first (get-in s [:queued key]))]
                 (let [rest-items (subvec (get-in s [:queued key]) 1)]
                   (if (valid? item)
                     (let [item* (assoc item :lease-deadline-ms (+ now lease-ms))]
                       (reset! leased item*)
                       (-> s (assoc-in [:queued key] rest-items)
                           (assoc-in [:leased (:followup-id item)] item*)))
                     (recur (-> s (assoc-in [:queued key] rest-items)
                                (assoc-in [:terminal (:followup-id item)]
                                          (assoc item :state :cancelled
                                                      :reason :revalidation-failed))))))
                 s))))
    (persist! @!state)
    @leased))

(defn ack! [id]
  (ensure!)
  (let [item (get-in @!state [:leased id])]
    (when item
      (swap! !state #(-> % (update :leased dissoc id)
                           (assoc-in [:terminal id] (assoc item :state :acked))))
      (persist! @!state)
      true)))
