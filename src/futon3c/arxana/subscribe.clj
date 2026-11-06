(ns futon3c.arxana.subscribe
  "D3 subscription projection (INSTANTIATE increment 6).

  Opening a subscription returns a subscription ID and a snapshot cursor;
  events strictly after the snapshot are delivered by poll. Delivery is
  at-least-once: poll replays from the last acked cursor and clients
  deduplicate by :event/id. Acks advance the cursor. Resume reopens with the
  acked cursor within retention; an expired or unknown cursor returns
  `resync-required`, causing a fresh snapshot rather than guessed replay.

  Subscriptions are latency/projection mechanisms, never correctness
  prerequisites: a disconnected client still receives stale refusal on
  guarded writes (D3)."
  (:require [futon3c.arxana.digest :as digest]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.journal :as journal]))

(defn subscription-id
  ^String [principal topics snapshot-cursor]
  (str "arxana:subscription:sha256:"
       (digest/canonical-digest {:principal/id principal
                                 :topics (vec (sort topics))
                                 :snapshot/cursor snapshot-cursor})))

(defrecord Subscriptions [journal state])

(defn subscriptions
  "Subscription registry over a journal."
  [journal]
  (->Subscriptions journal (atom {})))

(defn- matches? [sub-topics event]
  (or (empty? sub-topics)
      (contains? sub-topics (:topic event))))

(defn open
  "subscription/open. Body: {:topics [...] :resume/cursor n?}. Returns
  {:ok {:subscription/id .. :snapshot/cursor ..}} or a typed error. Resume
  with an expired/unknown cursor is :stale + resync-required."
  [subs {:keys [topics resume/cursor principal/id]}]
  (let [topics (set topics)
        unknown (remove journal/topics topics)]
    (cond
      (seq unknown)
      {:error (env/error :invalid :arxana.subscribe/unknown-topic
                         {:unknown (vec (sort unknown))
                          :known (vec (sort journal/topics))} nil)}

      (some? cursor)
      (let [{:keys [ok error]} (journal/events-after (:journal subs) cursor)]
        (if error
          {:error error}
          (let [sid (subscription-id id topics cursor)]
            (swap! (:state subs) assoc sid
                   {:subscription/id sid
                    :topics topics
                    :acked cursor
                    :principal/id id})
            {:ok {:subscription/id sid
                  :snapshot/cursor cursor
                  :resumed? true
                  :pending (count (filter #(matches? topics %) (:events ok)))}})))

      :else
      (let [snap (journal/head-cursor (:journal subs))
            sid (subscription-id id topics snap)]
        (swap! (:state subs) assoc sid
               {:subscription/id sid
                :topics topics
                :acked snap
                :principal/id id})
        {:ok {:subscription/id sid
              :snapshot/cursor snap
              :resumed? false
              :pending 0}}))))

(defn poll
  "Deliver events strictly after the subscription's acked cursor, filtered by
  topic. At-least-once: unacked events are re-delivered; clients dedupe by
  :event/id. Unknown subscription is :missing."
  [subs {:keys [subscription/id]}]
  (if-let [sub (get @(:state subs) id)]
    (let [{:keys [ok error]} (journal/events-after (:journal subs) (:acked sub))]
      (if error
        {:error error}
        {:ok {:subscription/id id
              :events (vec (filter #(matches? (:topics sub) %) (:events ok)))
              :head (:head ok)}}))
    {:error (env/error :missing :arxana.subscribe/unknown-subscription
                       {:subscription/id id}
                       [:subscription/open])}))

(defn ack
  "subscription/ack: advance the acked cursor. Acking ahead of delivery is
  invalid; acking at/behind the current ack is an idempotent no-op."
  [subs {:keys [subscription/id cursor]}]
  (if-let [sub (get @(:state subs) id)]
    (cond
      (not (integer? cursor))
      {:error (env/error :invalid :arxana.subscribe/malformed-cursor
                         {:cursor cursor} nil)}

      (> cursor (journal/head-cursor (:journal subs)))
      {:error (env/error :invalid :arxana.subscribe/ack-beyond-head
                         {:cursor cursor
                          :head (journal/head-cursor (:journal subs))} nil)}

      :else
      (let [acked (max (:acked sub) cursor)]
        (swap! (:state subs) assoc-in [id :acked] acked)
        {:ok {:subscription/id id :acked acked}}))
    {:error (env/error :missing :arxana.subscribe/unknown-subscription
                       {:subscription/id id}
                       [:subscription/open])}))
