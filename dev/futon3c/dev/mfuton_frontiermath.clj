(ns futon3c.dev.mfuton-frontiermath
  "mfuton-owned FrontierMath live room seams. Keep math-lane-specific claim and
   Bell behavior out of the generic futon dev owner files."
  (:require [clojure.string :as str]
            [futon3c.agents.mfuton-prompt-override :as mfuton-prompt-override]
            [futon3c.mfuton-mode :as mfuton-mode]
            [futon3c.dev.config :as config])
  (:import [java.time Instant]))

(def ^:private fm-claim-prompt-re
  #"(?im)^([a-z0-9._-]+):\s+I['’]ll take\s+([A-Za-z0-9._:-]+)\s*$")

(def ^:private fm-bell-prompt-re
  #"(?im)(?:@?tickle:?[ \t]+)?BELL[ \t]+([A-Za-z0-9._:-]+)")

(def ^:private irc-surface-channel-re
  #"(?i)\[Surface:\s*IRC\s*\|\s*Channel:\s*(#[^ |\]]+)")

(def ^:private irc-surface-speaker-re
  #"(?i)\[Surface:\s*IRC\s*\|\s*Channel:\s*#[^|\]]+\|\s*Speaker:\s*([^ |\]]+)")

(def ^:private irc-runtime-channel-re
  #"(?im)^\s*-\s*Channel:\s*(#[^\s]+)\s*$")

(def ^:private irc-runtime-sender-re
  #"(?im)^\s*-\s*Sender:\s*([^\s]+)\s*$")

(defn dispatch-channel
  [math-irc-enabled?]
  (if math-irc-enabled? (config/frontiermath-room) "#futon"))

(defn current-frontiermath-room
  []
  (dispatch-channel (config/env-bool "MATH_IRC" false)))

(defn current-conductor-state-atom
  [fm-conductor-handle]
  (some-> fm-conductor-handle :conductor-state))

(defn claimed-obligations
  [conductor-state]
  (or (:claimed-obligations @conductor-state) {}))

(defn claimed-obligation-ids
  [conductor-state]
  (set (keys (claimed-obligations conductor-state))))

(defn unclaimed-assignable-obligations
  [problem-id conductor-state assignable-obligations-fn]
  (let [claimed-ids (if conductor-state
                      (claimed-obligation-ids conductor-state)
                      #{})]
    (remove #(contains? claimed-ids (:item/id %))
            (assignable-obligations-fn problem-id))))

(defn- claim-nick->agent-id
  [nick]
  (config/agent-id-for-irc-nick nick))

(defn- prompt-channel
  [prompt]
  (let [prompt-str (str (or prompt ""))]
    (or (some->> (re-find irc-surface-channel-re prompt-str)
                 second)
        (some->> (re-find irc-runtime-channel-re prompt-str)
                 second))))

(defn- prompt-speaker
  [prompt]
  (let [prompt-str (str (or prompt ""))]
    (or (some->> (re-find irc-surface-speaker-re prompt-str)
                 second)
        (some->> (re-find irc-runtime-sender-re prompt-str)
                 second))))

(defn- parse-fm-claim-prompt
  [prompt]
  (when-let [[_ nick obligation-id] (re-find fm-claim-prompt-re (str (or prompt "")))]
    {:nick (str/lower-case nick)
     :obligation-id obligation-id
     :channel (prompt-channel prompt)}))

(defn- parse-fm-bell-prompt
  [prompt]
  (when-let [[_ event] (re-find fm-bell-prompt-re (str (or prompt "")))]
    {:nick (some-> (prompt-speaker prompt) str/lower-case)
     :event event
     :channel (prompt-channel prompt)}))

(defn render-dispatch-message
  [original-message]
  (mfuton-prompt-override/maybe-fm-dispatch-message original-message))

(defn render-task-prompt
  [original-prompt]
  (mfuton-prompt-override/maybe-task-prompt original-prompt))

(defn handle-claim-prompt!
  [{:keys [prompt
           session-id
           conductor-state
           problem-id
           assignable-obligations-fn
           dispatch-message-original-fn]}]
  (when-let [{:keys [nick obligation-id channel]} (parse-fm-claim-prompt prompt)]
    (when (= (current-frontiermath-room) channel)
      (if conductor-state
        (let [agent-id (claim-nick->agent-id nick)
              claimed (get (claimed-obligations conductor-state) obligation-id)
              obligation (some #(when (= obligation-id (:item/id %)) %)
                               (unclaimed-assignable-obligations
                                problem-id conductor-state assignable-obligations-fn))]
          (cond
            (nil? agent-id)
            {:ok true
             :session-id session-id
             :result (str "@" nick " claim rejected: unknown agent mapping for " nick ".")}

            claimed
            {:ok true
             :session-id session-id
             :result (str "@" nick " " obligation-id " already claimed by "
                          (or (:nick claimed) (:agent-id claimed)) ".")}

            (nil? obligation)
            {:ok true
             :session-id session-id
             :result (str "@" nick " " obligation-id " is not currently assignable.")}

            :else
            (let [ob-label (:item/label obligation)
                  claim {:agent-id agent-id
                         :nick nick
                         :label ob-label
                         :claimed-at (str (Instant/now))}
                  original-msg (dispatch-message-original-fn nick obligation-id ob-label)
                  msg (render-dispatch-message original-msg)
                  now-ms (System/currentTimeMillis)]
              (swap! conductor-state
                     (fn [s]
                       (-> s
                           (assoc-in [:claimed-obligations obligation-id] claim)
                           (assoc-in [:last-paged agent-id] now-ms)
                           (update-in [:paged-obligations agent-id] (fnil conj #{}) obligation-id)
                           (assoc :last-cycle {:action :claim
                                               :target agent-id
                                               :obligation obligation-id
                                               :text msg})
                           (assoc :last-cycle-ms now-ms))))
              {:ok true
               :session-id session-id
               :result msg})))
        {:ok true
         :session-id session-id
         :result (str "@" nick " claim rejected: FM conductor is not running.")}))))

(defn handle-bell-prompt!
  [{:keys [prompt
           session-id
           conductor-state
           problem-id
           bridge-send-fn
           dispatch-mechanical-fn]}]
  (when-let [{:keys [nick event channel]} (parse-fm-bell-prompt prompt)]
    (when (= (current-frontiermath-room) channel)
      (if conductor-state
        (let [agent-id (claim-nick->agent-id nick)]
          (if (nil? agent-id)
            {:ok true
             :session-id session-id
             :result (str "BELL " event
                          " rejected: unknown agent mapping for " nick ".")}
            (let [dispatch-result (dispatch-mechanical-fn
                                   agent-id
                                   {:problem-id problem-id
                                    :bridge-send-fn bridge-send-fn
                                    :conductor-state conductor-state})
                  now-ms (System/currentTimeMillis)
                  result-text (case (:action dispatch-result)
                                :page (str "BELL " event
                                           " acknowledged for " nick
                                           ". Next work was posted to " (current-frontiermath-room) ".")
                                :pass (str "BELL " event
                                           " acknowledged for " nick
                                           ". No new assignable obligations right now.")
                                :cooldown (str "BELL " event
                                               " acknowledged for " nick
                                               ". No new dispatch during cooldown.")
                                :skip (str "BELL " event
                                           " acknowledged for " nick
                                           ". You're not idle for a new assignment yet.")
                                (str "BELL " event
                                     " acknowledged for " nick "."))]
              (swap! conductor-state assoc
                     :last-cycle {:action :bell
                                  :target agent-id
                                  :event event
                                  :dispatch-action (:action dispatch-result)
                                  :text result-text}
                     :last-cycle-ms now-ms)
              {:ok true
               :session-id session-id
               :result result-text})))
        {:ok true
         :session-id session-id
         :result (str "BELL " event
                      " rejected: FM conductor is not running for " nick ".")}))))
