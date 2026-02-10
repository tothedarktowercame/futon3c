(ns futon3c.social.mode
  "S-mode: message mode classification + DISCUSS/DIAGNOSE/EXECUTE transition validation.

   R10 (mode-gate): coordination talk and action talk must be distinguishable.
   R4 (loud failure): always return ClassifiedMessage|ModeTransition|SocialError."
  (:require [clojure.string :as str]
            [futon3c.social.shapes :as shapes])
  (:import [java.time Instant]))

(defn- now-str []
  (str (Instant/now)))

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :S-mode
           :error/code code
           :error/message message
           :error/at (now-str)}
    (seq context) (assoc :error/context context)))

(def ^:private coordination-types
  #{"standup" "handoff" "coordination" "bell" "whistle" "page"})

(defn- coordination-payload?
  [payload]
  (cond
    (and (map? payload) (true? (:coordination payload)))
    true

    (and (map? payload) (contains? payload :type))
    (let [t (:type payload)
          s (cond
              (string? t) t
              (keyword? t) (name t)
              :else nil)]
      (contains? coordination-types s))

    :else
    false))

(defn classify
  "Classify a message into coordination or action mode.
   Uses message payload/metadata to determine mode.
   Returns ClassifiedMessage on success, SocialError on failure."
  [message patterns]
  (cond
    (not (map? message))
    (social-error :invalid-message
                  "Message must be a map"
                  :message message)

    (nil? patterns)
    (social-error :patterns-missing
                  "Pattern library input is required"
                  :patterns nil)

    (not (shapes/valid? shapes/PatternLibrary patterns))
    (social-error :invalid-patterns
                  "Invalid pattern library input"
                  :patterns patterns
                  :validation (or (:error (shapes/validate shapes/PatternLibrary patterns)) {}))

    :else
    (let [payload (:msg/payload message)
          mode (if (coordination-payload? payload) :coordination :action)
          classified (assoc message :msg/mode mode)]
      (if (shapes/valid? shapes/ClassifiedMessage classified)
        classified
        (social-error :invalid-message
                      "Message could not be classified into a valid ClassifiedMessage"
                      :message message
                      :classified classified
                      :validation (or (:error (shapes/validate shapes/ClassifiedMessage classified)) {}))))))

(defn validate-transition
  "Validate a mode transition request.
   Enforces: DISCUSS → DIAGNOSE → EXECUTE state machine.
   Returns ModeTransition on success, SocialError on failure."
  [current-mode requested-mode actor & {:keys [approval-token exit-criteria summary]}]
  (cond
    (not (shapes/valid? shapes/OperationalMode current-mode))
    (social-error :invalid-input
                  "Invalid current mode"
                  :current-mode current-mode)

    (not (shapes/valid? shapes/OperationalMode requested-mode))
    (social-error :invalid-input
                  "Invalid requested mode"
                  :requested-mode requested-mode)

    (not (string? actor))
    (social-error :invalid-input
                  "Actor must be a string"
                  :actor actor)

    (= current-mode requested-mode)
    (social-error :invalid-transition
                  "No-op transitions are not allowed"
                  :from current-mode
                  :to requested-mode)

    :else
    (let [allowed?
          (cond
            ;; Any -> DISCUSS (reset/timeout) is always allowed.
            (= requested-mode :discuss) true

            ;; DISCUSS -> DIAGNOSE allowed.
            (and (= current-mode :discuss) (= requested-mode :diagnose)) true

            ;; DIAGNOSE -> EXECUTE requires approval token.
            (and (= current-mode :diagnose) (= requested-mode :execute))
            (and (string? approval-token) (not (str/blank? approval-token)))

            ;; EXECUTE -> DISCUSS always allowed (exit).
            (and (= current-mode :execute) (= requested-mode :discuss)) true

            :else false)]
      (cond
        (and (= current-mode :diagnose) (= requested-mode :execute)
             (or (nil? approval-token)
                 (and (string? approval-token) (str/blank? approval-token))
                 (not (string? approval-token))))
        (social-error :approval-required
                      "DIAGNOSE → EXECUTE requires :approval-token"
                      :from current-mode
                      :to requested-mode)

        (not allowed?)
        (social-error :invalid-transition
                      "Invalid mode transition"
                      :from current-mode
                      :to requested-mode)

        :else
        (let [transition (cond-> {:mode/from current-mode
                                  :mode/to requested-mode
                                  :mode/actor actor
                                  :mode/at (now-str)}
                           exit-criteria (assoc :mode/exit-criteria exit-criteria)
                           (and (= current-mode :diagnose) (= requested-mode :execute))
                           (assoc :mode/approval-token approval-token)
                           summary (assoc :mode/summary summary))]
          (if (shapes/valid? shapes/ModeTransition transition)
            transition
            (social-error :invalid-transition
                          "Internal error: ModeTransition did not conform to shape"
                          :transition transition
                          :validation (or (:error (shapes/validate shapes/ModeTransition transition)) {}))))))))

