(ns futon3c.agency.turn-notice
  "Bounded, exact-seat notices consumed only by current-turn header assembly."
  (:require [clojure.string :as str]))

(def queue-limit 20)
(def seen-limit 500)

(defonce ^:private !state (atom {}))

(defn reset-state!
  "Testing seam. Remove all queued notices and deduplication history."
  []
  (reset! !state {}))

(defn- seat-key [agent session]
  (when (and (not (str/blank? (str agent)))
             (not (str/blank? (str session))))
    [(str agent) (str session)]))

(def ^:private refusal-reasons
  #{:no-visible-offer :unknown-offer :unknown-option})

(defn- nonblank? [x]
  (and (string? x) (not (str/blank? x))))

(defn- act-id? [x]
  (and (nonblank? x) (str/starts-with? x "act:")))

(defn- invalid! [reason field]
  (throw (ex-info "Invalid turn notice" {:reason reason :field field})))

(defn- validate-notice!
  [{:keys [agent session notice-id kind effect-id agreement-id offer-id
           option-id candidates reason grant-id grant-until grant-reason]
    :as notice}]
  (let [base #{:agent :session :notice-id :kind}
        allowed (case kind
                  "effect" (conj base :effect-id)
                  "agreement-accepted" (into base [:agreement-id :offer-id :option-id
                                                    :grant-id :grant-until :grant-reason])
                  "agreement-ambiguous" (conj base :candidates)
                  "agreement-refused" (conj base :reason)
                  base)]
    (when-not (seat-key agent session) (invalid! :invalid-seat :agent))
    (when-not (nonblank? notice-id) (invalid! :invalid-notice-id :notice-id))
    (when-not (contains? #{"unresolved" "effect" "no-grant"
                           "agreement-accepted" "agreement-ambiguous"
                           "agreement-refused"} kind)
      (invalid! :invalid-kind :kind))
    (when (seq (remove allowed (keys notice)))
      (invalid! :unexpected-key (first (remove allowed (keys notice)))))
    (case kind
      "effect" (when-not (act-id? effect-id) (invalid! :invalid-effect-id :effect-id))
      "agreement-accepted"
      (cond (not (act-id? agreement-id)) (invalid! :invalid-agreement-id :agreement-id)
            (not (act-id? offer-id)) (invalid! :invalid-offer-id :offer-id)
            (not (nonblank? option-id)) (invalid! :invalid-option-id :option-id)
            (and grant-id (not (act-id? grant-id)))
            (invalid! :invalid-grant-id :grant-id)
            (not= (boolean grant-id) (boolean grant-until))
            (invalid! :incomplete-grant :grant-id)
            (and grant-id (not (nonblank? grant-until)))
            (invalid! :invalid-grant-until :grant-until)
            (and grant-reason
                 (not (contains? #{"agreement-only" "grant-write-failed"}
                                 grant-reason)))
            (invalid! :invalid-grant-reason :grant-reason)
            (and grant-id grant-reason) (invalid! :conflicting-grant :grant-id))
      "agreement-ambiguous"
      (when-not (and (sequential? candidates) (seq candidates)
                     (every? (fn [candidate]
                               (and (map? candidate)
                                    (= #{:offer-id :option-id} (set (keys candidate)))
                                    (act-id? (:offer-id candidate))
                                    (nonblank? (:option-id candidate))))
                             candidates))
        (invalid! :invalid-candidates :candidates))
      "agreement-refused"
      (when-not (contains? refusal-reasons reason)
        (invalid! :invalid-refusal-reason :reason))
      nil)
    notice))

(defn- notice-text [{:keys [kind effect-id agreement-id offer-id option-id
                            candidates reason grant-id grant-until grant-reason]}]
  (case kind
    "unresolved" "withdraw inferred: unresolved (no target)"
    "effect" (str "withdraw inferred: effect " effect-id " (undo to reverse)")
    "no-grant" "withdraw inferred: off (no grant)"
    "agreement-accepted"
    (str "agreement " agreement-id ": you offered " offer-id
         ", Joe accepted option " option-id
         (cond
           grant-id (str "; grant " grant-id " until " grant-until)
           (= "agreement-only" grant-reason) "; agreement only, no grant"
           (= "grant-write-failed" grant-reason) "; grant write failed"
           :else ""))
    "agreement-ambiguous"
    (str "agreement ambiguous: ask Joe one short question naming which ("
         (str/join ", " (map (fn [{:keys [offer-id option-id]}]
                               (str offer-id " " option-id))
                             (take 6 candidates)))
         ")")
    "agreement-refused" (str "agreement refused: " (name reason))))

(defn publish!
  "Queue NOTICE once for its exact seat. Return :queued or :duplicate.
   NOTICE has :agent, :session, :notice-id and a closed kind-specific payload.
   This function validates that payload and renders all text server-side."
  [{:keys [agent session notice-id kind effect-id] :as input}]
  (let [input (validate-notice! input)
        seat (seat-key agent session)
        id (str notice-id)
        notice {:notice/id id
                :notice/kind kind
                :notice/text (notice-text input)
                :notice/effect-id effect-id}]
    (loop []
      (let [before @!state
            ;; `or`, not get's default: a nil stored under the seat would
            ;; otherwise be used, and (+ nil …) fails.
            entry (or (get before seat) {:queue [] :seen-order [] :seen #{} :drops 0})]
        (if (contains? (:seen entry) id)
          :duplicate
          (let [queue (conj (:queue entry) notice)
                overflow (max 0 (- (count queue) queue-limit))
                queue (vec (drop overflow queue))
                seen-order (conj (:seen-order entry) id)
                seen-overflow (max 0 (- (count seen-order) seen-limit))
                forgotten (take seen-overflow seen-order)
                seen-order (vec (drop seen-overflow seen-order))
                seen (-> (:seen entry) (conj id) (#(apply disj % forgotten)))
                after (assoc before seat {:queue queue
                                          :seen-order seen-order
                                          :seen seen
                                          :drops (+ (:drops entry) overflow)})]
            (if (compare-and-set! !state before after) :queued (recur))))))))

(defn take!
  "Atomically remove and return one queued notice for AGENT and SESSION."
  [agent session]
  (when-let [seat (seat-key agent session)]
    (let [[before _]
          ;; Every exact-seat header calls this; a seat with nothing queued
          ;; must be left absent, not given a nil entry.
          (swap-vals! !state
                      (fn [state]
                        (let [entry (get state seat)]
                          (if (seq (:queue entry))
                            (assoc state seat (assoc entry :queue (vec (rest (:queue entry)))))
                            state))))]
      (first (get-in before [seat :queue])))))

(defn stats
  "Inspectable per-seat queue depth, seen count, and dropped count."
  [agent session]
  (when-let [entry (get @!state (seat-key agent session))]
    {:queued (count (:queue entry))
     :seen (count (:seen entry))
     :drops (:drops entry)}))
