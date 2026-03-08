(ns futon3c.peripheral.mentor-map
  "Space-like conversation map for the Mentor peripheral.

   Whereas Tickle is time-like (what needs to happen next, pushing through DAGs),
   the Mentor is space-like (what territory has been covered, where are the gaps).

   The conversation map tracks:
   - topics:     mathematical territories explored, with status and who worked them
   - patterns:   which proof/question patterns have been applied vs. relevant-but-unapplied
   - tensions:   contradictions or unresolved conflicts between approaches
   - confidence: claims made with stated certainty, and whether they were tested
   - effort:     who has worked on what — detects clustering and neglected areas

   The map is the primary input to QP trigger evaluation. Triggers detect gaps
   in the map's coverage, not just patterns in raw conversation text.

   Persistence: the map checkpoints to futon1a as :observation evidence entries.
   On mentor restart, the latest checkpoint is restored so the map survives."
  (:require [clojure.string :as str])
  (:import [java.time Instant]))

;; =============================================================================
;; Empty map
;; =============================================================================

(defn empty-map
  "Create an empty conversation map for a problem."
  [handle problem-id channel]
  {:mentor/handle   handle
   :mentor/problem  problem-id
   :mentor/channel  channel
   :mentor/version  0
   ;; Space-like dimensions
   :map/topics      []    ;; [{:id kw :label str :status :explored|:mentioned|:abandoned
                          ;;   :by str :at str :notes str}]
   :map/patterns    {:applied  #{}    ;; pattern IDs that have been used
                     :relevant #{}    ;; pattern IDs that seem relevant but unapplied
                     :rejected #{}}   ;; patterns explicitly considered and rejected
   :map/tensions    []    ;; [{:id kw :between [topic-a topic-b] :description str
                          ;;   :resolved? bool :at str}]
   :map/confidence  []    ;; [{:agent str :claim str :certainty :high|:medium|:low
                          ;;   :tested? bool :at str}]
   :map/effort      {}    ;; {agent-id #{obligation-id ...}}
   ;; Conversation digest (replaces ring buffer)
   :map/digest      []    ;; [{:at str :speaker str :summary str :tags #{kw}}]
   :map/messages-seen 0
   :map/last-seen-at nil
   ;; Trigger state
   :triggers/fired     #{}
   :triggers/last-check nil
   ;; Intervention log
   :interventions   []})  ;; [{:at str :trigger-id kw :message str}]

;; =============================================================================
;; Map enrichment — called on each observe cycle
;; =============================================================================

(defn- extract-topic-mentions
  "Detect mathematical topic references in a message.
   Returns set of topic keywords found."
  [text]
  (let [text-lower (str/lower-case (or text ""))]
    (cond-> #{}
      (re-find #"falsif|counterexample|disprove" text-lower)
      (conj :falsification)

      (re-find #"construct|build|witness|explicit" text-lower)
      (conj :construction)

      (re-find #"ramsey|book.graph|B_\{?[nk]" text-lower)
      (conj :ramsey-book-graphs)

      (re-find #"small.case|enumerate|exhaustive|brute" text-lower)
      (conj :small-case-enumeration)

      (re-find #"paley|cayley|algebraic" text-lower)
      (conj :algebraic-construction)

      (re-find #"random|probabilistic" text-lower)
      (conj :probabilistic-method)

      (re-find #"literature|paper|known.result|published" text-lower)
      (conj :literature-search)

      (re-find #"obstruction|impossible|lower.bound" text-lower)
      (conj :structural-obstruction)

      (re-find #"complement|B_n.*free|freeness" text-lower)
      (conj :complement-freeness)

      (re-find #"arse|stack.exchange|corpus|question" text-lower)
      (conj :arse-query))))

(defn- extract-confidence
  "Detect confidence claims in a message. Returns nil or a confidence entry."
  [speaker text]
  (let [text-lower (str/lower-case (or text ""))]
    (cond
      (re-find #"(?:i'm )?(?:sure|certain|definitely|clearly|trivially)|(?:obviously|clearly)\s+(?:true|false|holds|works|correct)" text-lower)
      {:agent speaker :claim (subs text 0 (min 120 (count text)))
       :certainty :high :tested? false :at (str (Instant/now))}

      (re-find #"(?:i think|probably|likely|seems like|might be)" text-lower)
      {:agent speaker :claim (subs text 0 (min 120 (count text)))
       :certainty :medium :tested? false :at (str (Instant/now))}

      :else nil)))

(defn- extract-pattern-signals
  "Detect pattern application or pattern-relevant signals in a message."
  [text]
  (let [text-lower (str/lower-case (or text ""))]
    (cond-> {:applied #{} :relevant #{}}
      (re-find #"landscape|survey|what.methods|alternatives" text-lower)
      (update :applied conj :technique-landscape)

      (re-find #"scout|explore.*territory|computational.*exploration" text-lower)
      (update :applied conj :landscape-scout)

      (re-find #"probe|obstruction|check.*before" text-lower)
      (update :applied conj :structural-probe)

      (re-find #"fail.*theorem|characterize.*failure|dead.end.*into" text-lower)
      (update :applied conj :failure-conversion)

      (re-find #"verify.*hypothesis|check.*applies|applicability" text-lower)
      (update :applied conj :theorem-applicability)

      (re-find #"tension|conflict|contradict" text-lower)
      (update :applied conj :tension-recognition)

      (re-find #"kernel|isolat.*gap|remaining.*step" text-lower)
      (update :applied conj :kernel-isolation)

      (re-find #"try.*harder|stuck|same.*approach.*again" text-lower)
      (update :relevant conj :try-harder-break))))

(defn- coalesce-messages
  "Merge consecutive messages from the same speaker within 3 seconds.
   IRC bridges split long messages into multiple PRIVMSG lines — this
   reassembles them so the mentor sees coherent utterances."
  [msgs]
  (when (seq msgs)
    (reduce
      (fn [acc {:keys [nick text at] :as msg}]
        (let [prev (peek acc)]
          (if (and prev
                   (= nick (:nick prev))
                   at (:at prev)
                   ;; Within 3 seconds = same logical message
                   (let [t1 (str (:at prev))
                         t2 (str at)]
                     (and (>= (count t1) 19) (>= (count t2) 19)
                          (= (subs t1 0 17) (subs t2 0 17)))))
            ;; Merge: append text to previous message
            (conj (pop acc) (update prev :text #(str % " " text)))
            ;; New speaker or time gap: new entry
            (conj acc msg))))
      []
      msgs)))

(defn enrich
  "Enrich the conversation map with new messages.
   new-msgs: [{:nick str :text str :at str}]
   Coalesces split IRC lines from the same speaker, then extracts
   topics, patterns, confidence, and effort from each utterance.
   Returns updated map."
  [cmap new-msgs]
  (if (empty? new-msgs)
    cmap
    (let [now (str (Instant/now))
          msgs (coalesce-messages new-msgs)]
      (reduce
        (fn [m {:keys [nick text at]}]
          (let [topics (extract-topic-mentions text)
                confidence (extract-confidence nick text)
                patterns (extract-pattern-signals text)]
            (cond-> m
              ;; Bump version
              true
              (update :mentor/version inc)

              ;; Update topics
              (seq topics)
              (update :map/topics
                      into (for [t topics
                                 :when (not (some #(= t (:id %)) (:map/topics m)))]
                             {:id t :label (name t) :status :mentioned
                              :by nick :at (or at now) :notes nil}))

              ;; Update patterns
              (seq (:applied patterns))
              (update-in [:map/patterns :applied] into (:applied patterns))

              (seq (:relevant patterns))
              (update-in [:map/patterns :relevant] into (:relevant patterns))

              ;; Track confidence claims
              confidence
              (update :map/confidence conj confidence)

              ;; Track effort by agent
              (and nick (not= nick "joe"))
              (update-in [:map/effort nick] (fnil into #{}) topics)

              ;; Add to digest (keep full text, cap at 500 chars for storage)
              true
              (update :map/digest conj
                      {:at (or at now) :speaker nick
                       :summary (subs (or text "") 0 (min 500 (count (or text ""))))
                       :tags topics})

              ;; Update seen counter
              true
              (-> (update :map/messages-seen inc)
                  (assoc :map/last-seen-at (or at now))))))
        cmap
        msgs))))

;; =============================================================================
;; Gap detection — what the triggers check against
;; =============================================================================

(defn topic-gaps
  "Return topics that are relevant to the problem but have not been mentioned."
  [cmap]
  (let [mentioned (set (map :id (:map/topics cmap)))
        ;; Core topics for any FM problem in FALSIFY mode
        expected #{:falsification :small-case-enumeration :literature-search
                   :structural-obstruction}]
    (remove mentioned expected)))

(defn pattern-gaps
  "Patterns that are relevant but not yet applied."
  [cmap]
  (let [applied (get-in cmap [:map/patterns :applied])
        relevant (get-in cmap [:map/patterns :relevant])]
    (into relevant (remove applied #{:landscape-scout :technique-landscape
                                     :structural-probe}))))

(defn effort-concentration
  "Detect if all effort is concentrated on one approach.
   Returns nil if balanced, or {:concentrated-on topic :agents [agents]} if clustered."
  [cmap]
  (let [effort (:map/effort cmap)
        all-topics (mapcat val effort)]
    (when (seq all-topics)
      (let [freq (frequencies all-topics)
            [top-topic top-count] (apply max-key val freq)
            total (count all-topics)]
        (when (and (> total 3) (> (/ top-count total) 0.7))
          {:concentrated-on top-topic
           :agents (vec (for [[agent topics] effort
                              :when (contains? topics top-topic)]
                          agent))})))))

(defn untested-high-confidence
  "Return high-confidence claims that haven't been tested."
  [cmap]
  (filter (fn [{:keys [certainty tested?]}]
            (and (= certainty :high) (not tested?)))
          (:map/confidence cmap)))

(defn unresolved-tensions
  "Return tensions that haven't been resolved."
  [cmap]
  (filter (complement :resolved?) (:map/tensions cmap)))

;; =============================================================================
;; Trigger evaluation against the map
;; =============================================================================

(defn evaluate-triggers
  "Check all 8 QP triggers against the conversation map.
   Returns [{:trigger-id kw :fires? bool :reason str}]."
  [cmap]
  (let [t-gaps (topic-gaps cmap)
        p-gaps (pattern-gaps cmap)
        concentration (effort-concentration cmap)
        untested (untested-high-confidence cmap)
        tensions (unresolved-tensions cmap)
        msgs-seen (:map/messages-seen cmap 0)
        already-fired (:triggers/fired cmap)]
    ;; Only evaluate if we have some conversation to work with
    (when (> msgs-seen 5)
      [{:trigger-id :QP-1
        :fires? (and (contains? p-gaps :landscape-scout)
                     (not (contains? already-fired :QP-1)))
        :reason (when (contains? p-gaps :landscape-scout)
                  "No computational exploration before committing to approach")}
       {:trigger-id :QP-2
        :fires? (and (contains? p-gaps :technique-landscape)
                     (not (contains? already-fired :QP-2)))
        :reason (when (contains? p-gaps :technique-landscape)
                  "Single method committed without surveying alternatives")}
       {:trigger-id :QP-3
        :fires? (and (contains? p-gaps :structural-probe)
                     (not (contains? already-fired :QP-3)))
        :reason (when (contains? p-gaps :structural-probe)
                  "Approach pursued without checking for obstructions")}
       {:trigger-id :QP-4
        :fires? (and (some #(= :abandoned (:status %)) (:map/topics cmap))
                     (not (contains? already-fired :QP-4)))
        :reason "Approach abandoned without characterizing the failure"}
       {:trigger-id :QP-5
        :fires? false ;; needs deeper semantic analysis
        :reason nil}
       {:trigger-id :QP-6
        :fires? (and (seq tensions)
                     (not (contains? already-fired :QP-6)))
        :reason (when (seq tensions)
                  (str "Unresolved tension: " (:description (first tensions))))}
       {:trigger-id :QP-7
        :fires? false ;; needs late-stage proof context
        :reason nil}
       {:trigger-id :QP-8
        :fires? (and (seq untested)
                     (not (contains? already-fired :QP-8)))
        :reason (when (seq untested)
                  (str "High confidence untested: " (:claim (first untested))))}])))

;; =============================================================================
;; Serialization for persistence
;; =============================================================================

(defn checkpoint-body
  "Extract the persistable subset of the map (no functions, no transient state)."
  [cmap]
  (select-keys cmap
    [:mentor/handle :mentor/problem :mentor/channel :mentor/version
     :map/topics :map/patterns :map/tensions :map/confidence :map/effort
     :map/digest :map/messages-seen :map/last-seen-at
     :triggers/fired :triggers/last-check
     :interventions]))

(defn restore-from-checkpoint
  "Rebuild a conversation map from a persisted checkpoint body.
   Merges checkpoint data into a fresh empty-map (to pick up any new fields)."
  [checkpoint]
  (merge (empty-map (:mentor/handle checkpoint)
                     (:mentor/problem checkpoint)
                     (:mentor/channel checkpoint))
         checkpoint))
