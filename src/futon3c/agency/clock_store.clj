(ns futon3c.agency.clock-store
  "Per-agent-session clock state for agent-native auto-clock signals.

   INSTANTIATE-4 mirrors the Emacs edit-activity rule on the JVM side:
   repeated Edit/Write/MultiEdit tool uses against existing C-/M-/E-.md docs
   switch the agent session clock. The file path is the witness; no prose or
   fuzzy matching participates in the decision."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def default-window-ms
  (* 600 1000))

(def default-threshold
  3)

(def ^:private editable-tool-names
  #{"Edit" "Write" "MultiEdit"})

(defonce ^:private !sessions
  (atom {}))

(defn editable-tool?
  [tool-name]
  (contains? editable-tool-names (str tool-name)))

(defn- session-key
  [agent-id session-id]
  [(str agent-id) (some-> session-id str str/trim not-empty)])

(defn- fallback-key
  [agent-id]
  [(str agent-id) nil])

(defn empty-clock
  []
  {:campaign-id nil
   :mission-id nil
   :excursion-id nil})

(defn empty-session-state
  []
  {:clock (empty-clock)
   :edit-events {}
   :last-reclock-target nil
   :last-auto-clock-witness nil})

(defn reset-store!
  []
  (reset! !sessions {}))

(defn- parent-segment?
  [^java.io.File file segment]
  (loop [f (.getParentFile file)]
    (cond
      (nil? f) false
      (= segment (.getName f)) true
      :else (recur (.getParentFile f)))))

(defn resolve-clock-target-file
  "Resolve FILE-PATH to an existing C-/M-/E-.md doc target by exact basename.
   Returns a single-active clock target map, or nil when the path is not a
   witnessed mission/campaign/excursion doc."
  [file-path]
  (when-let [raw (some-> file-path str str/trim not-empty)]
    (let [file (io/file raw)]
      (when (and (.exists file)
                 (.isFile file)
                 (parent-segment? file "holes"))
        (when-let [[_ id] (re-matches #"^([CME]-[^/]+)\.md$" (.getName file))]
          (let [canonical (.getCanonicalPath file)]
            (case (first id)
              \C {:id id
                  :kind :campaign
                  :file canonical
                  :clock {:campaign-id id :mission-id nil :excursion-id nil}}
              \M {:id id
                  :kind :mission
                  :file canonical
                  :clock {:campaign-id nil :mission-id id :excursion-id nil}}
              \E {:id id
                  :kind :excursion
                  :file canonical
                  :clock {:campaign-id nil :mission-id nil :excursion-id id}}
              nil)))))))

(defn- clock-label
  [clock]
  (or (:excursion-id clock)
      (:mission-id clock)
      (:campaign-id clock)
      "no mission"))

(defn- dispatch-clock
  [target-id]
  (let [target-id (if (re-matches #"^[MEC]-.+" target-id)
                    target-id
                    (str "M-" target-id))]
    (case (first target-id)
      \C {:campaign-id target-id :mission-id nil :excursion-id nil}
      \E {:campaign-id nil :mission-id nil :excursion-id target-id}
      {:campaign-id nil :mission-id target-id :excursion-id nil})))

(defn- prune-events
  [events now-ms window-ms]
  (into {}
        (keep (fn [[target-id timestamps]]
                (let [recent (vec (filter #(<= (- now-ms %) window-ms) timestamps))]
                  (when (seq recent)
                    [target-id recent]))))
        events))

(defn- dominant-target?
  [counts target-id threshold]
  (let [target-count (long (get counts target-id 0))
        next-count (long (or (->> counts
                                  (remove (fn [[id _]] (= target-id id)))
                                  (map val)
                                  sort
                                  last)
                             0))
        margin (max 1 (dec (long threshold)))]
    (and (>= target-count threshold)
         (>= target-count (+ next-count margin)))))

(defn apply-edit-activity
  "Pure reclock transition. STATE plus a witnessed TARGET edit at NOW-MS returns
   updated state. Reclock requires threshold + dominance, so alternating edits do
   not thrash."
  ([state target now-ms]
   (apply-edit-activity state target now-ms {}))
  ([state target now-ms {:keys [threshold window-ms]
                         :or {threshold default-threshold
                              window-ms default-window-ms}}]
   (let [state (merge (empty-session-state) state)
         target-id (:id target)
         pruned (prune-events (:edit-events state) now-ms window-ms)
         events (update pruned target-id (fnil conj []) now-ms)
         counts (update-vals events count)
         old-clock (:clock state)
         new-clock (:clock target)
         should-switch? (and target-id
                             (not= old-clock new-clock)
                             (not= target-id (:last-reclock-target state))
                             (dominant-target? counts target-id threshold))
         edit-count (long (get counts target-id 0))]
     (if should-switch?
       (let [witness {:rule "agent-edit-activity"
                      :source "agent-tool-edit"
                      :file (:file target)
                      :edit-count edit-count
                      :window-seconds (long (/ window-ms 1000))
                      :old-target (clock-label old-clock)
                      :new-target (clock-label new-clock)}]
         (assoc state
                :clock new-clock
                :edit-events events
                :last-reclock-target target-id
                :last-auto-clock-witness witness))
       (assoc state :edit-events events)))))

(defn record-edit!
  "Record a tool edit against FILE-PATH for AGENT-ID/SESSION-ID. Returns the
   resolved target and current session state when the file is a witnessed doc."
  ([agent-id session-id file-path]
   (record-edit! agent-id session-id file-path {}))
  ([agent-id session-id file-path opts]
   (when-let [target (resolve-clock-target-file file-path)]
     (let [k (session-key agent-id session-id)
           now-ms (long (or (:now-ms opts) (System/currentTimeMillis)))
           transition-opts (select-keys opts [:threshold :window-ms])
           state (swap! !sessions update k
                        #(apply-edit-activity (or % (empty-session-state))
                                              target now-ms transition-opts))]
       {:target target
        :state (get state k)}))))

(defn record-tool-use!
  "Record a single tool-use detail when it is an editing tool with a file_path."
  ([agent-id session-id tool-detail]
   (record-tool-use! agent-id session-id tool-detail {}))
  ([agent-id session-id tool-detail opts]
   (let [tool-name (or (:name tool-detail) (get tool-detail "name"))
         input (or (:input tool-detail) (get tool-detail "input"))
         file-path (or (:file_path input) (get input "file_path"))]
     (when (and (editable-tool? tool-name) file-path)
       (record-edit! agent-id session-id file-path opts)))))

(defn set-dispatch-mission!
  "Secondary signal: an explicit dispatch mission-id clocks this agent session to
   that mission immediately."
  [agent-id session-id mission-id]
  (when-let [target-id (some-> mission-id str str/trim not-empty)]
    (let [k (session-key agent-id session-id)
          new-clock (dispatch-clock target-id)]
      (get
       (swap! !sessions update k
              (fn [state]
                (let [state (merge (empty-session-state) state)
                      old-clock (:clock state)]
                  (assoc state
                         :clock new-clock
                         ;; An explicit dispatch is a hard re-clock: clear the
                         ;; edit-activity anti-thrash baseline so subsequent edits
                         ;; can reclock back to the dispatched-away target (else a
                         ;; dispatch permanently pins the session — the stuck-clock
                         ;; bug behind C-cascade-real D1's repl display).
                         :last-reclock-target nil
                         :last-auto-clock-witness
                         {:rule "dispatch-mission-id"
                          :source "invoke-receipt"
                          :old-target (clock-label old-clock)
                          :new-target (clock-label new-clock)}))))
       k))))

(defn current-state
  [agent-id session-id]
  (let [sessions @!sessions
        k (session-key agent-id session-id)]
    (or (get sessions k)
        (get sessions (fallback-key agent-id))
        (empty-session-state))))

(defn current-clock
  [agent-id session-id]
  (:clock (current-state agent-id session-id)))

(defn evidence-clock-fields
  "String-keyed fields suitable for invoke evidence bodies."
  [agent-id session-id]
  (let [{:keys [clock last-auto-clock-witness]} (current-state agent-id session-id)]
    (cond-> {}
      (:campaign-id clock) (assoc "clocked-campaign" (:campaign-id clock))
      (:mission-id clock) (assoc "mission-id" (:mission-id clock)
                                 "clocked-mission" (:mission-id clock))
      (:excursion-id clock) (assoc "clocked-excursion" (:excursion-id clock))
      last-auto-clock-witness (assoc "auto-clock-witness" last-auto-clock-witness))))
