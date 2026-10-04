(ns futon3c.xiang.turn-acts
  "Settled 象 turns plus marked agent replies, as P24 acts.

   E-agency-work-orders' heads-up display needs the kernel
   (futon3c.logic.xiang) to answer, for one turn: which ports closed in it
   and which are still open. The kernel speaks in acts; this namespace is
   the pure adapter from the settled record, its reading, the agent's
   marked reply and the happened note's commits to the act maps
   `futon3c.logic.xiang/facts` accepts.

   Sources of acts, per turn:
   - the operator's turn: one act per reading fragment whose intent has a
     kernel kind (an intent with no kind is skipped and counted, never
     invented);
   - the agent's reply: one act per paragraph carrying a proforma mark
     (futon3c.xiang.turn-record/reply-marks), the mark declaring the kind.
     🈸 is :offer when the paragraph enumerates numbered options, else
     :ask-action;
   - the happened note's commits: one :commit act each.

   Links are stated only from explicit evidence: an approve fragment in
   the operator's turn becomes an :accept targeting the offer in the
   previous turn's reply (session-acts threads that context; turn->acts
   alone cannot see it, and leaves approve as :approve); a commit in a
   turn that accepted an offer, or in the turn right after the
   acceptance, carries that offer out. Everything inferred beyond that is
   left to the kernel's flow/posterior.

   Act ids are stable: <turn_id>-f-<sentence>-<n> for fragments,
   <turn_id>-r-<n> for reply paragraphs, <turn_id>-c-<sha> for commits."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.logic.xiang :as lx]
            [futon3c.xiang.turn-record :as turn-record]))

;; ---------------------------------------------------------------------------
;; Intent -> kind

(def intent-kinds
  "The reading/mark intents that name a kernel act kind, as keywords.
   Intents absent here (gist, unresolved, anything a reading invents) have
   no kind: the acts are skipped and counted."
  (into {} (keep (fn [[k _]] [(name k) k]) lx/kinds)))

(defn- intent-kind
  "The kernel kind for a reading intent string, or nil."
  [intent]
  (get intent-kinds (some-> intent str/trim)))

;; ---------------------------------------------------------------------------
;; Offers and options

(defn- option-numbers
  "The option numbers a 🈸 paragraph states: a numbered list, or \"yes N\"
   spellings, in first-mentioned order."
  [text]
  (distinct (map #(Long/parseLong %)
                 (concat (map second (re-seq #"(?m)^\s*(\d+)\." text))
                         (map second (re-seq #"(?i)\byes\s+(\d+)\b" text))))))

(defn- mark-kind
  "The kernel kind a proforma mark's paragraph declares, or nil when the
   mark is annotator-only (gist, unresolved). 🈸 is :offer when the
   paragraph states options, else :ask-action."
  [intent text]
  (if (= intent "ask-action")
    (if (seq (option-numbers text)) :offer :ask-action)
    (intent-kind intent)))

;; ---------------------------------------------------------------------------
;; Happened-note commits

(defn happened-commits
  "The commits of a happened note's \"Commits during the turn\" section:
   [{:repo :sha :subject}], [] when the section says (none) or is absent."
  [happened-summary]
  (let [s (str (or happened-summary ""))]
    (if-let [section (second (str/split s #"Commits during the turn[^\n]*:\n" 2))]
      (vec (keep (fn [line]
                   (when-let [[_ repo sha subject] (re-matches #"- (\S+) ([0-9a-f]{7,40}) (.+)" (str/trim line))]
                     {:repo repo :sha sha :subject subject}))
                 (str/split-lines section)))
      [])))

;; ---------------------------------------------------------------------------
;; Turn -> acts

(defn- fragment-acts
  "One act per reading fragment with a kernel kind. Returns
   {:acts .. :skipped n} — fragments whose intent has no kind are skipped
   and counted, never invented."
  [record reading]
  (let [base (:turn_id record)
        at (:created_at record)
        seat {:agent (:agent_id record) :session (:session_id record)}]
    (reduce (fn [acc [sid idx {:keys [intent] :as frag}]]
              (if-let [kind (intent-kind intent)]
                (update acc :acts conj
                        (cond-> {:id (str base "-f-" sid "-" idx)
                                 :kind kind
                                 :author "operator"
                                 :at at
                                 :to (:agent_id record)
                                 :agent (:agent seat) :session (:session seat)
                                 :turn base
                                 :text (:text frag)}
                          (:target frag) (assoc :about (:target frag))))
                (update acc :skipped inc)))
            {:acts [] :skipped 0}
            (mapcat (fn [sentence]
                      (map-indexed (fn [i f] [(:id sentence) i f])
                                   (:fragments sentence)))
                    (:sentences reading)))))

(defn- reply-acts
  "One act per marked paragraph of the agent's reply, the mark declaring
   the kind. Marked paragraphs whose intent has no kernel kind (gist,
   unresolved) are skipped and counted."
  [record reply-text]
  (let [base (:turn_id record)
        at (:created_at record)]
    (reduce (fn [acc [idx {:keys [intent text]}]]
              (if-let [kind (mark-kind intent text)]
                (update acc :acts conj
                        (cond-> {:id (str base "-r-" idx)
                                 :kind kind
                                 :author (:agent_id record)
                                 :at at
                                 :to "operator"
                                 :agent (:agent_id record) :session (:session_id record)
                                 :turn base
                                 :text text}
                          (= kind :offer) (assoc :option (vec (option-numbers text)))))
                (update acc :skipped inc)))
            {:acts [] :skipped 0}
            (map-indexed vector (turn-record/reply-marks reply-text)))))

(defn- commit-acts
  "One :commit act per happened-note commit."
  [record commits]
  (let [base (:turn_id record)]
    (mapv (fn [{:keys [sha repo subject]}]
            {:id (str base "-c-" sha)
             :kind :commit
             :author (:agent_id record)
             :at (:created_at record)
             :agent (:agent_id record) :session (:session_id record)
             :turn base
             :sha sha :repo repo :subject subject})
          commits)))

(defn- accept-fragment?
  "An operator fragment that accepts: kind :approve, or :accept already."
  [act]
  (contains? #{:approve :accept} (:kind act)))

(defn- approve->accept
  "Rewrite an :approve act as an :accept of OFFER-ID, keeping any option
   the fragment's text names (\"1\", \"yes 1\")."
  [act offer-id]
  (let [n (some-> (or (re-find #"(?i)\byes\s+(\d+)\b" (:text act ""))
                      (re-find #"^\W*(\d+)\W*$" (:text act "")))
                  second Long/parseLong)]
    (cond-> (assoc act :kind :accept :target offer-id)
      n (assoc :option n))))

(defn turn->acts
  "The acts of one settled turn: the operator's reading fragments, the
   agent's marked reply paragraphs and the happened commits, in that
   order. RECORD and READING are the decoded record and analysis maps
   (keyword keys); REPLY-TEXT is the agent's reply as text; COMMITS is
   [{:repo :sha :subject}] as `happened-commits` parses the note.

   OPTS:
   :preceding-offer  id of the offer in the previous turn's reply: this
                     turn's first approve fragment accepts it (explicit
                     evidence). Without it an approve stays :approve.
   :carries-out      offer id this turn's commits carry out (an offer the
                     previous turn accepted).

   Returns the act vector, with ^{:skipped n} metadata counting fragments
   and marked paragraphs whose intent had no kernel kind."
  ([record reading reply-text commits]
   (turn->acts record reading reply-text commits {}))
  ([record reading reply-text commits {:keys [preceding-offer carries-out]}]
   (let [{frag-acts :acts frag-skipped :skipped} (fragment-acts record reading)
         frag-acts (cond-> frag-acts
                     preceding-offer
                     (->> (reduce (fn [[acts accepted?] act]
                                    (if (and (not accepted?) (accept-fragment? act))
                                      [(conj acts (approve->accept act preceding-offer)) true]
                                      [(conj acts act) accepted?]))
                                  [[] false])
                          first))
         {reply-acts :acts reply-skipped :skipped} (reply-acts record reply-text)
         commit-acts (cond-> (commit-acts record commits)
                       carries-out (->> (mapv #(assoc % :carries-out carries-out))))]
     (with-meta (vec (concat frag-acts reply-acts commit-acts))
       {:skipped (+ frag-skipped reply-skipped)}))))

;; ---------------------------------------------------------------------------
;; Session

(defn session-acts
  "The acts of TURNS — seqs of {:record :reading :reply :commits} —
   ordered by the records' created_at, with the two cross-turn links
   stated from explicit evidence:

   - an approve fragment accepts the offer of the previous turn's reply
     (:preceding-offer);
   - a commit carries out the offer accepted in its own turn or in the
     turn before it (:carries-out).

   ^{:skipped n} metadata totals the skipped no-kind fragments and
   paragraphs over the session."
  [turns]
  (let [ordered (sort-by #(get-in % [:record :created_at]) turns)]
    (loop [todo ordered
           acts []
           skipped 0
           preceding-offer nil
           accepted nil]
      (if-let [{:keys [record reading reply commits]} (first todo)]
        (let [turn-acts (turn->acts record reading reply commits
                                    {:preceding-offer preceding-offer
                                     :carries-out accepted})
              skipped-here (:skipped (meta turn-acts))
              accepted-here (some (fn [{:keys [kind target]}]
                                    (when (= kind :accept) target))
                                  turn-acts)
              ;; A commit carries out the offer accepted in its own turn
              ;; as well as one accepted the turn before.
              turn-acts (with-meta
                          (if accepted-here
                            (mapv (fn [act]
                                    (if (= :commit (:kind act))
                                      (assoc act :carries-out accepted-here)
                                      act))
                                  turn-acts)
                            turn-acts)
                          {:skipped skipped-here})
              offer-here (some (fn [{:keys [kind author id]}]
                                 (when (and (contains? #{:offer :ask-action} kind)
                                            (= author (:agent_id record)))
                                   id))
                               (reverse turn-acts))]
          (recur (rest todo)
                 (into acts turn-acts)
                 (+ skipped skipped-here)
                 offer-here
                 accepted-here))
        (with-meta acts {:skipped skipped})))))

;; ---------------------------------------------------------------------------
;; The kernel's answer for one turn

(defn- instant-minus-1s
  [iso]
  (str (.minusSeconds (java.time.Instant/parse iso) 1)))

(defn turn-ports
  "Which ports closed in TURN-ID and which are still open after it, over
   ACTS (a session-acts vector): {:closed-this-turn [act-id ..]
   :still-open [{:act :kind :since}] :obligations [{:debtor :creditor
   :source}]}. The kernel's openo/closedo/obligationo answer, as of the
   turn's own timestamp."
  [acts turn-id]
  (let [at (some (fn [{:keys [turn at]}] (when (= turn turn-id) at)) acts)
        _ (when-not at
            (throw (ex-info (str "No acts for turn " turn-id)
                            {:reason :unknown-turn :turn turn-id})))
        db (lx/db acts)
        before (lx/open-ports db (instant-minus-1s at))
        after (lx/open-ports db at)
        by-id (into {} (map (juxt :id identity) acts))]
    {:closed-this-turn (vec (sort (set/difference before after)))
     :still-open (vec (sort-by :act
                               (map (fn [id]
                                      {:act id
                                       :kind (:kind (by-id id))
                                       :since (:at (by-id id))})
                                    after)))
     :obligations (vec (lx/obligations db at))}))
