(ns futon3c.agency.operator-turn-source
  "Pure provenance resolution from an operator chat turn to source jobs.

   The resolver never examines displayed text. A chained predecessor produced
   by a park resume resolves through its stamped park id and the persisted
   promise/woken row. A directly delivered completion resolves through the
   delivery job's stored :bellback-of. Other predecessors bind to nothing."
  (:require [futon3c.agency.promise-history :as promise-history]))

(defn- field [m k]
  (or (get m k) (get m (name k))))

(defn- body [entry]
  (or (:evidence/body entry) (get entry "evidence/body") {}))

(defn- origin [entry]
  (or (:evidence/origin entry) (get entry "evidence/origin") {}))

(defn- evidence-id [entry]
  (or (:evidence/id entry) (get entry "evidence/id")))

(defn- session-id [entry]
  (or (:evidence/session-id entry) (get entry "evidence/session-id")))

(defn- reply-id [entry]
  (or (:evidence/in-reply-to entry) (get entry "evidence/in-reply-to")))

(defn- parked-resume? [entry]
  (let [source-ref (some-> (or (get-in entry [:evidence/harness :source-ref])
                               (get-in entry ["evidence/harness" "source-ref"]))
                           str)]
    (or (= "parked-resume" (some-> (field (origin entry) :actor) str))
        (and source-ref (boolean (re-find #"^park[:-]" source-ref))))))

(defn- park-id [entry]
  (some-> (or (field (origin entry) :source-id)
              (get-in entry [:evidence/harness :source-ref])
              (get-in entry ["evidence/harness" "source-ref"]))
          str))

(defn- woken-record [entry]
  (when (= :promise/woken (:evidence/type entry))
    (try (let [decoded (:record (promise-history/payload entry))
               projected (:awaiting (body entry))]
           (cond-> decoded projected (assoc :awaiting projected)))
         (catch Throwable _ (body entry)))))

(defn- chat-role [entry]
  (some-> (field (body entry) :role) str))

(defn source-jobs-for-turn
  "Resolve OPERATOR-TURN's structured predecessor.

   PREVIOUS-AGENT-TURN may be enriched with :bellback-of from the durable
   delivery job. PARK-HISTORY is a collection of promise-history evidence.
   Returns source jobs in stored order (deduplicated), never IDs parsed from
   any display text."
  [operator-turn previous-agent-turn park-history]
  (let [same-chain? (and operator-turn previous-agent-turn
                         (= "user" (chat-role operator-turn))
                         (= "assistant" (chat-role previous-agent-turn))
                         (or (= (reply-id operator-turn)
                                (evidence-id previous-agent-turn))
                             (= (:chain/previous-agent-id operator-turn)
                                (evidence-id previous-agent-turn)))
                         (= (session-id operator-turn)
                            (session-id previous-agent-turn)))]
    (cond
      (not same-chain?) {:source-jobs [] :basis :none}

      (parked-resume? previous-agent-turn)
      (let [pid (park-id previous-agent-turn)
            rec (some (fn [entry]
                        (let [record (woken-record entry)]
                          (when (= pid (some-> (:id record) str)) record)))
                      park-history)
            jobs (->> (:awaiting rec) (map str) distinct sort vec)]
        (if (seq jobs)
          {:source-jobs jobs :basis :park-resume :park-id pid}
          {:source-jobs [] :basis :none :park-id pid}))

      (some? (:bellback-of previous-agent-turn))
      {:source-jobs [(str (:bellback-of previous-agent-turn))]
       :basis :bellback}

      :else {:source-jobs [] :basis :none})))
