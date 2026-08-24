(ns futon3c.apm.library-loop-checkpoint
  "Structured strategy checkpoints and independent progress review."
  (:require [clojure.string :as str])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

(def strength-claims #{:reduced :equivalent :stronger-or-unclear})
(def review-rulings #{:reduced :equivalent :stronger-or-unclear})

(defn- refuse! [finding data]
  (throw (ex-info (name finding) (assoc data :finding finding))))

(defn normalize-statement [statement]
  (-> statement str/trim (str/replace #"\s+" " ")))

(defn- sha256 [content]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (str content) StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn statement-digest [statement]
  (sha256 (normalize-statement statement)))

(defn- canonical [value]
  (cond
    (map? value) (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                       (map (fn [[k v]] [k (canonical v)])) value)
    (set? value) (vec (sort-by pr-str (map canonical value)))
    (sequential? value) (mapv canonical value)
    :else value))

(defn validate-checkpoint
  "Validates an agent-authored checkpoint. The strength remains a claim, not
  a ruling. The declaration text is retained so its normalized digest can be
  independently checked."
  [checkpoint]
  (let [{:keys [id declaration statement statement-digest dependencies
                strength reduction-witness next-plan]} checkpoint]
    (when-not (keyword? id)
      (refuse! :invalid-obligation-id {:checkpoint checkpoint}))
    (when-not (or (symbol? declaration)
                  (and (string? declaration) (not (str/blank? declaration))))
      (refuse! :invalid-declaration {:checkpoint checkpoint}))
    (when-not (and (string? statement) (not (str/blank? statement))
                   (= statement-digest
                      (futon3c.apm.library-loop-checkpoint/statement-digest
                       statement)))
      (refuse! :statement-digest-mismatch {:checkpoint checkpoint}))
    (when-not (and (set? dependencies)
                   (every? #(or (keyword? %) (symbol? %)
                                (and (string? %) (not (str/blank? %))))
                           dependencies))
      (refuse! :invalid-dependencies {:checkpoint checkpoint}))
    (when-not (contains? strength-claims strength)
      (refuse! :invalid-agent-strength-claim {:checkpoint checkpoint}))
    (when-not (and (string? reduction-witness)
                   (not (str/blank? reduction-witness)))
      (refuse! :missing-reduction-witness {:checkpoint checkpoint}))
    (when-not (and (string? next-plan) (not (str/blank? next-plan)))
      (refuse! :missing-next-plan {:checkpoint checkpoint}))
    checkpoint))

(defn checkpoint-digest [checkpoint]
  (sha256 (pr-str (canonical (validate-checkpoint checkpoint)))))

(defn validate-review
  [checkpoint review]
  (validate-checkpoint checkpoint)
  (when-not (= (checkpoint-digest checkpoint) (:checkpoint-digest review))
    (refuse! :review-checkpoint-mismatch {:review review}))
  (when-not (= (:id checkpoint) (:obligation-id review))
    (refuse! :review-obligation-mismatch {:review review}))
  (when-not (contains? review-rulings (:ruling review))
    (refuse! :invalid-review-ruling {:review review}))
  (when-not (and (string? (:rationale review))
                 (not (str/blank? (:rationale review))))
    (refuse! :missing-review-rationale {:review review}))
  (when-not (boolean? (:approved? review))
    (refuse! :missing-review-approval {:review review}))
  review)

(defn review-decision
  "Returns the typed outcome a runner may persist in its review receipt.
  Non-reduction history follows an explicitly superseded obligation id, so an
  agent cannot rename an obligation to reset the valve."
  [{previous-id :obligation/id
    previous-count :consecutive-nonreductions
    :as state}
   checkpoint review]
  (validate-review checkpoint review)
  (let [current-id (:id checkpoint)
        changed? (and previous-id (not= previous-id current-id))
        supersession (:supersedes review)]
    (when (and changed?
               (not (and (= previous-id (:id supersession))
                         (string? (:rationale supersession))
                         (not (str/blank? (:rationale supersession))))))
      (refuse! :obligation-id-change-without-supersession
               {:previous-id previous-id :current-id current-id :review review}))
    (let [ruling (:ruling review)
          count (if (= :reduced ruling) 0 (inc (or previous-count 0)))
          valve-open? (< count 2)
          authorized? (and (:approved? review) valve-open?)]
      {:schema 1
       :outcome (if authorized? :approved :rejected)
       :bank-authorized? authorized?
       :progress-ruling ruling
       :review-rationale (:rationale review)
       :obligation-id current-id
       :consecutive-nonreductions count
       :checkpoint-digest (:checkpoint-digest review)
       :problem-id (:problem-id state)
       :turn (:turn state)
       :checkpoint (:checkpoint state)
       :head-sha (:head-sha state)
       :supersedes supersession
       :finding (cond
                  (not (:approved? review)) :review-not-approved
                  (not valve-open?) :checkpoint-nonreduction-limit
                  :else nil)})))

(defn valid-decision?
  "Checks the state-bound fields the durable runner must verify before a
  review receipt can authorize banking."
  [state decision]
  (let [ruling (:progress-ruling decision)
        current-id (:obligation-id decision)
        previous-id (:obligation/id state)
        expected-count (if (= :reduced ruling)
                         0
                         (inc (:consecutive-nonreductions state)))
        supersession (:supersedes decision)
        id-bound? (or (nil? previous-id)
                      (= previous-id current-id)
                      (and (= previous-id (:id supersession))
                           (string? (:rationale supersession))
                           (not (str/blank? (:rationale supersession)))))]
    (and (= 1 (:schema decision))
         (contains? #{:approved :rejected} (:outcome decision))
         (boolean? (:bank-authorized? decision))
         (keyword? current-id)
         (contains? review-rulings ruling)
         (string? (:review-rationale decision))
         (not (str/blank? (:review-rationale decision)))
         (string? (:checkpoint-digest decision))
         (re-matches #"[0-9a-f]{64}" (:checkpoint-digest decision))
         (= expected-count (:consecutive-nonreductions decision))
         (= (:problem-id state) (:problem-id decision))
         (= (:turn state) (:turn decision))
         (= (:checkpoint state) (:checkpoint decision))
         (= (:head-sha state) (:head-sha decision))
         id-bound?
         (or (not (:bank-authorized? decision))
             (and (= :approved (:outcome decision))
                  (< expected-count 2))))))
