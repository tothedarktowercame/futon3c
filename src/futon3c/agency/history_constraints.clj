(ns futon3c.agency.history-constraints
  "Constraints are relational queries over recorded acts, never current queue state.
   'Evaluated as each act arrives' is approximated by an incremental cursor run;
   this namespace does not hook the evidence write path. A cursor is an XTDB
   system-as-of boundary, not evidence/at, so late insertion is still a new act."
  (:refer-clojure :exclude [run!])
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [cheshire.core :as json]
            [futon3c.agency.promise-history :as history]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as store])
  (:import [java.security MessageDigest]
           [java.time Instant]))

(def constraints
  [{:id :constraint/no-anonymous-bell-v1
    :query '[[:act ?bell {:type :invoke-start :surface "bell" :caller "http-caller"}]]
    :witnesses '[?bell]}
   ;; No park↔followup association is stored. Ready delivery uses the exact park
   ;; id, so this equivalent withdrawal constraint needs no inferred join.
   {:id :constraint/no-ready-at-budget-retraction-v1
    :query '[[:act ?ready {:type :promise/ready-enqueued :promise-id ?park :sequence ?q}]
             [:act ?retraction {:type :promise/budget-exhausted :promise-id ?park :sequence ?r}]
             [:< ?q ?r]
             [:not [[:act ?ack {:type :promise/ready-acked :promise-id ?park :sequence ?a}]
                    [:< ?q ?a] [:<= ?a ?r]]]]
    :witnesses '[?ready ?retraction]}])

(defn- variable? [x] (and (symbol? x) (str/starts-with? (str x) "?")))
(defn- bind-value [env pattern value]
  (if (variable? pattern)
    (if (contains? env pattern) (when (= (get env pattern) value) env) (assoc env pattern value))
    (when (= pattern value) env)))
(defn- value [env x] (if (variable? x) (get env x) x))

(defn query
  "Small declarative relation evaluator: act-pattern joins, numeric inequalities,
   and correlated not-exists. Predicates apply to normalized acts only."
  ([acts clauses] (query acts clauses [{}]))
  ([acts clauses environments]
   (reduce
    (fn [envs [op a b]]
      (mapcat
       (fn [env]
         (case op
           :act (keep (fn [act]
                        (reduce (fn [e [k pattern]]
                                  (if e (bind-value e pattern (get act k)) (reduced nil)))
                                (bind-value env a (:id act)) b)) acts)
           :< (when (< (value env a) (value env b)) [env])
           :<= (when (<= (value env a) (value env b)) [env])
           :not (when (empty? (query acts a [env])) [env])
           (throw (ex-info "Unknown constraint operator" {:operator op})))) envs))
    environments clauses)))

(defn- field [m k] (or (get m k) (get m (name k))))
(defn- body [row]
  (let [b (:evidence/body row)]
    (cond (map? b) b
          (string? b) (try (edn/read-string b)
                           (catch Exception _ (try (json/parse-string b true) (catch Exception _ {}))))
          :else {})))
(defn invoke? [row] (= "invoke-start" (field (body row) :event)))

(defn- invoke-act [row]
  (let [preview (field (body row) :prompt-preview)
        ;; Only the transport-generated leading header counts. Quoted headers
        ;; later in user content must never become caller evidence.
        lines (when (and (string? preview) (str/starts-with? preview "--- CURRENT TURN ---\n"))
                (take-while #(and (not (str/blank? %)) (not= "---" %))
                            (rest (str/split-lines preview))))
        header (into {} (keep #(when-let [[_ k v] (re-matches #"(Surface|Caller|From): (.+)" %)] [k (str/trim v)]) lines))
        caller (or (get header "Caller") (get header "From"))]
    (when (and (get header "Surface") caller
               (or (nil? (get header "Caller")) (nil? (get header "From"))
                   (= (get header "Caller") (get header "From"))))
      {:id (:evidence/id row) :type :invoke-start :at (:evidence/at row)
       :surface (get header "Surface") :caller caller})))

(defn normalize-history [rows]
  (let [promises (filter #(= "promise" (namespace (:evidence/type %))) rows)
        issues (history/check-chains promises)
        malformed (keep (fn [row]
                          (try
                            (let [b (:evidence/body row) p (history/payload row) r (:record p)]
                              (when-not (and (map? r) (string? (:history/promise-id b))
                                             (= (:history/promise-id b) (or (:id r) (:followup-id r) (:park-id r)))
                                             (= (:predecessor p) (:history/predecessor b)))
                                (:history/promise-id b)))
                            (catch Exception _ (get-in row [:evidence/body :history/promise-id])))) promises)
        duplicates (for [[pid rows] (group-by #(get-in % [:evidence/body :history/promise-id]) promises)
                         [_ same] (group-by #(get-in % [:evidence/body :history/promise-sequence]) rows)
                         :when (> (count same) 1)] pid)
        incomplete (set (concat (map :promise-id issues) malformed duplicates))
        decoded (for [row promises :let [b (:evidence/body row)]
                      :when (and (string? (:history/promise-id b))
                                 (not (contains? incomplete (:history/promise-id b))))]
                  {:id (:evidence/id row) :type (:evidence/type row) :at (:evidence/at row)
                   :promise-id (:history/promise-id b) :sequence (:history/promise-sequence b)})
        invokes (filter invoke? rows)
        recognized (keep invoke-act invokes)]
    {:acts (vec (concat recognized (remove nil? decoded)))
     :incomplete-promises (vec (sort-by str incomplete))
     :unclassified-invokes (- (count invokes) (count recognized))}))

(defn violation-id [constraint-id witnesses]
  (let [bytes (.digest (MessageDigest/getInstance "SHA-256")
                       (.getBytes (pr-str [constraint-id (vec (sort witnesses))]) "UTF-8"))]
    (str "constraint-violation:" (apply str (map #(format "%02x" (bit-and 255 %)) bytes)))))

(defn match-history
  "Evaluate every registered query on full history. NEW-IDS selects affected
   witness sets, not constrained record types: a retraction must trigger joins.
   SINCE restricts reported witness event times; context can precede SINCE."
  [rows new-ids since]
  (let [{:keys [acts] :as normalized} (normalize-history rows)
        by-id (into {} (map (juxt :id identity)) acts)
        affected-promises (set (keep #(get-in % [:evidence/body :history/promise-id])
                                     (filter #(contains? new-ids (:evidence/id %)) rows)))
        matches (for [{:keys [id witnesses] clauses :query} constraints
                      env (query acts clauses)
                      :let [ids (vec (sort (map env witnesses)))]
                      :when (and (or (some new-ids ids)
                                      (some #(contains? affected-promises (:promise-id (by-id %))) ids))
                                 (some #(not (.isBefore (Instant/parse (:at (by-id %))) (Instant/parse since))) ids))]
                  {:constraint id :witnesses ids
                   :witness-times (mapv #(:at (by-id %)) ids)
                   :id (violation-id id ids)})]
    (assoc (dissoc normalized :acts) :violations (vec (distinct matches)))))

(defn run!
  "READ supplies a complete history at a system-time cursor. No cursor file is
   written: the successful next cursor is returned. Only violation evidence may
   be appended. Failed writes leave the prior cursor so retry is idempotent."
  [{:keys [read-history backend cursor until since dry-run?]
    :or {dry-run? true}}]
  (when (and cursor (not= since (:since cursor)))
    (throw (ex-info "Cursor scope mismatch" {:cursor cursor :since since})))
  (let [rows (read-history until)
        prior (if cursor (read-history (:system-as-of cursor)) [])
        old-ids (set (map :evidence/id prior))
        new-ids (set (remove old-ids (map :evidence/id rows)))
        result (match-history rows new-ids since)
        receipts
        (when-not dry-run?
          (mapv
           (fn [{:keys [id constraint witnesses] :as violation}]
             (letfn [(same? [row] (= [constraint witnesses]
                                    [(get-in row [:evidence/body :constraint])
                                     (get-in row [:evidence/body :witnesses])]))]
               (if-let [existing (store/get-entry* backend id)]
                 {:id id :ok (same? existing) :status :existing}
                 (let [receipt (boundary/append! backend
                                {:evidence/id id :evidence/type :constraint/violation
                                 :evidence/claim-type :observation :evidence/author "constraint-matcher"
                                 :evidence/subject {:ref/type :agent :ref/id "constraint-matcher"}
                                 :evidence/at until :evidence/tags [:constraint-violation]
                                 :evidence/body (assoc violation :evaluated-as-of until)})]
                   (if (= :duplicate-id (:error/code receipt))
                     {:id id :ok (same? (store/get-entry* backend id)) :status :existing}
                     (assoc (select-keys receipt [:ok :error/code]) :id id :status :written))))))
           (:violations result)))
        ok? (every? :ok receipts)]
    (assoc result :ok ok? :dry-run? dry-run? :history-count (count rows) :new-acts (count new-ids)
           :receipts (vec receipts)
           ;; A dry run must not advance the committed evaluation boundary.
           :cursor (if (and ok? (not dry-run?)) {:system-as-of until :since since} cursor)
           :candidate-cursor {:system-as-of until :since since})))
