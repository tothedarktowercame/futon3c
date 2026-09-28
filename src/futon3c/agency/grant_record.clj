(ns futon3c.agency.grant-record
  "P3-1 explicit, sourced grants. No dispatch graph, harness tagging or inference.
   Domain valid-time intervals are [from,until); storage uses from. An open
   storage record is not proof of authority after its domain until.
   CLI validates against live source/parent reads by default; --write mints."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.agreement-record :as agreement-record]
            [futon3c.agency.rule-record :as store])
  (:import [java.time Instant]
           [java.net URLEncoder]))

(defn- refuse! [reason field]
  (throw (ex-info "Invalid explicit grant" {:reason reason :field field})))
(defn- text? [v] (and (string? v) (not (str/blank? v))))
(defn- act? [v] (and (text? v) (str/starts-with? v "act:")))
(defn- stamp! [v]
  (try (Instant/parse v) (catch Exception _ (refuse! :invalid-interval :grant/interval))))
(defn- before? [a b] (.isBefore ^Instant (stamp! a) (stamp! b)))
(defn- body [r]
  (let [b (:evidence/body r)]
    (if (string? b) (edn/read-string b) b)))
(defn- field [m k] (or (get m k) (get m (name k))))
(defn- props [r] (dissoc (:hx/props r) :grant/schema :act/harness))

(defn- scope! [s]
  (when-not (and (map? s) (every? #{:description :act-kinds :rule-ids :own-acts-only
                                    :provisional-only} (keys s))
                 (text? (:description s))) (refuse! :invalid-scope :grant/scope))
  (doseq [k [:own-acts-only :provisional-only]]
    (when (and (contains? s k) (not (true? (get s k))))
      (refuse! :invalid-scope k)))
  (doseq [k [:act-kinds :rule-ids] :when (contains? s k)]
    (let [v (get s k)]
      (when-not (and (vector? v) (seq v) (= (count v) (count (set v)))
                     (every? (if (= k :act-kinds) keyword? act?) v))
        (refuse! :invalid-scope k))))
  s)
(defn- checkable? [s] (boolean (or (seq (:act-kinds s)) (seq (:rule-ids s)))))

(defn- source-kind! [source]
  (cond
    (and (map? source)
         (= #{:id :author :at :quote} (set (keys source)))
         (every? text? (vals source)))
    :evidence

    (and (map? source)
         (= #{:kind :offer :agreement} (set (keys source)))
         (= :agreement (:kind source))
         (act? (:offer source))
         (act? (:agreement source)))
    :agreement

    :else (refuse! :unsourced-grant :grant/source)))

(defn- shape! [r]
  (when-not (and (map? r)
                 (= (set (keys (dissoc r :grant/parent))) #{:grant/grantor :grant/grantee :grant/scope
                                    :grant/interval :grant/source :grant/basis}))
    (refuse! :missing-grant :record))
  (when-not (= :explicit (:grant/basis r)) (refuse! :interpretation-not-grant :grant/basis))
  (doseq [k [:grant/grantor :grant/grantee]]
    (when-not (text? (get r k)) (refuse! :missing-grant k)))
  (scope! (:grant/scope r))
  (when (and (= "*" (:grant/grantee r))
             (= :agreement (source-kind! (:grant/source r))))
    (refuse! :agreement-grantee-wildcard :grant/grantee))
  (when (= "*" (:grant/grantee r))
    (when (or (:grant/parent r) (not= "joe" (:grant/grantor r)))
      (refuse! :wildcard-not-root :grant/grantee))
    (when-not (true? (get-in r [:grant/scope :own-acts-only]))
      (refuse! :wildcard-needs-own-acts :grant/scope)))
  (let [{:keys [from until] :as interval} (:grant/interval r)]
    (when-not (and (map? interval) (contains? interval :from)
                   (every? #{:from :until} (keys interval)))
      (refuse! :invalid-interval :grant/interval))
    (stamp! from)
    (when until (when-not (before? from until) (refuse! :invalid-interval :grant/interval))))
  (let [s (:grant/source r)]
    (case (source-kind! s)
      :evidence
      (do
        (stamp! (:at s))
        (when-not (= (:author s) (:grant/grantor r))
          (refuse! :source-author-mismatch :grant/source))
        (when (before? (get-in r [:grant/interval :from]) (:at s))
          (refuse! :grant-before-source :grant/interval)))
      :agreement
      (do
        (when (:grant/parent r)
          (refuse! :agreement-grant-not-root :grant/parent))
        (when-not (= "joe" (:grant/grantor r))
          (refuse! :agreement-grantor-mismatch :grant/grantor))
        (when (= "*" (:grant/grantee r))
          (refuse! :agreement-grantee-wildcard :grant/grantee)))))
  (if-let [parent (:grant/parent r)]
    (when-not (act? parent) (refuse! :invalid-parent :grant/parent))
    (when-not (= "joe" (:grant/grantor r)) (refuse! :non-operator-root :grant/grantor)))
  r)

(defn- index! [records]
  (let [ids (map :hx/id records)]
    (when-not (and (every? act? ids) (= (count ids) (count (set ids)))
                   (every? #(= :grant/record (:hx/type %)) records))
      (refuse! :invalid-grant-records :records))
    (into {} (map (juxt :hx/id identity) records))))

(defn- chain! [r by-id seen]
  (shape! r)
  (if-let [id (:grant/parent r)]
    (do
      (when (contains? seen id) (refuse! :parent-cycle :grant/parent))
      (let [parent (get by-id id)]
        (when-not parent (refuse! :broken-parent-chain :grant/parent))
        (let [p (props parent)
              ancestors (chain! p by-id (conj seen id))
              cscope (:grant/scope r) pscope (:grant/scope p)
              ci (:grant/interval r) pi (:grant/interval p)]
          (when (= "*" (:grant/grantee p))
            (refuse! :wildcard-not-delegable :grant/parent))
          (when-not (= (:grant/grantor r) (:grant/grantee p))
            (refuse! :delegation-identity-mismatch :grant/parent))
          (when-not (and (checkable? cscope) (checkable? pscope))
            (refuse! :scope-unchecked :grant/scope))
          (doseq [k [:act-kinds :rule-ids]]
            (when-not (set/subset? (set (get cscope k)) (set (get pscope k)))
              (refuse! :scope-exceeds-parent :grant/scope)))
          (when (or (before? (:from ci) (:from pi))
                    (and (:until pi) (or (nil? (:until ci))
                                        (before? (:until pi) (:until ci)))))
            (refuse! :interval-exceeds-parent :grant/interval))
          (conj ancestors parent))))
    []))

(defn- exactly-one! [records id]
  (let [matches (filter #(= id (:id %)) records)]
    (when-not (= 1 (count matches))
      (refuse! :unsourced-grant :grant/source))
    (first matches)))

(defn- validate-agreement-source!
  [grant offers agreements]
  (let [source (:grant/source grant)
        agreement (exactly-one! agreements (:agreement source))
        offer (exactly-one! offers (:offer source))]
    (when-not (= (:offer source) (:agreement/offer agreement))
      (refuse! :agreement-source-mismatch :grant/source))
    (agreement-record/validate-against-offer! agreement offer)
    (when-not (= "joe" (:grant/grantor grant) (:agreement/acceptor agreement))
      (refuse! :agreement-grantor-mismatch :grant/grantor))
    (when-not (= (:grant/grantee grant) (:agreement/offeror agreement))
      (refuse! :agreement-grantee-mismatch :grant/grantee))
    (let [grant-scope (:grant/scope grant)
          agreement-scope (:agreement/scope agreement)]
      (when-not (checkable? agreement-scope)
        (refuse! :unchecked-agreement-scope :grant/source))
      (doseq [key [:act-kinds :rule-ids]]
        (when-not (set/subset? (set (get grant-scope key))
                               (set (get agreement-scope key)))
          (refuse! :scope-exceeds-agreement :grant/scope))))
    (when (before? (get-in grant [:grant/interval :from])
                   (:agreement/at agreement))
      (refuse! :grant-before-source :grant/interval))))

(defn validate!
  "Validate against independently fetched evidence, agreements, offers, and
   grant ancestors. An evidence-source quote remains a verbatim witness, not
   an NLP classifier. Text-only child or agreement scope cannot confer authority."
  [r {:keys [evidence records offers agreements]}]
  (when (contains? r :act/harness)
    (act-harness/validate! (:act/harness r)))
  (let [record (dissoc r :act/harness)
        by-id (index! (or records []))
        chain (chain! record by-id #{})
        sources (group-by :evidence/id evidence)]
    (doseq [g (conj (mapv props chain) record)]
      (let [s (:grant/source g)]
        (case (source-kind! s)
          :evidence
          (let [matches (get sources (:id s)) e (first matches)
                b (when e (body e))]
            (when-not (= 1 (count matches)) (refuse! :unsourced-grant :grant/source))
            (when-not (= (:grant/grantor g) (:evidence/author e) (:author s))
              (refuse! :source-author-mismatch :grant/source))
            (when-not (and (= (:at s) (:evidence/at e))
                           (text? (field b :text)) (str/includes? (field b :text) (:quote s))
                           (not= "harness" (some-> (get-in e [:evidence/origin :kind]) name))
                           (if (= "joe" (:grant/grantor g))
                             (= "user" (field b :role)) true))
              (refuse! :unsourced-grant :grant/source)))
          :agreement
          (validate-agreement-source! g offers agreements)))))
  r)

(defn grant-covers?
  "Return {:status :granted :chain [root ... leaf]} or typed :no-grant. Records
   are validated stored grants. Query checks every ancestor, time, and scope;
   text-only scope never answers yes, even when its text mentions the target."
  ([records grantee target at] (grant-covers? records grantee target at nil))
  ([records grantee target at leaf-or-options]
  (try
    (stamp! at)
    ;; "*" names a grant's audience, never the party asking: asking as "*"
    ;; with signer "*" would otherwise satisfy the own-acts check.
    (when (= "*" grantee) (refuse! :wildcard-not-a-grantee :grantee))
    (let [{:keys [leaf-id target-signer effect-status]}
          (if (map? leaf-or-options) leaf-or-options {:leaf-id leaf-or-options})
          by-id (index! records)
          candidates (sort-by :hx/id (filter #(and (contains? #{grantee "*"}
                                                                (get-in % [:hx/props :grant/grantee]))
                                              (or (nil? leaf-id) (= leaf-id (:hx/id %)))) records))
          results (for [leaf candidates]
                    (try
                      (let [chain (conj (chain! (props leaf) by-id #{(:hx/id leaf)}) leaf)]
                        (doseq [node chain]
                          (let [r (props node) s (:grant/scope r) {:keys [from until]} (:grant/interval r)]
                            (when-not (checkable? s) (refuse! :scope-unchecked :grant/scope))
                            (when (and (:own-acts-only s) (not= grantee target-signer))
                              (refuse! :not-own-act :target-signer))
                            (when (and (:provisional-only s)
                                       (not= :provisional effect-status))
                              (refuse! :not-provisional :effect-status))
                            (when-not (contains? (set (concat (:act-kinds s) (:rule-ids s))) target)
                              (refuse! :out-of-scope :grant/scope))
                            (when (or (before? at from) (and until (not (before? at until))))
                              (refuse! :out-of-time :grant/interval))))
                        {:status :granted :chain chain})
                      (catch clojure.lang.ExceptionInfo e {:status :no-grant :reason (:reason (ex-data e))})))]
      (or (first (filter #(= :granted (:status %)) results))
          (first results) {:status :no-grant :reason :no-candidate}))
    (catch clojure.lang.ExceptionInfo e {:status :no-grant :reason (:reason (ex-data e))}))))

(defn grant-status-for
  "P13b-style adoption lookup; match the explicit source as well as scope/time.
   Does not change the old adoption or clearance record."
  [records grantee target adoption]
  (let [ref (get-in adoption [:source :ref])
        source (when (string? ref) (str/replace-first ref #"^evidence:" ""))
        matching (filter #(= source (get-in % [:hx/props :grant/source :id])) records)
        answers (for [r matching]
                  (grant-covers? records grantee target (:at adoption) (:hx/id r)))
        granted (first (filter #(= :granted (:status %)) answers))]
    (if granted {:status :recorded :act-id (:hx/id (last (:chain granted)))}
        {:status :unrecorded :reason :no-covering-explicit-grant})))

(defn- drop-nils
  "futon1b does not store nil map values, so an absent key is how an open
   interval or a root grant is written; the payload must match its readback."
  [m]
  (into {} (keep (fn [[k v]] (when-not (nil? v) [k (if (map? v) (drop-nils v) v)]))) m))

(defn payload
  ([request context]
   (payload request context (act-harness/plain "cli:futon3c.agency.grant-record")))
  ([{:keys [idempotency-key] :as request} context harness]
   (when-not (= #{:record :idempotency-key} (set (keys request)))
     (refuse! :invalid-request :request))
   (when-not (text? idempotency-key) (refuse! :invalid-request :idempotency-key))
   (let [record (drop-nils (:record request))]
     (validate! record context)
     {:hx/type :grant/record :hx/mint-id true :hx/idempotency-key idempotency-key
      :hx/valid-time (get-in record [:grant/interval :from])
      :hx/endpoints (cond-> [(or (get-in record [:grant/source :id])
                                 (get-in record [:grant/source :agreement]))
                             (str "agent:" (:grant/grantee record))]
                      (:grant/parent record) (conj (:grant/parent record)))
      :hx/props (assoc record :grant/schema 1
                       :act/harness (act-harness/validate! harness))})))

(defn- path-id [prefix id] (str prefix (URLEncoder/encode id "UTF-8")))
(defn live-context! [base record]
  (loop [r record records [] evidence [] seen #{}]
    (let [source (store/request! base "GET" (path-id "/api/alpha/evidence/" (get-in r [:grant/source :id])) nil)
          parent (:grant/parent r)]
      (if parent
        (do (when (contains? seen parent) (refuse! :parent-cycle :grant/parent))
            (let [p (store/request! base "GET" (path-id "/api/alpha/hyperedge/" parent) nil)]
              (recur (props p) (conj records p) (conj evidence source) (conj seen parent))))
        {:records records :evidence (vec (vals (into {} (map (juxt :evidence/id identity) (conj evidence source)))))}))))

(defn write-with-context!
  "Mint REQUEST after validation against already fetched CONTEXT, then verify
   the stored record by id. This is the callable form used when another write
   has already fetched and validated its agreement and offer."
  ([base request context]
   (write-with-context! base request context
                        (act-harness/plain "cli:futon3c.agency.grant-record")))
  ([base request context harness]
   (let [p (payload request context harness)
         receipt (store/request! base "POST" "/api/alpha/hyperedge" p)
         id (:hx/id receipt)]
     (when-not (and (:ok receipt) (act? id)) (refuse! :missing-minted-receipt :receipt))
     (let [stored (store/request! base "GET" (path-id "/api/alpha/hyperedge/" id) nil)]
       (when-not (= (select-keys p [:hx/type :hx/endpoints :hx/props])
                    (select-keys stored [:hx/type :hx/endpoints :hx/props]))
         (refuse! :readback-mismatch :receipt))
       (assoc receipt :verified? true)))))

(defn write!
  ([base request]
   (write! base request (act-harness/plain "cli:futon3c.agency.grant-record")))
  ([base request harness]
   (write-with-context! base request (live-context! base (:record request)) harness)))

(defn -main [& args]
  (try
    (let [{:keys [write? file harness]}
          (act-harness/parse-cli args "cli:futon3c.agency.grant-record"
                                 "Usage: grant-record [--write] [--harness-kind KIND --harness-execution-id ID] FILE.edn")
          base (or (System/getenv "FUTON1B_URL") "http://127.0.0.1:7073")
          request (edn/read-string (slurp file))]
      (prn (if write? (write! base request harness)
               {:ok true :dry-run? true
                :payload (payload request (live-context! base (:record request)) harness)})))
    (shutdown-agents)
    (catch Exception e
      (binding [*out* *err*] (prn {:ok false :message (.getMessage e) :detail (ex-data e)}))
      (shutdown-agents) (System/exit 1))))
