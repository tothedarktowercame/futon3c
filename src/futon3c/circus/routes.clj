(ns futon3c.circus.routes
  "Read-only HTTP-facing adapter for Circus QA. Maps and keyword values are
  normalized here so transport/http remains a thin Ring adapter."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.circus.chain-record :as chain]
            [futon3c.circus.qa :as qa]
            [futon3c.giraffe.run :as giraffe]
            [futon3c.xiang.turn-service :as turn-service]
            [futon3c.xiang.turn-store :as turns]))

(def default-root giraffe/default-root)
(def default-certificate-root
  "/home/joe/code/futon3c/data/circus-certificates")

(defn json-ready [x]
  (cond
    (map? x) (into {} (map (fn [[k v]] [(if (keyword? k) (name k) (str k))
                                         (json-ready v)])) x)
    (keyword? x) (name x)
    (set? x) (mapv json-ready (sort-by pr-str x))
    (sequential? x) (mapv json-ready x)
    :else x))

(defn- run-generated-at [manifest]
  (or (:generated_at manifest) (:generated-at manifest)
      (when-let [[_ stamp] (re-find #"^(\d{8}T\d{6}\.\d{3}Z)-" (str (:run-id manifest)))]
        (try
          (str (java.time.Instant/from
                (.parse (java.time.format.DateTimeFormatter/ofPattern
                         "yyyyMMdd'T'HHmmss.SSSX") stamp)))
          (catch Exception _ stamp)))))

(defn- safe-run-id? [s]
  (boolean (and (string? s) (re-matches #"[A-Za-z0-9._-]+" s))))

(defn- resolve-run [{:keys [root latest-run-fn verify-run-fn] :as _cfg} requested]
  (let [root (or root default-root)
        latest-run-fn (or latest-run-fn giraffe/newest-field-run)
        verify-run-fn (or verify-run-fn giraffe/verify-run)
        run-id (if (or (nil? requested) (= "latest" requested))
                 (:run-id (latest-run-fn root)) requested)
        dir (when (safe-run-id? run-id) (io/file root run-id))]
    (cond
      (or (nil? run-id) (nil? dir) (not (.isDirectory dir)))
      {:error {:status 404 :reason "run-not-found" :run requested}}

      :else
      ;; newest-field-run returns only a run that verifies; re-check an
      ;; explicitly named run only.
      (let [verification (if (or (nil? requested) (= "latest" requested))
                           {:ok? true}
                           (verify-run-fn dir))]
        (if (:ok? verification)
          {:run-id run-id :run-dir (.getAbsolutePath dir)
           :manifest (edn/read-string (slurp (io/file dir "manifest.edn")))}
          {:error {:status 409 :reason "run-unverified" :run run-id
                   :verification verification}})))))

(defn- certificate [cfg run-id turn-id]
  (let [root (or (:certificate-root cfg) default-certificate-root)
        file (io/file root (str turn-id "-" run-id ".lean"))
        sidecar (io/file (str (.getAbsolutePath file) ".check.json"))]
    (if (.isFile file)
      {:path (.getAbsolutePath file)
       :check (when (.isFile sidecar) (json/parse-string (slurp sidecar)))}
      {:path nil :check nil})))

(defn- attach-certificates [cfg run-id result]
  (letfn [(attach [relation]
            (if-let [turn-id (get-in relation [:turn :id])]
              (assoc relation :certificate (certificate cfg run-id turn-id))
              relation))]
    (cond-> result
      (:items result)
      (update :items
              #(mapv (fn [item]
                       (update item :focus-relations
                               (fn [rows] (mapv attach (or rows []))))) %))
      (:focus-relations result)
      (update :focus-relations #(mapv attach %)))))

(def ^:private cache-limit 64)

(defonce ^:private !qa-cache
  ;; A verified run never changes, so its QA answer is cached by run id
  ;; (top-items ~7 s live, 2026-11-17). Certificates are attached after the
  ;; cache, so a newly checked certificate shows at once. run=latest still
  ;; resolves on every call, so a new run is picked up.
  (atom {}))

(defn- cached-qa [k compute]
  (or (get @!qa-cache k)
      (let [v (compute)]
        (when (= :ok (:status v))
          (swap! !qa-cache #(assoc (if (>= (count %) cache-limit) {} %) k v)))
        v)))

(defn- error-response [status reason & [extra]]
  {:status status :body (json-ready (merge {:status :error :reason reason} extra))})

(defn top-items-response [cfg params]
  (try
    (let [resolved (resolve-run cfg (get params "run"))]
      (if-let [error (:error resolved)]
        (error-response (:status error) (:reason error) (dissoc error :status :reason))
        (let [{:keys [run-id run-dir manifest]} resolved
              n (try (Long/parseLong (str (or (get params "n") 10)))
                     (catch Exception _ 10))
              top-fn (or (:qa-top-fn cfg) qa/top-items)
              result (cached-qa [:top-items top-fn run-id n]
                                #(top-fn (merge (:qa-opts cfg) {:run-dir run-dir :n n})))]
          {:status 200
           :body (json-ready
                  (assoc (attach-certificates cfg run-id result)
                         :run run-id :generated_at (run-generated-at manifest)))})))
    (catch Throwable t
      (error-response 503 "qa-unavailable"
                      {:message (.getMessage t)
                       :failure (some-> t ex-data :reason)}))))

(defn why-not-response [cfg params]
  (if (str/blank? (get params "item"))
    (error-response 400 "item-required")
    (try
      (let [resolved (resolve-run cfg (get params "run"))]
        (if-let [error (:error resolved)]
          (error-response (:status error) (:reason error) (dissoc error :status :reason))
          (let [{:keys [run-id run-dir manifest]} resolved
                why-fn (or (:qa-why-fn cfg) qa/why-not)
                item (get params "item")
                result (cached-qa [:why-not why-fn run-id item]
                                  #(why-fn (merge (:qa-opts cfg)
                                                  {:run-dir run-dir :item item})))]
            {:status 200
             :body (json-ready
                    (assoc (attach-certificates cfg run-id result)
                           :run run-id :generated_at (run-generated-at manifest)))})))
      (catch Throwable t
        (error-response 503 "qa-unavailable"
                        {:message (.getMessage t)
                         :failure (some-> t ex-data :reason)})))))

;; ------------------------------------------------------- growing turn strand

(defn strand-cache [] {:entries (atom {}) :dirty (atom {})})

(defonce ^:private default-strand-cache (strand-cache))

(defn mark-strand-dirty!
  "Invalidate the cached projection for a turn after any turn-service event."
  ([turn-id] (mark-strand-dirty! default-strand-cache turn-id))
  ([cache turn-id]
   (when turn-id (swap! (:dirty cache) update (str turn-id) (fnil inc 0)))
   nil))

;; Reload-safe: re-evaluating this namespace replaces the keyed listener.
(turn-service/add-turn-listener!
 ::strand-cache
 (fn [{:keys [id]}] (mark-strand-dirty! id)))

(defn- gcc-counts [gcc]
  (into {}
        (map (fn [[animal reading]]
               [animal (cond-> {:status (:status reading)}
                         (= :present (:status reading))
                         (assoc :count (count (:clusion-ids reading)))
                         (= :absent (:status reading))
                         (assoc :reason (:reason reading)))])
             gcc)))

(def ^:private ma-parts [:I1 :I2 :G :B])

(defn- mapped-part [part]
  (when (and (map? part) (some? (:element part)) (some? (:turn part)))
    part))

(defn- ma-thread [row]
  (let [parts (into {}
                    (keep (fn [part]
                            ;; Prefer the chain reading when both windows map
                            ;; the same role: that is the longer-reach strand.
                            (when-let [[window reading]
                                       (or (when-let [v (mapped-part (get-in row [:chain part]))]
                                             [:chain v])
                                           (when-let [v (mapped-part (get-in row [:turn part]))]
                                             [:turn v]))]
                              [part {:window window :from (:turn reading)}])))
                    ma-parts)]
    (when (and (contains? #{:partial :occurrence "partial" "occurrence"}
                          (:status row))
               (seq parts))
      {:pattern (:pattern row) :status (:status row) :parts parts})))

(defn- ma-projection [verdict]
  (let [rows (vec (:patterns verdict))]
    {:status (or (:state verdict) :read)
     :window (vec (:window verdict))
     :threads (into [] (keep ma-thread) rows)
     :void-count (count (filter #(contains? #{:void "void"} (:status %)) rows))}))

(defn- ma-quote-edges [turn-id verdict]
  (->> (:patterns verdict)
       (mapcat (fn [row]
                 (for [window [:turn :chain]
                       part ma-parts
                       :let [reading (mapped-part (get-in row [window part]))
                             from (:turn reading)]
                       :when (and reading (not= (str turn-id) (str from)))]
                   {:from turn-id :to from :kind :quotes
                    :pattern (:pattern row) :part part})))
       distinct
       vec))

(defn- open-acts [rows]
  (mapv #(select-keys % [:act :author :kind]) (or rows [])))

(defn- turn-edges [{:keys [turn gcc item-links pattern-verdicts]}]
  (let [id (:id turn)
        prev (:prev_operator_turn_id turn)]
    (vec
     (concat
      (when prev [{:from id :to prev :kind :strand}])
      (map (fn [{:keys [item-id kind]}]
             {:from id :to item-id :kind kind}) item-links)
      (map (fn [pattern-id] {:from id :to pattern-id :kind :proposed})
           (get-in gcc [:驴 :clusion-ids]))
      (map (fn [{:keys [pattern status]}]
             {:from id :to pattern :kind status})
           (:threads (ma-projection pattern-verdicts)))
      (ma-quote-edges id pattern-verdicts)))))

(defn- compute-turn [cfg turn-id]
  (let [gcc-fn (or (:turn-gcc-fn cfg) chain/turn-gcc)
        light (gcc-fn (merge (:gcc-opts cfg)
                             {:turn-store (:turn-store cfg) :turn-id turn-id}))]
    {:id turn-id
     :evidence_ref (get-in light [:turn :evidence-ref])
     :created_at (get-in light [:turn :created_at])
     :prev_operator_turn_id (get-in light [:turn :prev_operator_turn_id])
     :next_operator_turn_id (get-in light [:turn :next_operator_turn_id])
     :ma_connection (:ma-connection light)
     :ma (ma-projection (:pattern-verdicts light))
     :open_acts (open-acts (:still-open light))
     :gcc (gcc-counts (:gcc light))
     :items (:items light)
     :item_absence (:item-absence light)
     :edges (turn-edges light)}))

(defn- cached-turn [cfg turn-id force?]
  (let [cache (or (:strand-cache cfg) default-strand-cache)
        dirty-version (get @(:dirty cache) turn-id)
        dirty? (some? dirty-version)]
    (if (and (not force?) (not dirty?) (contains? @(:entries cache) turn-id))
      (get @(:entries cache) turn-id)
      (let [result (compute-turn cfg turn-id)]
        (swap! (:entries cache) assoc turn-id result)
        ;; Do not consume an event that arrived while the projection ran.
        (swap! (:dirty cache)
               (fn [dirty]
                 (if (= dirty-version (get dirty turn-id))
                   (dissoc dirty turn-id)
                   dirty)))
        result))))

(defn- parse-n [s]
  (try (-> (Long/parseLong (str (or s 20))) (max 1) (min 200))
       (catch Exception _ 20)))

(defn strand-response
  "Return a newest-first, incrementally cached Circus graph for one session.
  Traversal reads the durable session head and stored predecessor pointers;
  it never lists the turn store. SINCE is exclusive and the returned cursor
  is the head observed by this request."
  [cfg params]
  (let [session (get params "session")]
    (cond
      (str/blank? session)
      (error-response 400 "session-required")

      :else
      (let [store (or (:turn-store cfg) (turns/store))
            head-fn (or (:session-head-fn cfg) turns/session-head)
            head (head-fn store session)]
        (if-not head
          (error-response 404 "session-not-found" {:session session})
          (try
            (let [n (parse-n (get params "n"))
                  since (get params "since")
                  rows (loop [id head i 0 out [] seen #{}]
                         (if (or (nil? id) (= id since) (>= i n) (seen id))
                           out
                           (let [row (cached-turn (assoc cfg :turn-store store)
                                                  id (< i 2))]
                             (recur (:prev_operator_turn_id row) (inc i)
                                    (conj out row) (conj seen id)))))
                  edges (vec (mapcat :edges rows))
                  turns (mapv #(dissoc % :edges) rows)]
              {:status 200
               :body (json-ready {:status :ok :session session :head head
                                  :cursor head :turns turns :edges edges})})
            (catch clojure.lang.ExceptionInfo e
              (error-response 503 "strand-unavailable"
                              {:failure (or (:reason (ex-data e)) :read-failed)
                               :message (.getMessage e)}))
            (catch Throwable t
              (error-response 503 "strand-unavailable" {:message (.getMessage t)}))))))))

;; ------------------------------------------------------- parallel session herd

(defn- instant [s] (java.time.Instant/parse (str s)))

(defn- in-window? [at since until]
  (and at (.isAfter (instant at) since) (not (.isAfter (instant at) until))))

(defn- known-session [s]
  (when (and (some? s) (not (str/blank? (str s))) (not= "pending" s)) s))

(defn- herd-lane [cfg store {:keys [session head]} since until agents]
  (loop [id head seen #{} rows [] agent nil]
    (if (or (nil? id) (seen id))
      (when (seq rows)
        (let [ordered (vec (sort-by (comp instant :created_at) rows))]
          {:session session :agent agent :first (:created_at (first ordered))
           :last (:created_at (peek ordered)) :turns ordered}))
      (if-let [record (turns/read-record store id)]
        (let [at (:created_at record)
              owner (or agent (:agent_id record))]
          (if (or (and at (not (.isAfter (instant at) since)))
                  (and (seq agents) (not (agents owner))))
            (recur nil seen rows owner)
            (let [projection (when (in-window? at since until)
                               ;; The strand's exact cache and dirty-event versions;
                               ;; a herd poll does not force a second projection.
                               (cached-turn (assoc cfg :turn-store store) id false))
                  row (when projection
                        {:id id :created_at at :ma_status (get-in projection [:ma :status])
                         :threads_n (count (get-in projection [:ma :threads]))
                         :quotes_n (count (filter #(= :quotes (:kind %)) (:edges projection)))})]
              (recur (:prev_operator_turn_id record) (conj seen id)
                     (cond-> rows row (conj row)) owner))))
        (recur nil seen rows agent)))))

(defn- herd-bells [store since until agents]
  (->> (turns/bell-indexes store)
       (mapcat :turns)
       (filter #(in-window? (:created_at %) since until))
       (keep (fn [{:keys [id]}]
               (when-let [r (turns/read-record store id)]
                 (when (and (= "agent" (:origin r))
                            (in-window? (:created_at r) since until)
                            (or (empty? agents) (agents (:caller r)) (agents (:agent_id r))))
                   (cond-> {:at (:created_at r) :from_agent (:caller r)
                            :to_agent (:agent_id r) :turn_id id
                            :kind (or (:surface r) "agent")}
                     (known-session (:caller_session r)) (assoc :from_session (:caller_session r))
                     (known-session (:session_id r)) (assoc :to_session (:session_id r)))))))
       (sort-by (juxt (comp instant :at) :turn_id)) vec))

(defn herd-response
  "Parallel operator strands plus indexed agent-origin interactions. Window is
  (since, until], defaulting to the last six hours. Cursor is latest served time;
  an empty result retains since. Pending bell sessions stay agent-addressed."
  [cfg params]
  (try
    (let [until (instant (or (get params "until")
                             (str ((or (:now-fn cfg) #(java.time.Instant/now))))))
          since (if-let [s (get params "since")] (instant s) (.minusSeconds until (* 6 3600)))
          agents (set (remove str/blank? (map str/trim (str/split (or (get params "agents") "") #","))))]
      (if (.isAfter since until)
        (error-response 400 "invalid-window")
        (let [store (or (:turn-store cfg) (turns/store))
              lanes (->> (turns/active-session-heads store (.toEpochMilli since))
                         (keep #(herd-lane cfg store % since until agents))
                         (sort-by (juxt :agent :session)) vec)
              bells (herd-bells store since until agents)
              cursor (reduce (fn [latest at] (let [t (instant at)] (if (.isAfter t latest) t latest)))
                             since (concat (map :last lanes) (map :at bells)))]
          {:status 200 :body (json-ready {:lanes lanes :bells bells :cursor (str cursor)})})))
    (catch java.time.format.DateTimeParseException _ (error-response 400 "invalid-time"))
    (catch Exception e (error-response 503 "herd-unavailable" {:message (.getMessage e)}))))
