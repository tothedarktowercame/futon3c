(ns futon3c.agency.dispatch-graph
  "P3-2 read-only mesh dispatch queries. Cause is interval containment in a
   bounded job snapshot, never a stored causal edge or an authority grant.
   `edges` accepts {:evidence [...] :jobs [...] :as-of instant}; job context is
   retained as vector metadata. Stripping metadata loses cause information and
   yields :unattributed, never a guessed parent. Invoke-result is not a dispatch."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.data.json :as json])
  (:import [java.time Instant Duration]
           [java.net URI URLEncoder]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]))

(defn- getv [m k] (if (contains? m k) (get m k) (get m (subs (str k) 1))))
(defn- text [s] (if (and (string? s) (not (str/blank? s))) (str/trim s) "unknown"))
(defn- instant [s] (try (Instant/parse s) (catch Exception _ nil)))
(defn- fail! [reason data] (throw (ex-info "Invalid dispatch snapshot" (assoc data :reason reason))))
(defn- body [r]
  (let [b (getv r :evidence/body)] (if (string? b) (edn/read-string b) b)))
(defn- kind [k] (if (keyword? k) (name k) k))

(defn edges
  "Normalize only mesh :invoke records. Unknown caller rows survive. :ok? is
   nil unless the invoke itself or a unique matching result records a boolean.
   A sequence of evidence is also accepted, without job context."
  [records]
  (let [context (if (map? records) records {:evidence records})
        rows (filter #(some #{"mesh-edge"} (map kind (getv % :evidence/tags))) (:evidence context))
        _ (when-not (= (count rows) (count (set (map #(getv % :evidence/id) rows))))
            (fail! :duplicate-evidence {}))
        result-rows (filter #(= "invoke-result" (kind (getv (body %) :edge/kind))) rows)
        normalized
        (mapv (fn [r]
                (let [b (body r) id (getv b :edge/id) at (getv b :edge/at)
                      from (text (getv b :edge/from)) to (text (getv b :edge/to))
                      results (filter #(let [rb (body %)]
                                         (and (= id (getv rb :edge/id))
                                              (= from (text (getv rb :edge/from)))
                                              (= to (text (getv rb :edge/to))))) result-rows)
                      oks (distinct (keep #(getv (body %) :edge/ok?) results))
                      recorded (getv b :edge/ok?)]
                  (when-not (and (string? id) (not (str/blank? id)) (instant at)
                                 (string? (getv r :evidence/id)))
                    (fail! :malformed-edge {:source (getv r :evidence/id)}))
                  {:from from :to to :at at :job-id id
                   :surface (text (getv b :edge/surface))
                   :ok? (if (boolean? recorded) recorded
                            (when (and (= 1 (count oks)) (boolean? (first oks))) (first oks)))
                   :source (getv r :evidence/id)}))
              (filter #(= "invoke" (kind (getv (body %) :edge/kind))) rows))]
    (with-meta (vec (sort-by (juxt #(instant (:at %)) :source) normalized))
      (select-keys context [:jobs :as-of :coverage]))))

(defn- running-at? [job agent at as-of]
  (let [start (instant (getv job :started-at)) end (instant (getv job :finished-at))
        now (instant as-of) t (instant at) state (kind (getv job :state))]
    (and (= agent (text (getv job :agent-id))) start t
         (not (.isBefore ^Instant t start))
         (if end (not (.isAfter ^Instant t end))
             (and (= "running" state) now (not (.isAfter ^Instant t now)))))))

(defn upstream
  "Walk child dispatch -> unique running caller job -> its dispatch. Intervals
   are closed [started-at,finished-at]; ongoing jobs are bounded by capture as-of.
   Missing terminal time on a terminal job does not invent an open interval."
  [es job-id]
  (let [{:keys [jobs as-of coverage]} (meta es)
        base {:basis :bounded-reconstruction :as-of as-of :coverage coverage}]
    (loop [id job-id seen #{} path []]
      (cond
        (contains? seen id) (assoc base :path path :status :cycle :job-id id)
        (= 256 (count path)) (assoc base :path path :status :bounded :reason :depth-limit)
        :else
        (let [matches (filter #(= id (:job-id %)) es)]
          (cond
            (empty? matches) (assoc base :path path :status :unattributed :reason :missing-dispatch :job-id id)
            (> (count matches) 1) (assoc base :path path :status :ambiguous :reason :multiple-dispatch-records :job-id id)
            :else
            (let [edge (first matches) next-path (conj path edge)
                  candidates (vec (filter #(running-at? % (:from edge) (:at edge) as-of) jobs))]
              (cond
                (= "unknown" (:from edge)) (assoc base :path next-path :status :unattributed :reason :unknown-caller)
                (empty? candidates) (assoc base :path next-path :status :unattributed :reason :no-running-caller-job)
                (> (count candidates) 1) (assoc base :path next-path :status :ambiguous
                                                :reason :overlapping-caller-jobs
                                                :candidates (mapv #(getv % :job-id) candidates))
                :else (let [cause (getv (first candidates) :job-id)]
                        (if-not (and (string? cause) (not (str/blank? cause)))
                          (assoc base :path next-path :status :unattributed :reason :missing-cause-id)
                          (recur cause (conj seen id)
                                 (conj path (assoc edge :cause-job-id cause)))))))))))))

(defn- in-window [es field agent from to]
  (let [lo (instant from) hi (instant to)]
    (when-not (and lo hi (.isBefore ^Instant lo hi)) (fail! :invalid-window {}))
    (filterv #(let [t (instant (:at %))]
                (and (= agent (get % field)) t (not (.isBefore ^Instant t lo))
                     (.isBefore ^Instant t hi))) es)))
(defn dispatched-by "Dispatches sent by agent in [from,to)." [es agent from to]
  (in-window es :from agent from to))
(defn dispatched-to "Dispatches received by agent in [from,to)." [es agent from to]
  (in-window es :to agent from to))

(defn- query-string [m]
  (str/join "&" (map (fn [[k v]] (str (name k) "=" (URLEncoder/encode (str v) "UTF-8"))) m)))

(defn- get! [base path]
  (let [request (-> (HttpRequest/newBuilder (URI/create (str base path)))
                    (.timeout (Duration/ofSeconds 60))
                    (.header "Accept" "application/json") .GET .build)
        response (.send (HttpClient/newHttpClient) request (HttpResponse$BodyHandlers/ofString))]
    (when-not (= 200 (.statusCode response))
      (fail! :read-refused {:status (.statusCode response) :path path}))
    (json/read-str (.body response) :key-fn keyword)))

(defn read-records!
  "Read one pinned evidence window sequentially via LIST (never temporal BY-ID).
   Job endpoint is a bounded current snapshot, not a historical/system-time API.
   No writes, JVM reloads, dispatches, or registration."
  [evidence-base agency-base from to]
  (when-not (and (instant from) (instant to)) (fail! :invalid-window {}))
  (let [pin (str (Instant/now))
        params {:tags "coordination,mesh-edge" :since from :before to
                :system-as-of pin :limit 1000}
        evidence (loop [q params seen #{} rows []]
                   (let [page (get! evidence-base (str "/api/alpha/evidence?" (query-string q)))
                         entries (:entries page) cursor (:next-cursor page)]
                     (when-not (vector? entries) (fail! :invalid-evidence-page {}))
                     (when (or (:incomplete page) (and cursor (contains? seen cursor)))
                       (fail! :incomplete-evidence {:cursor cursor}))
                     (if cursor
                       (recur (assoc q :cursor-at (:at cursor) :cursor-id (:id cursor))
                              (conj seen cursor) (into rows entries))
                       (into rows entries))))
        page (get! agency-base "/api/alpha/invoke/jobs?limit=1000")]
    (when-not (and (:ok page) (vector? (:jobs page))) (fail! :invalid-job-page {}))
    {:evidence evidence :jobs (:jobs page) :as-of pin
     :coverage {:evidence-window [from to] :evidence-system-as-of pin
                :jobs-limit 1000 :jobs-returned (count (:jobs page))
                :jobs-truncated? (= 1000 (count (:jobs page)))}}))
