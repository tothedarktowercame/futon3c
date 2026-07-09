(ns futon3c.logic.capability-star-map-extractor
  "Unit A extractor for M-capability-star-map's WM-region slice.

  The first slice is intentionally bounded: it starts from the hand-authored
  ensemble-1 capability set, enriches it from the live mission registry and the
  pudding-prover registry, emits the bipartite mission/capability graph EDN, and
  adapts that real graph back into the existing invariant queries."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]
            [cheshire.core :as json]
            [futon2.aif.mission-registry :as mission-registry]
            [futon3c.logic.capability-star-map-invariants :as inv])
  (:import (java.net URI URLEncoder)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)
           (java.nio.charset StandardCharsets)
           (java.time Duration)))

(def default-ensemble-path
  "/home/joe/code/futon0/holes/missions/M-capability-star-map.ensemble.edn")

(def default-output-path
  "/home/joe/code/futon0/holes/missions/M-capability-star-map.graph.edn")

(def default-pudding-registry-path
  "/home/joe/code/futon7/holes/pudding-prover-registry.edn")

(def ^:private wm-region-capability-order
  [:agency
   :evidence-persistence
   :self-representing-stack
   :live-geometric-stack
   :war-machine
   :wm-steps-forward-guardrailed
   :efe-trustworthy-over-starmap
   :wm-overnight-unsupervised])

(def ^:private non-mission-builders
  {"guardrails core (Unit 1)"
   {:id "builder/wm-guardrails-core"
    :builder "guardrails core"
    :built-under "WM-GUARDRAILS-SPEC"}
   "input-sources hygiene (Unit 2)"
   {:id "builder/wm-input-sources-hygiene"
    :builder "input-sources hygiene"
    :built-under "WM-GUARDRAILS-SPEC"}
   "hole-counter (Unit 3)"
   {:id "builder/wm-hole-counter"
    :builder "hole-counter"
    :built-under "WM-GUARDRAILS-SPEC"}
   "gate-runner (Unit 4)"
   {:id "builder/wm-gate-runner"
    :builder "gate-runner"
    :built-under "WM-GUARDRAILS-SPEC"}})

(def ^:private ego-proxy-base "http://localhost:3100")

(def ^:private structural-cache-ttl-ms 30000)

(def ^:private structural-query-timeout-ms 5000)

(def ^:private eightfold-phases
  ["head" "identify" "map" "derive" "argue" "verify" "instantiate" "document"])

(defonce ^:private structural-cache (atom {}))

(defn- now-ms [] (System/currentTimeMillis))

(defn- urlencode [s]
  (URLEncoder/encode (str s) StandardCharsets/UTF_8))

(defn- ego-url
  [base mission-id]
  (str (or base ego-proxy-base)
       "/api/futon/ego/"
       (urlencode mission-id)
       "?fold=1&depth=3"))

(defn- fetch-ego-json
  "Bounded :3100 ego-proxy read. Returns nil on any failure; the WM scan must
   degrade, never crash, when substrate scope data is unavailable."
  [mission-id {:keys [ego-base timeout-ms client]
               :or {ego-base ego-proxy-base
                    timeout-ms structural-query-timeout-ms}}]
  (try
    (let [client (or client (HttpClient/newHttpClient))
          req (-> (HttpRequest/newBuilder (URI/create (ego-url ego-base mission-id)))
                  (.timeout (Duration/ofMillis (long timeout-ms)))
                  (.GET)
                  (.build))
          resp (.send client req (HttpResponse$BodyHandlers/ofString))]
      (when (<= 200 (.statusCode resp) 299)
        (json/parse-string (.body resp) true)))
    (catch Throwable _
      nil)))

(defn- scope-frames-from-ego
  [ego-json]
  (->> (get-in ego-json [:ego :outgoing])
       (keep :entity)
       (filter #(= "scope/frame" (:type %)))
       vec))

(defn- cached-scope-frames
  [mission-id opts]
  (let [cache-key [(or (:ego-base opts) ego-proxy-base) mission-id]
        now (now-ms)
        cached (get @structural-cache cache-key)]
    (if (and cached (< (- now (long (:at cached))) structural-cache-ttl-ms))
      (:frames cached)
      (let [frames (some-> (fetch-ego-json mission-id opts) scope-frames-from-ego)]
        (swap! structural-cache assoc cache-key {:at now :frames frames})
        frames))))

(defn- frame-props [frame] (or (:props frame) {}))

(defn- frame-binder [frame]
  (let [props (frame-props frame)]
    (or (:scope/binder props)
        (:scope/binder-type props))))

(defn- frame-scope-id [frame]
  (or (get-in frame [:props :scope/id])
      (:id frame)))

(defn- scope-leaf [frame]
  (some-> (frame-scope-id frame)
          (str/split #"/")
          last
          (str/replace #"--[0-9a-f]{8}$" "")))

(defn- substantive-frame? [frame]
  (let [props (frame-props frame)]
    (or (pos? (long (or (:fold/sub-count props) 0)))
        (pos? (long (or (:fold/child-count props) 0)))
        (and (:anchor/state props)
             (not= :detached (:anchor/state props))))))

(defn structural-hole-report-from-frames
  "Pure structural counter. Missing eightfold phases are ghost holes; present
   phases with no substance are vacuous holes; an Open Questions loose-section
   with content is one held-question hole."
  [frames]
  (let [phase-frames (->> frames
                          (filter #(= "eightfold-phase" (frame-binder %)))
                          (group-by scope-leaf))
        phase-status (into {}
                           (for [phase eightfold-phases
                                 :let [fs (get phase-frames phase)]]
                             [phase (cond
                                      (empty? fs) :ghost
                                      (some substantive-frame? fs) :written
                                      :else :vacuous)]))
        loose-open (->> frames
                        (filter #(= "loose-section" (frame-binder %)))
                        (filter #(re-find #"(?i)^open-questions?$" (or (scope-leaf %) "")))
                        (filter substantive-frame?)
                        vec)
        ghost (->> phase-status (filter (comp #{:ghost} val)) (map key) vec)
        vacuous (->> phase-status (filter (comp #{:vacuous} val)) (map key) vec)
        written (->> phase-status (filter (comp #{:written} val)) (map key) vec)
        open-question-count (count loose-open)]
    {:structural-hole-count (+ (count ghost) (count vacuous) open-question-count)
     :phase/written written
     :phase/ghost ghost
     :phase/vacuous vacuous
     :loose/open-question-count open-question-count
     :loose/open-question-scopes (mapv frame-scope-id loose-open)}))

(defn structural-hole-report
  "Fetch and summarize substrate-2 mission structure via the :3100 ego proxy.
   Returns nil when the proxy is unavailable or the mission has no scope frames."
  ([mission-id] (structural-hole-report mission-id {}))
  ([mission-id opts]
   (when-let [frames (seq (cached-scope-frames mission-id opts))]
     (structural-hole-report-from-frames frames))))

(defn contaminated-path?
  "True for sources that must not enter the canonical landscape prior."
  [path]
  (let [s (str path)]
    (or (str/includes? s "/.state/")
        (str/includes? s "/worktrees/")
        (str/includes? s "/futon3-origin/")
        (str/includes? s "/futon3/origin/"))))

(defn clean-missions
  [missions]
  (->> missions
       (remove #(contaminated-path? (:path %)))
       vec))

(defn- strip-md [s]
  (str/replace (str s) #"\.md$" ""))

(defn- mission-id-from-minted-by [s]
  (some-> (re-find #"M-[A-Za-z0-9-]+" (str s)) strip-md))

(defn- status->mission-status [cap-status]
  (if (= :satisfied cap-status) :complete :held))

(defn read-edn-file [path]
  (with-open [r (io/reader path)]
    (edn/read (java.io.PushbackReader. r))))

(defn read-pudding-registry
  ([] (read-pudding-registry default-pudding-registry-path))
  ([path]
   (let [doc (read-edn-file path)]
     (into {} (map (juxt :id identity) (:sorries doc))))))

(defn- resolve-minted-by [mission-index raw]
  (if-let [builder (get non-mission-builders raw)]
    (assoc builder :real-mission? false :raw raw)
    (let [mission-id (mission-id-from-minted-by raw)]
      (cond
        (and mission-id (contains? mission-index mission-id))
        {:id mission-id :real-mission? true :raw raw}

        mission-id
        (throw (ex-info "minted-by names an M-* mission that is not in the registry"
                        {:raw raw :mission-id mission-id}))

        :else
        (throw (ex-info "minted-by entry is not resolvable as a real mission or known builder"
                        {:raw raw}))))))

(defn- capability-from-ensemble [pudding-by-id mission-index [cap-id cap]]
  (let [pudding (case cap-id
                  :wm-overnight-unsupervised (get pudding-by-id :T4.2)
                  nil)
        minted-by (mapv #(resolve-minted-by mission-index %) (:minted-by cap))]
    [cap-id
     (cond-> {:title (:title cap)
              :status (:status cap)
              :scope (vec (:scope cap))
              :minted-by (->> minted-by (map :id) distinct vec)
              :pre-registered? true}
       (:attested cap) (assoc :attested true)
       (:frontier cap) (assoc :frontier true)
       (:keystone cap) (assoc :keystone true)
       (:pre-witness cap) (assoc :pre-witness (:pre-witness cap))
       (:grounding cap) (assoc :grounding (:grounding cap))
       pudding (assoc :pudding-thesis (:id pudding)
                      :cap/altitude (:altitude pudding)))]))

(defn capability-nodes
  [ensemble pudding-by-id mission-index]
  (let [caps (:capabilities ensemble)]
    (into (sorted-map)
          (map #(capability-from-ensemble pudding-by-id mission-index [% (get caps %)]))
          wm-region-capability-order)))

(defn- mission-ref-details [mission-index mission-id]
  (if-let [registry (get mission-index mission-id)]
    {:id mission-id
     :real-mission? true
     :registry registry}
    (when-let [builder (some (fn [[_ spec]]
                               (when (= mission-id (:id spec)) spec))
                             non-mission-builders)]
      (assoc builder :real-mission? false))))

(defn- mission-node [mission-index cap-id cap mission-id structural-hole-fn]
  (let [{:keys [real-mission? registry] :as ref} (mission-ref-details mission-index mission-id)
        _ (when-not ref
            (throw (ex-info "resolved minted-by id has no registry mission or known builder"
                            {:mission-id mission-id :capability cap-id})))
        structural-report (when (and real-mission? structural-hole-fn)
                            (try
                              (structural-hole-fn mission-id)
                              (catch Throwable _ nil)))]
    [mission-id
     (cond-> {:scope (vec (:scope cap))
              :produces [cap-id]
              :real-mission? real-mission?
              :next-exit-operator-verify? (= cap-id :efe-trustworthy-over-starmap)}
       real-mission?
       (assoc :open-hole-count (long (or (:open-hole-count registry) 0))
              :phase (:status-class registry)
              :status (:status-class registry)
              :path (:path registry))

       (and real-mission?
            (some? (:structural-hole-count structural-report)))
       (assoc :structural-hole-count (long (:structural-hole-count structural-report)))

       (not real-mission?)
       (assoc :open-hole-count 0
              :phase :builder
              :status (status->mission-status (:status cap))
              :builder (:builder ref)
              :built-under (:built-under ref)))]))

(defn mission-nodes
  ([missions capabilities] (mission-nodes missions capabilities structural-hole-report))
  ([missions capabilities structural-hole-fn]
  (let [mission-index (into {} (map (juxt :id identity) (clean-missions missions)))]
    (into (sorted-map)
          (for [[cap-id cap] capabilities
                mission-id (:minted-by cap)]
            (mission-node mission-index cap-id cap mission-id structural-hole-fn))))))

(defn requires-edges [capabilities]
  (vec (for [[cap-id cap] capabilities
             prereq (:scope cap)]
         {:from cap-id :to prereq :type :requires})))

(defn produces-edges [capabilities]
  (vec (for [[cap-id cap] capabilities
             mission-id (:minted-by cap)]
         {:from mission-id :to cap-id :type :produces})))

(defn pudding-edges [pudding-by-id]
  (let [interesting #{:T4 :T4.2}]
    (vec
     (concat
      (for [[id rec] pudding-by-id
            :let [parent (:parent rec)]
            :when (and (interesting id) parent)]
        {:from id :to parent :type :specialises})
      (for [[id rec] pudding-by-id
            :let [coupled (:couples rec)]
            :when (and (interesting id) coupled)]
        {:from id :to coupled :type :couples})))))

(defn build-graph
  "Build the WM-region graph. Optional opts: :ensemble-path, :pudding-path,
  :missions. Supplying :missions keeps tests and retries deterministic."
  ([] (build-graph {}))
  ([{:keys [ensemble-path pudding-path missions structural-holes? structural-hole-fn]
     :or {ensemble-path default-ensemble-path
          pudding-path default-pudding-registry-path
          structural-holes? true}}]
   (let [ensemble (read-edn-file ensemble-path)
         pudding-by-id (read-pudding-registry pudding-path)
         missions (or missions (:missions (mission-registry/load-missions)))
         mission-index (into {} (map (juxt :id identity) (clean-missions missions)))
         capabilities (capability-nodes ensemble pudding-by-id mission-index)
         structural-hole-fn (when structural-holes?
                              (or structural-hole-fn structural-hole-report))
         mission-map (mission-nodes missions capabilities structural-hole-fn)]
     {:star-map/region :wm
      :star-map/source {:ensemble ensemble-path
                        :pudding-registry pudding-path
                        :mission-source :futon2.aif.mission-registry/load-missions
                        :exclusion [:worktrees :futon3-origin :.state]}
      :capabilities capabilities
      :missions mission-map
      :edges (vec (concat (requires-edges capabilities)
                          (produces-edges capabilities)
                          (pudding-edges pudding-by-id)))})))

(defn write-graph!
  ([] (write-graph! default-output-path))
  ([path]
   (let [graph (build-graph)]
     (spit path (with-out-str (pprint/pprint graph)))
     graph)))

(defn requires-toposort
  "Return capability ids in prerequisite-before-dependent order. Throws on cycle."
  [graph]
  (let [reqs (filter #(= :requires (:type %)) (:edges graph))
        nodes (set (keys (:capabilities graph)))
        deps (reduce (fn [m {:keys [from to]}]
                       (update m from (fnil conj #{}) to))
                     (zipmap nodes (repeat #{}))
                     reqs)]
    (loop [remaining deps
           ordered []]
      (if (empty? remaining)
        ordered
        (let [ready (->> remaining
                         (filter (fn [[_ ds]] (empty? (set/intersection ds (set (keys remaining))))))
                         (map key)
                         sort
                         vec)]
          (when (empty? ready)
            (throw (ex-info "cycle in :requires graph" {:remaining remaining})))
          (recur (apply dissoc remaining ready) (into ordered ready)))))))

(defn graph->trace
  "Adapt a real graph into the abstract invariant trace. The trace contains the
  actual :requires edges, provenance facts for satisfied capabilities, and only
  applicable advance steps for missions whose scope is already satisfied."
  [graph]
  (let [caps (:capabilities graph)
        satisfied? #(= :satisfied (get-in caps [% :status]))
        mission-complete? (fn [m] (= :complete (get-in graph [:missions m :status])))
        edge-steps (map-indexed (fn [i {:keys [from to]}]
                                  {:step (keyword (str "edge-" i))
                                   :edge-from from
                                   :edge-to to})
                                (filter #(= :requires (:type %)) (:edges graph)))
        provenance-steps (for [[cap-id cap] caps
                               :when (= :satisfied (:status cap))]
                           {:step (keyword (str "prov-" (name cap-id)))
                            :satisfied-cap cap-id
                            ;; Grounded ledger rows can certify shipped substrate
                            ;; even while the producing mission remains open.
                            :minted-by-complete? (boolean (or (:attested-substrate cap)
                                                              (:external cap)
                                                              (some mission-complete? (:minted-by cap))))})
        advance-steps (for [[mission-id mission] (:missions graph)
                            :let [scope (:scope mission)
                                  applicable? (every? satisfied? scope)]
                            :when applicable?]
                        {:step (keyword (str "advance-" mission-id))
                         :action :advance
                         :mission mission-id
                         :requires-sat? true
                         :crosses-exit? (:next-exit-operator-verify? mission)
                         :gap-agreed? true})]
    (vec (concat edge-steps provenance-steps advance-steps))))

(defn run-verify-equivalent
  ([] (run-verify-equivalent (build-graph)))
  ([graph]
   (let [order (requires-toposort graph)
         trace (graph->trace graph)
         violations (inv/query-violations trace)]
     {:verified? (not (inv/violations? violations))
      :toposort order
      :violations violations
      :trace-count (count trace)})))

(defn transitive-scope
  [graph cap-id]
  (let [caps (:capabilities graph)]
    (loop [frontier (seq (get-in caps [cap-id :scope]))
           seen #{}]
      (if-let [c (first frontier)]
        (if (seen c)
          (recur (next frontier) seen)
          (recur (concat (next frontier) (get-in caps [c :scope])) (conj seen c)))
        seen))))

(defn keystone-path-report
  ([] (keystone-path-report (build-graph)))
  ([graph]
   (let [scope (transitive-scope graph :wm-overnight-unsupervised)
         held (->> scope
                   (filter #(not= :satisfied (get-in graph [:capabilities % :status])))
                   sort
                   vec)]
     {:target :wm-overnight-unsupervised
      :transitive-scope (vec (sort scope))
      :held held
      :single-held-substantive-node? (= held [:efe-trustworthy-over-starmap])})))

(defn extract-write-and-verify!
  ([] (extract-write-and-verify! default-output-path))
  ([path]
   (let [graph (write-graph! path)]
     {:output path
      :graph-counts {:capabilities (count (:capabilities graph))
                     :missions (count (:missions graph))
                     :edges (count (:edges graph))}
      :verify (run-verify-equivalent graph)
      :keystone (keystone-path-report graph)})))
