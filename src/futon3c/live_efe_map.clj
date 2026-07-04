(ns futon3c.live-efe-map
  "Read-only live EFE-map projection for M-live-efe-map VERIFY.

   This namespace joins already-existing live surfaces: agent registry,
   invoke jobs, durable clock-lineage, wm-tick evidence, and the static
   June-12 EFE coordinate set. It never writes substrate state and never
   feeds agent selection."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.agency.clock-lineage :as clock-lineage]
            [futon3c.agency.clock-store :as clock-store]
            [futon3c.evidence.store :as estore]))

(def ^:private code-root
  (or (System/getenv "FUTON_CODE_ROOT") "/home/joe/code"))

(def ^:private coord-path
  "/home/joe/code/futon6/data/mission-carpet-pos-embed.json")

(def ^:private bge-path
  "/home/joe/code/futon3a/resources/notions/bge_mission_embeddings.json")

(def ^:private coordinate-variant "embed")

(def ^:private placement-weights
  {:scope-edge 3.0
   :explicit-ref 2.0
   :clock-or-coordination 2.0
   :bge-knn 1.0
   :doc-mention 1.0
   :tau 0.45
   :active-terms [:explicit-ref :bge-knn]
   :provenance "arbitrary-starting-values; tune under VERIFY. v1 computes only :active-terms; scope-edge/clock-or-coordination/doc-mention are declared for the D2 formula but not yet wired."})

(defn- now-ms [] (System/currentTimeMillis))

(defn- read-json
  [path]
  (try
    (when (.exists (io/file path))
      (json/parse-string (slurp path) false))
    (catch Exception _ nil)))

;; The coordinate set and BGE embeddings are multi-MB JSON files; parsing them
;; per request made the endpoint ~2s. Cache on file mtime so a re-embed is
;; picked up on the next request without a reload.
(def ^:private file-cache (atom {}))

(defn- read-json-cached
  [path]
  (let [f (io/file path)
        mtime (.lastModified f)
        cached (get @file-cache path)]
    (if (and cached (= (:mtime cached) mtime))
      (:value cached)
      (let [v (read-json path)]
        (swap! file-cache assoc path {:mtime mtime :value v})
        v))))

(defn- coordinate-map
  []
  (or (read-json-cached coord-path) {}))

(defn- bge-records
  []
  (let [raw (read-json-cached bge-path)]
    (cond
      (vector? raw) raw
      (map? raw) (or (get raw "records") [])
      :else [])))

(defn- mission-docs
  []
  (let [root (io/file code-root)]
    (if-not (.exists root)
      []
      (->> (.listFiles root)
           (filter #(.isDirectory ^java.io.File %))
           (filter #(str/starts-with? (.getName ^java.io.File %) "futon"))
           (mapcat file-seq)
           (filter #(.isFile ^java.io.File %))
           (filter #(re-matches #"M-[^/]+\.md" (.getName ^java.io.File %)))
           (filter #(str/includes? (.getPath ^java.io.File %) "/holes/"))
           (remove #(str/includes? (.getPath ^java.io.File %) "/.git/"))
           vec))))

;; The doc index walks every futon* repo; cache it for 60s — mission docs
;; appear at human cadence, the map polls at machine cadence.
(def ^:private doc-index-cache (atom nil))

(defn- mission-doc-index
  []
  (let [{:keys [at value]} @doc-index-cache
        now (now-ms)]
    (if (and value (< (- now (long at)) 60000))
      value
      (let [v (into {}
                    (map (fn [^java.io.File f]
                           [(str/replace (.getName f) #"\.md$" "") f]))
                    (mission-docs))]
        (reset! doc-index-cache {:at now :value v})
        v))))

(defn- mission-refs
  [text]
  (->> (re-seq #"M-[A-Za-z0-9][A-Za-z0-9-]*" (or text ""))
       distinct
       vec))

(defn- explicit-ref-anchors
  [mission-id positions doc-index]
  (when-let [^java.io.File f (get doc-index mission-id)]
    (let [text (slurp f)]
      (->> (mission-refs text)
           (remove #{mission-id})
           (filter #(contains? positions %))
           distinct
           (mapv (fn [anchor]
                   {:mission-id anchor
                    :weight (:explicit-ref placement-weights)
                    :reason :explicit-ref
                    :position (get positions anchor)}))))))

(defn- cosine
  [a b]
  (let [dot (reduce + (map * a b))
        na (Math/sqrt (double (reduce + (map #(* % %) a))))
        nb (Math/sqrt (double (reduce + (map #(* % %) b))))]
    (if (or (zero? na) (zero? nb)) 0.0 (/ dot (* na nb)))))

(defn- native-bge-anchors
  [mission-id positions]
  (let [records (bge-records)
        by-id (into {} (keep (fn [r]
                               (when-let [b (get r "basename")]
                                 [b r])))
                    records)]
    (when-let [target (get-in by-id [mission-id "vector"])]
      (->> records
           (keep (fn [r]
                   (let [anchor (get r "basename")]
                     (when (and anchor
                                (not= anchor mission-id)
                                (contains? positions anchor)
                                (vector? (get r "vector")))
                       (let [c (cosine target (get r "vector"))
                             w (max 0.0 (- c (:tau placement-weights)))]
                         (when (pos? w)
                           {:mission-id anchor
                           :weight w
                            :reason :native-bge-knn
                            :cosine c
                            :position (get positions anchor)}))))))
           (sort-by (comp - :weight))
           (take 6)
           vec))))

(defn- frontier-shelf-position
  [mission-id]
  (let [slot (mod (Math/abs (hash mission-id)) 20)
        row (mod (quot (Math/abs (hash mission-id)) 20) 3)]
    [(double (+ 120 (* slot 170)))
     (double (+ 3430 (* row 48)))]))

(defn- weighted-centroid
  [anchors]
  (let [total (double (reduce + (map :weight anchors)))]
    (when (pos? total)
      [(double (/ (reduce + (map (fn [{:keys [weight position]}]
                                   (* weight (first position)))
                                 anchors))
                  total))
       (double (/ (reduce + (map (fn [{:keys [weight position]}]
                                   (* weight (second position)))
                                 anchors))
                  total))])))

(defn- placement
  [mission-id positions doc-index]
  (cond
    (str/blank? (str mission-id))
    nil

    (contains? positions mission-id)
    (let [[x y] (get positions mission-id)]
      {:mission-id mission-id
       :x x
       :y y
       :placement :embedded
       :method :embedded
       :anchors []
       :anchor-depth 0
       :confidence 1.0
       :as-of (now-ms)
       :source-coordinate-set coord-path})

    :else
    (let [ref-anchors (explicit-ref-anchors mission-id positions doc-index)
          bge-anchors (when (empty? ref-anchors)
                        (native-bge-anchors mission-id positions))
          anchors (vec (or (seq ref-anchors) bge-anchors []))]
      (if-let [[x y] (weighted-centroid anchors)]
        {:mission-id mission-id
         :x x
         :y y
         :placement :approximate
         :method (if (seq ref-anchors) :relational-centroid :native-bge-knn)
         :anchors (mapv #(dissoc % :position) anchors)
         :anchor-depth 1
         :confidence (min 0.85 (+ 0.35 (* 0.08 (count anchors))))
         :as-of (now-ms)
         :source-coordinate-set coord-path
         :weights placement-weights}
        (let [[x y] (frontier-shelf-position mission-id)]
          {:mission-id mission-id
           :x x
           :y y
           :placement :frontier-shelf
           :method :unanchored-frontier
           :anchors []
           :anchor-depth nil
           :confidence 0.0
           :as-of (now-ms)
           :source-coordinate-set coord-path
           :weights placement-weights})))))

(defn- durable-clock-by-agent
  []
  (let [rows (get (clock-lineage/reconstitute {:limit 200}) :missions [])]
    (reduce (fn [acc row]
              (reduce (fn [m agent]
                        (update m agent
                                (fn [old]
                                  (if (or (nil? old)
                                          (> (long (:last-clock-ms row 0))
                                             (long (:last-clock-ms old 0))))
                                    row
                                    old))))
                      acc
                      (:agents row)))
            {}
            rows)))

(defn- job-by-agent
  [jobs]
  (reduce (fn [acc job]
            (assoc acc (:agent-id job) job))
          {}
          jobs))

(defn- agent-row
  [positions doc-index jobs-by-agent durable-by-agent [agent-id info]]
  (let [session-id (:session-id info)
        durable (get durable-by-agent agent-id)
        live-mission (:mission-id (:clock (clock-store/current-state agent-id session-id)))
        mission-id (or live-mission (:target durable))
        place (placement mission-id positions doc-index)
        job (get jobs-by-agent agent-id)]
    {:agent-id agent-id
     :type (:type info)
     :status (:status info)
     :session-id session-id
     :mission-id mission-id
     :clock-source (cond
                     live-mission :live-clock-store
                     durable :durable-clock-lineage
                     :else nil)
     :last-active (:last-active info)
     :invoke-activity (:invoke-activity info)
     :job-state (:state job)
     :running-job-id (when (#{"running" "queued"} (str (:state job)))
                       (:job-id job))
     :placement place}))

(defn- wm-ticks
  [evidence-store limit]
  (try
    (->> (estore/query* evidence-store
                        {:query/author "war-machine"
                         :query/tags [:wm-tick]
                         :query/limit limit
                         :query/include-ephemeral? true})
         (take limit)
         vec)
    (catch Exception _ [])))

(defn- wm-row
  [positions doc-index entry]
  (let [body (:evidence/body entry)
        enacted (:enacted body)
        target (:target body)]
    {:evidence-id (:evidence/id entry)
     :at (:evidence/at entry)
     :body-at (:at body)
     :decision (:decision body)
     :mode (:mode body)
     :trigger (:trigger body)
     :G (:G body)
     :expected-G (:expected-G body)
     :realized-G (:realized-G body)
     :gates (:gates body)
     :candidates (:candidates body)
     :enacted {:mission-id enacted
               :placement (placement enacted positions doc-index)}
     :target {:mission-id target
              :placement (placement target positions doc-index)}}))

(defn- ship-position
  "Joe's rocket (operator decision 2026-07-04, resolving T3/C2): not a cursor
   stream — the unweighted centroid of ACTIVE sessions' map positions. Active =
   status invoking/idle (alive this server epoch, not merely restored) with an
   :embedded or :approximate placement; frontier-shelf coordinates are synthetic
   and excluded. No live contributors -> nil (the rocket is honestly absent)."
  [agents]
  (let [contributing (->> agents
                          (filter #(contains? #{"invoking" "idle" :invoking :idle}
                                              (:status %)))
                          (filter #(contains? #{:embedded :approximate}
                                              (get-in % [:placement :placement]))))]
    (when (seq contributing)
      (let [n (count contributing)
            x (/ (reduce + (map #(double (get-in % [:placement :x])) contributing)) n)
            y (/ (reduce + (map #(double (get-in % [:placement :y])) contributing)) n)]
        {:x x
         :y y
         :method :active-session-centroid
         :contributing (mapv (fn [a] {:agent-id (:agent-id a)
                                      :mission-id (:mission-id a)})
                             contributing)
         :session-count n
         :as-of (now-ms)}))))

(defn build-response
  [{:keys [registry invoke-jobs evidence-store wm-limit]
    :or {wm-limit 25}}]
  (let [positions (coordinate-map)
        doc-index (mission-doc-index)
        jobs (vec (or invoke-jobs []))
        jobs-by-agent (job-by-agent jobs)
        durable-by-agent (durable-clock-by-agent)
        agents-map (:agents registry)
        agents (->> agents-map
                    (mapv (partial agent-row positions doc-index jobs-by-agent durable-by-agent))
                    (sort-by (juxt #(if (:mission-id %) 0 1) :agent-id))
                    vec)
        wm (mapv (partial wm-row positions doc-index)
                 (wm-ticks evidence-store wm-limit))
        frontier (->> (concat (keep :placement agents)
                              (mapcat (fn [row]
                                        [(get-in row [:enacted :placement])
                                         (get-in row [:target :placement])])
                                      wm))
                      (remove nil?)
                      (remove #(= :embedded (:placement %)))
                      (group-by :mission-id)
                      vals
                      (mapv first))]
    {:ok true
     :read-only? true
     :generated-at-ms (now-ms)
     :coordinate-set {:source coord-path
                      :variant coordinate-variant
                      :mission-count (count positions)}
     :placement-weights placement-weights
     :agents {:count (count agents)
              :with-placement (count (filter :placement agents))
              :items agents}
     :war-machine {:count (count wm)
                   :items wm}
     :ship (ship-position agents)
     :frontier {:count (count frontier)
                :items frontier}}))
