#!/usr/bin/env bb
;; First memory-topology perturbation experiment.
;;
;; The live store is used only by `--capture`, which writes a frozen fixture
;; once. The sweep itself replays two exact, evidence-backed lexical queries
;; entirely offline. It measures D_state (candidate-set/rank divergence), not
;; D_functional and not Salingaros T/H.

(ns damage-state-sweep
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn]
            [clojure.set :as set])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def root "/home/joe/code/futon3c/holes/labs/M-memory-retrieval")
(def fixture-path (str root "/damage-state-fixture-20260730.edn"))
(def result-path (str root "/damage-state-results-20260730.edn"))
(def base-url "http://127.0.0.1:7073")
(def search-limit 30)
(def proposal-row-limit 10)
(def projection-limit 100)
(def result-limit 5)

(def cases
  [{:case-id :run25-diagnosed-content-match
    :query "roots outside"
    :query-provenance
    {:job-id "invoke-1785439906978-381-36c58afb"
     :offered-evidence-id "e-11daf851-6ec7-4382-8839-7721d8b252f3"
     :outcome-evidence-id "7ed99fe8-c65b-4239-9c18-3c9f11ac82bd"
     :basis "outcome ladder replay names this exact query as retrieving the missed memory"}
    :descriptive-anchors
    {:historically-missed
     ["e-codexpilot-close-a92J05-by-transferring-the-unit-disk-zero-count"]}}
   {:case-id :lemniscate-known-good
    :query "card route connectedcomponents"
    :query-provenance
    {:job-id "invoke-1785440558225-384-2820fe28"
     :offered-evidence-id "e-a2cb197e-b14a-41c9-89b6-4b6ac16a9b3a"
     :outcome-evidence-id "7fd3850e-959a-4c6c-85d9-f40aec83963d"
     :basis "first strict ladder tier from the exact offered terms"}
    :descriptive-anchors
    {:historically-used
     ["e-codexpilot-count-polynomial-lemniscate-components-by-roots-plus-one-exterior"
      "e-codexpilot-frontier-of-open-connected-component-lies-in-ambient-frontier"
      "e-codexpilot-turn-component-membership-into-interior-membership-via-openness"
      "e-codexpilot-zulip-general-topology-complement-connected-component-anchor"]
     :architecture-memory
     "e-codexpilot-count-polynomial-lemniscate-components-by-roots-plus-one-exterior"}}])

(defn parse-body [body]
  (cond
    (map? body) body
    (string? body)
    (edn/read-string {:default (fn [_tag value] value)} body)
    :else body))

(defn request! [method path opts]
  (let [response
        ((case method :get http/get :post http/post)
         (str base-url path)
         (merge {:headers {"Accept" "application/edn"}
                 :timeout 60000
                 :throw false}
                opts))
        status (:status response)
        body (parse-body (:body response))]
    (when-not (<= 200 (long status) 299)
      (throw (ex-info "fixture capture request failed"
                      {:method method :path path :status status :body body})))
    body))

(defn encode [value]
  (URLEncoder/encode (str value) "UTF-8"))

(defn search! [query]
  (request! :get
            (str "/api/alpha/evidence/text-search?q=" (encode query)
                 "&limit=" search-limit)
            {}))

(defn project! [endpoints]
  (if (seq endpoints)
    (request! :post
              "/api/alpha/memory/projection"
              {:headers {"Accept" "application/edn"
                         "Content-Type" "application/edn"}
               :body (pr-str {:endpoints (vec endpoints)
                              :limit projection-limit})})
    {:ok true :groups []}))

(defn proposal-row? [row]
  (let [entry (:entry row)]
    (or (= :memory (:evidence/type entry))
        (and (= :reflection (:evidence/type entry))
             (= :pattern-description
                (get-in entry [:evidence/body :event]))
             (string? (get-in entry
                              [:evidence/body :pattern-id]))))))

(defn proposal-rows [search-result]
  (->> (:results search-result)
       (filter proposal-row?)
       (take proposal-row-limit)
       vec))

(defn entry-id [row]
  (get-in row [:entry :evidence/id]))

(defn group-map [projection]
  (into {} (map (juxt :endpoint identity)) (:groups projection)))

(defn patterns-in-group [group]
  (->> (:components group)
       (mapcat #(get-in % [:edge :hx/props :roles :patterns]))
       (filter string?)
       distinct
       sort
       vec))

(defn capture-case! [{:keys [query] :as case}]
  (let [search-result (search! query)
        rows (proposal-rows search-result)
        memory-ids
        (->> rows
             (filter #(= :memory
                         (get-in % [:entry :evidence/type])))
             (keep entry-id)
             distinct
             vec)
        description-pattern-ids
        (->> rows
             (keep #(get-in % [:entry :evidence/body :pattern-id]))
             distinct
             vec)
        validation (project! (concat memory-ids description-pattern-ids))
        validation-groups (group-map validation)
        attached-pattern-ids
        (->> memory-ids
             (mapcat #(patterns-in-group
                       (get validation-groups %)))
             distinct
             sort
             vec)
        pattern-projection (project! attached-pattern-ids)]
    (assoc case
           :search-result search-result
           :proposal-rows rows
           :validation-projection validation
           :pattern-projection pattern-projection
           :capture-audit
           {:search-row-count (count (:results search-result))
            :proposal-row-count (count rows)
            :memory-row-count (count memory-ids)
            :description-pattern-count
            (count description-pattern-ids)
            :attached-pattern-count (count attached-pattern-ids)})))

(defn write-once! [path value]
  (let [file (java.io.File. path)]
    (if (.exists file)
      (let [existing (edn/read-string (slurp file))]
        (when-not (= existing value)
          (throw (ex-info "refusing to overwrite frozen artifact"
                          {:path path})))
        :existing)
      (do
        (spit file (str (pr-str value) "\n"))
        :written))))

(defn capture-fixture! []
  (when (.exists (java.io.File. fixture-path))
    (throw (ex-info "fixture already exists; capture is write-once"
                    {:path fixture-path})))
  (let [fixture
        {:fixture/version 1
         :captured-at (str (Instant/now))
         :source {:kind :futon1b
                  :base-url base-url
                  :store-read-only? true}
         :scope
         {:operator
          :lexical-proposal-plus-reviewed-pattern-projection
          :excludes
          [:dispatch-packet-term-extraction
           :problem-and-subject-endpoint-arms
           :receipt-ranking
           :body-hydration
           :live-timeout-path]
          :search-limit search-limit
          :proposal-row-limit proposal-row-limit
          :projection-limit projection-limit
          :result-limit result-limit}
         :cases (mapv capture-case! cases)}]
    (write-once! fixture-path fixture)
    fixture))

(defn current-reviewed-mathematics? [component]
  (let [props (get-in component [:edge :hx/props])]
    (and (= :current (:state props))
         (= :reviewed (:attachment-status props))
         (= :mathematics (:domain props)))))

(defn component-memory-id [component]
  (get-in component [:edge :hx/props :roles :entry]))

(defn component-patterns [component]
  (vec (filter string?
               (get-in component [:edge :hx/props :roles :patterns]))))

(defn component-edge-id [component]
  (or (get-in component [:edge :hx/id])
      (:hyperedge-id component)))

(defn perturb-component
  [component {:keys [kind edge-id memory-id pattern-id]}]
  (case kind
    :none component
    :remove-edge
    (when-not (= edge-id (component-edge-id component)) component)
    :remove-pattern-role
    (if (and (= memory-id (component-memory-id component))
             (some #{pattern-id} (component-patterns component)))
      (update-in component [:edge :hx/props :roles :patterns]
                 (fn [patterns]
                   (vec (remove #{pattern-id} patterns))))
      component)
    component))

(defn perturb-group [group perturbation]
  (update group :components
          (fn [components]
            (->> components
                 (keep #(perturb-component % perturbation))
                 vec))))

(defn perturbed-group-map [projection perturbation]
  (into {}
        (map
         (fn [group]
           [(:endpoint group)
            (perturb-group group perturbation)]))
        (:groups projection)))

(defn reviewed-memories [group limit]
  (->> (:components group)
       (filter current-reviewed-mathematics?)
       (keep
        (fn [component]
          (let [memory-id (component-memory-id component)]
            (when (string? memory-id)
              {:memory/id memory-id
               :memory/hook
               (get-in component [:edge :hx/props :hook])
               :memory/pattern-ids (component-patterns component)
               :edge/id (component-edge-id component)}))))
       (reduce
        (fn [{:keys [seen items] :as acc} memory]
          (if (contains? seen (:memory/id memory))
            acc
            {:seen (conj seen (:memory/id memory))
             :items (conj items memory)}))
        {:seen #{} :items []})
       :items
       (sort-by :memory/id)
       (take limit)
       vec))

(defn row-score [row]
  (double (or (:score row) Double/POSITIVE_INFINITY)))

(defn memory-proposals [rows validation-groups]
  (reduce
   (fn [acc row]
     (let [memory-id (entry-id row)
           memories
           (reviewed-memories
            (get validation-groups memory-id)
            result-limit)]
       (reduce
        (fn [inner memory]
          (reduce
           (fn [m pattern-id]
             (update m pattern-id
                     (fn [prior]
                       (-> (or prior
                               {:pattern-id pattern-id
                                :memory-support []})
                           (update :memory-support conj
                                   {:memory-id memory-id
                                    :fts-score (row-score row)})))))
           inner
           (:memory/pattern-ids memory)))
        acc
        memories)))
   {}
   (filter #(= :memory (get-in % [:entry :evidence/type])) rows)))

(defn description-proposals [rows validation-groups]
  (reduce
   (fn [acc row]
     (let [pattern-id
           (get-in row [:entry :evidence/body :pattern-id])
           memories
           (reviewed-memories
            (get validation-groups pattern-id)
            result-limit)]
       (if (seq memories)
         (assoc acc pattern-id
                {:pattern-id pattern-id
                 :memory-support
                 [{:description-evidence-id (entry-id row)
                   :memory-ids (mapv :memory/id memories)
                   :fts-score (row-score row)}]})
         acc)))
   {}
   (filter
    #(= :pattern-description
        (get-in % [:entry :evidence/body :event]))
    rows)))

(defn merge-proposal [left right]
  (update left :memory-support into (:memory-support right)))

(defn proposal-rank [candidate]
  (let [scores (keep :fts-score (:memory-support candidate))]
    (if (seq scores)
      (apply min scores)
      Double/POSITIVE_INFINITY)))

(defn content-matches [rows validation-groups]
  (->> rows
       (filter #(= :memory (get-in % [:entry :evidence/type])))
       (keep
        (fn [row]
          (let [memory-id (entry-id row)
                memory
                (some #(when (= memory-id (:memory/id %)) %)
                      (reviewed-memories
                       (get validation-groups memory-id)
                       result-limit))]
            (when memory
              (assoc memory
                     :via :content-match
                     :content-match/score (row-score row))))))
       (reduce
        (fn [{:keys [seen items] :as acc} memory]
          (if (contains? seen (:memory/id memory))
            acc
            {:seen (conj seen (:memory/id memory))
             :items (conj items memory)}))
        {:seen #{} :items []})
       :items
       vec))

(defn replay
  [case perturbation {:keys [content-arm? pattern-arm?]
                      :or {content-arm? true pattern-arm? true}}]
  (let [rows (:proposal-rows case)
        validation-groups
        (perturbed-group-map
         (:validation-projection case)
         perturbation)
        pattern-groups
        (perturbed-group-map
         (:pattern-projection case)
         perturbation)
        proposals
        (merge-with
         merge-proposal
         (memory-proposals rows validation-groups)
         (description-proposals rows validation-groups))
        pattern-ids
        (if pattern-arm?
          (->> (vals proposals)
               (sort-by (juxt proposal-rank :pattern-id))
               (map :pattern-id)
               (take result-limit)
               vec)
          [])
        content
        (if content-arm?
          (content-matches rows validation-groups)
          [])
        pattern-memories
        (if pattern-arm?
          (mapcat
           (fn [pattern-id]
             (map #(assoc % :via :pattern
                          :dispatch/endpoint pattern-id)
                  (reviewed-memories
                   (get pattern-groups pattern-id)
                   result-limit)))
           pattern-ids)
          [])
        candidates
        (->> (concat content pattern-memories)
             (reduce
              (fn [{:keys [seen items] :as acc} memory]
                (if (contains? seen (:memory/id memory))
                  acc
                  {:seen (conj seen (:memory/id memory))
                   :items (conj items memory)}))
              {:seen #{} :items []})
             :items
             (take result-limit)
             vec)]
    {:pattern-ids pattern-ids
     :candidate-ids (mapv :memory/id candidates)
     :candidate-via
     (into (sorted-map)
           (map (juxt :memory/id :via))
           candidates)}))

(defn reciprocal-rank-map [ids]
  (into {}
        (map-indexed
         (fn [index id] [id (/ 1.0 (inc index))])
         ids)))

(defn divergence [baseline perturbed]
  (let [before (:candidate-ids baseline)
        after (:candidate-ids perturbed)
        before-set (set before)
        after-set (set after)
        union (set/union before-set after-set)
        intersection (set/intersection before-set after-set)
        before-rank (reciprocal-rank-map before)
        after-rank (reciprocal-rank-map after)
        rr-damage
        (reduce
         +
         0.0
         (map #(Math/abs
                (- (double (get before-rank % 0.0))
                   (double (get after-rank % 0.0))))
              union))]
    {:changed? (not= before after)
     :lost (vec (filter (complement after-set) before))
     :gained (vec (filter (complement before-set) after))
     :set-symmetric-difference
     (count (set/union
             (set/difference before-set after-set)
             (set/difference after-set before-set)))
     :jaccard-distance
     (if (seq union)
       (- 1.0 (/ (double (count intersection))
                 (count union)))
       0.0)
     :reciprocal-rank-damage rr-damage}))

(defn all-components [case]
  (->> [(:validation-projection case)
        (:pattern-projection case)]
       (mapcat :groups)
       (mapcat :components)
       (filter current-reviewed-mathematics?)
       vec))

(defn edge-perturbations [case]
  (->> (all-components case)
       (keep
        (fn [component]
          (let [edge-id (component-edge-id component)
                memory-id (component-memory-id component)]
            (when (and (string? edge-id) (string? memory-id))
              {:kind :remove-edge
               :edge-id edge-id
               :memory-id memory-id}))))
       distinct
       (sort-by (juxt :edge-id :memory-id))
       vec))

(defn pattern-role-perturbations [case]
  (->> (all-components case)
       (mapcat
        (fn [component]
          (let [memory-id (component-memory-id component)]
            (for [pattern-id (component-patterns component)]
              {:kind :remove-pattern-role
               :memory-id memory-id
               :pattern-id pattern-id}))))
       distinct
       (sort-by (juxt :pattern-id :memory-id))
       vec))

(defn anchor-presence [case replay-result]
  (let [candidate-set (set (:candidate-ids replay-result))]
    (into
     (sorted-map)
     (for [[label ids]
           (:descriptive-anchors case)
           :when (sequential? ids)]
       [label
        (into (sorted-map)
              (map (fn [id] [id (contains? candidate-set id)]))
              ids)]))))

(defn run-perturbations [case baseline perturbations]
  (mapv
   (fn [perturbation]
     (let [after (replay case perturbation {})]
       {:perturbation perturbation
        :candidate-ids (:candidate-ids after)
        :divergence (divergence baseline after)}))
   perturbations))

(defn changed-summary [rows]
  (let [changed (filter #(get-in % [:divergence :changed?]) rows)]
    {:perturbation-count (count rows)
     :changed-count (count changed)
     :changed-fraction
     (if (seq rows)
       (/ (double (count changed)) (count rows))
       0.0)
     :maximum-jaccard-distance
     (reduce max 0.0
             (map #(get-in % [:divergence :jaccard-distance])
                  rows))
     :maximum-reciprocal-rank-damage
     (reduce max 0.0
             (map #(get-in % [:divergence
                              :reciprocal-rank-damage])
                  rows))}))

(defn run-case [case]
  (let [baseline (replay case {:kind :none} {})
        no-content (replay case {:kind :none}
                           {:content-arm? false})
        no-pattern (replay case {:kind :none}
                           {:pattern-arm? false})
        edges
        (run-perturbations
         case baseline (edge-perturbations case))
        pattern-roles
        (run-perturbations
         case baseline (pattern-role-perturbations case))]
    {:case-id (:case-id case)
     :query (:query case)
     :query-provenance (:query-provenance case)
     :descriptive-anchors (:descriptive-anchors case)
     :baseline (assoc baseline
                      :anchor-presence
                      (anchor-presence case baseline))
     :arm-ablations
     {:without-content
      {:candidate-ids (:candidate-ids no-content)
       :divergence (divergence baseline no-content)}
      :without-pattern
      {:candidate-ids (:candidate-ids no-pattern)
       :divergence (divergence baseline no-pattern)}}
     :edge-removals
     {:summary (changed-summary edges)
      :rows edges}
     :pattern-role-removals
     {:summary (changed-summary pattern-roles)
      :rows pattern-roles}}))

(defn validate-result! [result]
  (let [by-id (into {} (map (juxt :case-id identity)) (:cases result))
        run25 (get by-id :run25-diagnosed-content-match)
        lemniscate (get by-id :lemniscate-known-good)
        run25-anchor
        (get-in run25
                [:baseline :anchor-presence :historically-missed
                 "e-codexpilot-close-a92J05-by-transferring-the-unit-disk-zero-count"])
        architecture
        (get-in lemniscate
                [:descriptive-anchors :architecture-memory])
        architecture-present?
        (contains? (set (get-in lemniscate
                                [:baseline :candidate-ids]))
                   architecture)]
    (assert (= 2 (count (:cases result)))
            "the frozen experiment must contain exactly two cases")
    (assert run25-anchor
            "the fixed operator must retain the diagnosed run-25 content match")
    (assert architecture-present?
            "known-good baseline must contain its architecture memory")
    (assert (every?
             #(= :offline-frozen-snapshot
                 (:execution-mode %))
             (:cases result))
            "every result case must declare offline execution")
    true))

(defn run-sweep [fixture]
  (let [result
        {:experiment/version 1
         :measured-from
         {:fixture-file (.getName (java.io.File. fixture-path))
          :captured-at (:captured-at fixture)}
         :claim-boundary
         {:measures :D-state
          :does-not-measure
          [:D-functional
           :memory-usefulness
           :outcome-lift
           :Salingaros-T
           :Salingaros-H
           :liveness]}
         :cases
         (mapv #(assoc (run-case %)
                       :execution-mode :offline-frozen-snapshot)
               (:cases fixture))}]
    (validate-result! result)
    result))

(defn print-summary [fixture-write result-write result]
  (println "damage-state sweep complete")
  (println "fixture:" fixture-write fixture-path)
  (println "result:" result-write result-path)
  (println "claim boundary:" (pr-str (:claim-boundary result)))
  (doseq [case (:cases result)]
    (println
     (name (:case-id case))
     "baseline" (pr-str (get-in case [:baseline :candidate-ids]))
     "edge-removals"
     (pr-str (get-in case [:edge-removals :summary]))
     "pattern-role-removals"
     (pr-str (get-in case [:pattern-role-removals :summary])))))

(let [capture? (some #{"--capture"} *command-line-args*)
      fixture
      (if capture?
        (capture-fixture!)
        (if (.exists (java.io.File. fixture-path))
          (edn/read-string (slurp fixture-path))
          (throw (ex-info "frozen fixture missing; run once with --capture"
                          {:path fixture-path}))))
      result (run-sweep fixture)
      result-write (write-once! result-path result)]
  (print-summary (if capture? :written :existing)
                 result-write
                 result))
