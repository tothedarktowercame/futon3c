#!/usr/bin/env bb
;; Batch promotion: scribe passes 2 (Schwarz) + 3 (lemniscate) — 12 drafts,
;; plus TWO new terrain patterns (the fix for consecutive recall
;; misses: complex-analysis and topology had no patterns, so no lexical
;; bridge could carry packets to memories). Shapes mirror
;; promote_scribe_pass_1.bb + wire_math_memory_patterns.clj
;; ensure-pattern!/description-entry (mint receipt + description
;; evidence, deterministic ids). RAM caution: health probe, 150ms
;; pacing, idempotent, read-back verify, stop on 503.

(require '[babashka.http-client :as http]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def root "/home/joe/code/futon3c/holes/labs/M-codex-sorry-loop")
(def base "http://127.0.0.1:7073")
(def commit? (= "--commit" (first *command-line-args*)))
(def pace-ms 150)
(def mission-id "M-codex-sorry-loop")
(def reviewer "claude-6")
(def recall-system :v1.2-normalized)
(def warrant "operator-delegated: in-use evaluation (proof-in-pudding; owner draft review claude-6 2026-07-28; Joe's standing direction)")

(def new-patterns
  {"math/holomorphic-disk-api"
   "Holomorphic maps of the unit disk: Schwarz lemma and its equality case, dslope and affine_of_mapsTo_ball_of_norm_dslope_eq_div, unit derivative, rotation exp(I*alpha), norm_mul_exp_arg_mul_I, disk automorphisms, maximum modulus. Trigger vocabulary: holomorphic, disk, ball, Schwarz, rotation, derivative, norm, complex, analysis, dslope, affine, equality."
   "math/connectedness-component-api"
   "Connected components and connectedness in topology: connectedComponentIn and its maximality, frontier of components, open components of open sets, preconnected sets, superlevel and sublevel sets, unbounded components meeting ball exteriors, complement connectivity, polynomial lemniscates. Trigger vocabulary: connected, component, frontier, preconnected, sublevel, superlevel, complement, topology, interior, open, lemniscate."})

(def plan
  ;; [draft-file name pattern kind distill-id session-id]
  [["scribe-pass-2-drafts.edn" "specialize-affine-dslope-equality-at-the-disk-center" "math/holomorphic-disk-api" :feedback "e-codexroll-019fa2c1-t003"]
   ["scribe-pass-2-drafts.edn" "convert-unit-complex-coefficient-to-an-angle-rotation" "math/holomorphic-disk-api" :feedback "e-codexroll-019fa2c1-t003"]
   ["scribe-pass-2-drafts.edn" "check-general-dslope-equality-before-building-schwarz-machinery" "math/proof-architecture" :feedback "e-codexroll-019fa2c1-t003"]
   ["scribe-pass-2-drafts.edn" "reverse-polar-identity-when-the-coefficient-goal-points-outward" "math-formalization/tactic-algebra-interference" :feedback "e-codexroll-019fa2c1-t003"]
   ["scribe-pass-2-drafts.edn" "record-unrelated-recall-as-honest-non-use-and-a-terrain-gap" "math/missing-dependency-protocol" :feedback "e-codexroll-019fa2c1-t003"]
   ["scribe-pass-3-drafts.edn" "frontier-of-open-connected-component-lies-in-ambient-frontier" "math/connectedness-component-api" :feedback "e-codexroll-019f9b12-t011"]
   ["scribe-pass-3-drafts.edn" "zulip-general-topology-complement-connected-component-anchor" "math/connectedness-component-api" :reference "e-codexroll-019f9b12-t011"]
   ["scribe-pass-3-drafts.edn" "open-set-notation-scope-before-relative-intersection" "math-formalization/tactic-algebra-interference" :feedback "e-codexroll-019f9b12-t011"]
   ["scribe-pass-3-drafts.edn" "turn-component-membership-into-interior-membership-via-openness" "math/connectedness-component-api" :feedback "e-codexroll-019f9b12-t011"]
   ["scribe-pass-3-drafts.edn" "lemniscate-superlevel-preconnected" "math/missing-dependency-protocol" :reference "e-codexroll-019f9b12-t011"]
   ["scribe-pass-3-drafts.edn" "lemniscate-sublevel-components-inject-into-roots" "math/missing-dependency-protocol" :reference "e-codexroll-019f9b12-t011"]
   ["scribe-pass-3-drafts.edn" "record-consecutive-recall-empty-terrain-gaps-as-curriculum-signal" "math/missing-dependency-protocol" :feedback "e-codexroll-019f9b12-t011"]])

(defn witness-for [lane]
  (if (#{:solve-lane :arc-lane} lane) :independently-witnessed :self-asserted))

(defn hook-for [d]
  (let [b (:body d)]
    (or (:rule b) (:problem-class b) (:statement b) (:trigger b)
        (get-in b [:scope :context]) (:name d))))

(defn exists? [path]
  (= 200 (:status (http/get (str base path)
                            {:headers {"accept" "application/edn"}
                             :throw false :timeout 15000}))))

(defn get-edn [path]
  ;; missing-entity lookups take ~17s server-side before the 404 —
  ;; wire_math_memory_patterns.clj uses 60s for the same reason
  (let [r (http/get (str base path) {:headers {"accept" "application/edn"}
                                     :throw false :timeout 60000})]
    (when (= 200 (:status r))
      (edn/read-string {:default (fn [_ v] v)} (:body r)))))

(defn post! [path payload]
  (let [r (http/post (str base path)
                     {:headers {"content-type" "application/edn"
                                "accept" "application/edn"
                                "x-penholder" "api"}
                      :body (pr-str payload)
                      :throw false :timeout 30000})]
    (when (= 503 (:status r))
      (throw (ex-info "store busy — stopping" {:path path})))
    ;; 409 = deterministic id already present: idempotent success, not
    ;; failure (the whole point of client-supplied ids).
    (when-not (or (<= 200 (:status r) 299) (= 409 (:status r)))
      (throw (ex-info "write failed" {:path path :status (:status r)})))
    (Thread/sleep pace-ms)
    (if (= 409 (:status r)) :already-exists true)))

(defn ensure-pattern! [pattern-id description]
  (let [entity (get-edn (str "/api/alpha/entity/"
                             (java.net.URLEncoder/encode pattern-id "UTF-8")))]
    (if (= description (get-in entity [:props :description]))
      :existing
      (do (when-not commit? (throw (ex-info "dry-run reached pattern write" {})))
          (post! "/api/alpha/entity"
                 {:id pattern-id :name pattern-id :type :pattern/library
                  :external-id pattern-id
                  :source "M-codex-sorry-loop terrain vocabulary (claude-6)"
                  :props (merge (:props entity)
                                {:domain :mathematics
                                 :pattern/kind :trigger-class
                                 :description description
                                 :recall-system recall-system})})
          (post! "/api/alpha/evidence"
                 {:evidence/id (str "e-pattern-mint-" (str/replace pattern-id "/" "-"))
                  :evidence/subject {:ref/type :pattern :ref/id pattern-id}
                  :evidence/type :reflection :evidence/claim-type :observation
                  :evidence/author reviewer
                  :evidence/body {:event :pattern-mint :pattern-id pattern-id
                                  :kind :trigger-class
                                  :warrant "terrain-gap fix (two consecutive recall misses); vocabulary by claude-6"
                                  :description description}
                  :evidence/tags [:memory :pattern-mint]})
          (post! "/api/alpha/evidence"
                 {:evidence/id (str "e-pattern-description-v1-" (subs pattern-id (count "math/")))
                  :evidence/subject {:ref/type :pattern :ref/id pattern-id}
                  :evidence/type :reflection :evidence/claim-type :observation
                  :evidence/author reviewer
                  :evidence/session-id "M-codex-sorry-loop/recall-vocabulary"
                  :evidence/body {:event :pattern-description :pattern-id pattern-id
                                  :description description :recall-system recall-system}
                  :evidence/tags [:memory :pattern-description :mathematics]})
          :minted))))

(defn promote! [draft pattern kind distill session-id]
  (let [nm (:name draft)
        ids {:entry (str "e-codexpilot-" nm)
             :hx (str "hx-codexpilot-" nm)
             :review (str "e-review-codexpilot-" nm)}
        b (:body draft)
        subject-ids (mapv (comp str :ref/id) (:subjects draft))
        endpoints (->> (concat [(:entry ids)] subject-ids
                               [pattern distill session-id mission-id])
                       (remove str/blank?) distinct vec)]
    (post! "/api/alpha/evidence"
           {:evidence/id (:entry ids) :evidence/type :memory
            :evidence/claim-type :assert :evidence/author reviewer
            :evidence/session-id session-id
            :evidence/subject (first (:subjects draft))
            :evidence/tags [:memory :mathematics :codex-pilot (:lane draft)]
            :evidence/body {:name nm :hook (hook-for draft) :kind kind :body b
                            :why (or (:problem-class b) (:trigger b) (get-in b [:scope :context]))
                            :how-to-apply (or (:rule b)
                                              (some->> (:proof-shape b) (str/join " → "))
                                              (some->> (:observed-trajectory b) (str/join " → ")))
                            :scribe-author "codex-5"
                            :draft-source "holes/labs/M-codex-sorry-loop/"
                            :runner-model :codex
                            :promotion {:reviewed-by reviewer :verdict :approve :warrant warrant}}})
    (post! "/api/alpha/hyperedge"
           {:hx/id (:hx ids) :hx/type :memory/assert
            :hx/endpoints endpoints
            :hx/props {:roles {:entry (:entry ids)
                               :subjects (vec (distinct (conj subject-ids pattern)))
                               :distills [distill]
                               :session session-id :mission mission-id
                               :patterns [pattern]}
                       :kind kind :name nm :hook (hook-for draft)
                       :volatile? (= :frontier (:level draft))
                       :state :current :domain :mathematics
                       :witness-status (witness-for (:lane draft))
                       :attachment-status :proposed}})
    (post! "/api/alpha/evidence"
           {:evidence/id (:review ids)
            :evidence/subject {:ref/type :memory :ref/id (:entry ids)}
            :evidence/type :memory :evidence/claim-type :observation
            :evidence/author reviewer
            :evidence/session-id "M-codex-sorry-loop/operator-review"
            :evidence/body {:review/event :memory-attachment-review
                            :review/memory-id (:entry ids)
                            :review/pattern-ids [pattern]
                            :review/verdict :approve
                            :review/witness-status (witness-for (:lane draft))
                            :review/provenance {:kind :operator-delegated-in-use-evaluation
                                                :reviewer reviewer :date "2026-07-28"
                                                :warrant warrant}
                            :review/policy-verdict :pudding-delegated}
            :evidence/tags [:memory :memory/review :mathematics :codex-pilot]})
    (if (and (exists? (str "/api/alpha/evidence/" (:entry ids)))
             (exists? (str "/api/alpha/evidence/" (:review ids))))
      :promoted :VERIFY-FAILED)))

(let [drafts-by-file (into {} (for [f (distinct (map first plan))]
                                [f (into {} (map (juxt :name identity))
                                         (edn/read-string (slurp (str root "/" f))))]))
      _ (when-not (exists? "/api/alpha/evidence?limit=1")
          (throw (ex-info "store health probe failed" {})))
      pattern-results
      (if commit?
        (into {} (for [[pid desc] new-patterns] [pid (ensure-pattern! pid desc)]))
        (into {} (for [[pid _] new-patterns]
                   [pid (if (get-edn (str "/api/alpha/entity/"
                                          (java.net.URLEncoder/encode pid "UTF-8")))
                          :exists :would-mint)])))
      results
      (vec (for [[file nm pattern kind distill] plan
                 :let [draft (get-in drafts-by-file [file nm])]]
             (cond
               (nil? draft) {:name nm :status :DRAFT-NOT-FOUND}
               (exists? (str "/api/alpha/evidence/e-codexpilot-" nm))
               {:name nm :status :skipped-existing}
               (not commit?) {:name nm :status :dry-run :pattern pattern :kind kind}
               :else {:name nm :pattern pattern
                      :status (promote! draft pattern kind distill
                                        "M-codex-sorry-loop/cron-lane")})))]
  (spit (str root "/promotion-pass-2-3-report.edn")
        (pr-str {:at "2026-07-28" :mode (if commit? :commit :dry-run)
                 :patterns pattern-results :results results}))
  (println "patterns:" pattern-results)
  (doseq [r results] (println (:status r) "|" (:name r) "|" (:pattern r "")))
  (println "TOTAL:" (frequencies (map :status results))))
