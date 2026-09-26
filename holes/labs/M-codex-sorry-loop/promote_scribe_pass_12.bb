#!/usr/bin/env bb
;; Batch promotion: scribe pass 12 — the first per-run-cadence pass (Joe 2026-07-30: batching defeats compounding).
;; eLpNorm) — 6 drafts, NO new terrain patterns (all six attach to
;; existing ones; deliberately not minting, since neither pass exposed a
;; terrain gap). Shape mirrors promote_scribe_pass_8_9.bb exactly.
;; RAM caution: health probe, 150ms pacing, idempotent, read-back
;; verify, stop on 503. 60s GETs because missing-entity lookups take
;; ~17s server-side before the 404.
;;
;; SEAT NOTE: reviewer is claude-9 (succeeded claude-6/Fable 2026-07-29,
;; futon3c commit 006eaf50). Same role, same ledger; authorship is
;; recorded honestly so store joins split by model stratum.
;;
;; review-notes carries per-draft amendments from the owner review into
;; the store, so they do not live only in the (untracked) drafts file.

(require '[babashka.http-client :as http]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def root "/home/joe/code/futon3c/holes/labs/M-codex-sorry-loop")
(def base "http://127.0.0.1:7073")
(def commit? (= "--commit" (first *command-line-args*)))
(def pace-ms 400)   ;; raised 2026-07-30: a 30s POST timed out mid-pass-12; store writes are slower under load
(def mission-id "M-codex-sorry-loop")
(def reviewer "claude-9")
(def recall-system :v1.2-normalized)
(def warrant "operator-delegated: in-use evaluation (proof-in-pudding; owner draft review claude-9 2026-07-29/30; Joe's standing direction)")

(def new-patterns {})

(def plan
  [["scribe-pass-12-drafts.edn" "apply-radial-R3-integration-through-a-one-dimensional-profile" "math/measure-integration-api" :feedback]
   ["scribe-pass-12-drafts.edn" "refute-essential-boundedness-at-a-singularity-on-a-positive-measure-ball" "math/measure-integration-api" :feedback]
   ["scribe-pass-12-drafts.edn" "make-construction-targets-buildable-importable-and-advertised-before-counting-them" "math/missing-dependency-protocol" :feedback]
   ["scribe-pass-12-drafts.edn" "derive-circle-curve-length-from-the-average-of-the-derivative" "math/holomorphic-disk-api" :feedback]
   ["scribe-pass-12-drafts.edn" "prove-exponential-Jensen-with-integrable-log-and-set-averages" "math/measure-integration-api" :feedback]
   ["scribe-pass-12-drafts.edn" "transport-a-composition-derivative-through-local-eventual-equality" "math/holomorphic-disk-api" :feedback]
   ["scribe-pass-12-drafts.edn" "analytic-order-at-least-two-implies-local-noninjectivity" "math/holomorphic-disk-api" :reference]
   ["scribe-pass-12-drafts.edn" "bound-automatic-frontier-descent-when-a-leaf-recurses" "math/missing-dependency-protocol" :feedback]
   ["scribe-pass-12-drafts.edn" "differentiate-a-cauchy-transform-locally-under-the-integral" "math/measure-integration-api" :feedback]
   ["scribe-pass-12-drafts.edn" "place-section-include-after-a-definition-to-repair-theorems-without-changing-the-definition" "math/missing-dependency-protocol" :feedback]
   ["scribe-pass-12-drafts.edn" "derive-local-L1-from-weak-L2-by-layer-cake-and-optimized-splitting" "math/measure-integration-api" :feedback]
   ["scribe-pass-12-drafts.edn" "order-proof-search-by-known-route-components-before-literature" "math/missing-dependency-protocol" :feedback]])

;; Owner-review amendments carried into the store with the memory.
(def review-notes
  {"order-proof-search-by-known-route-components-before-literature"
   (str "Owner review: this draft encodes Joe's cond model (2026-07-30) - arXiv is the FALL-THROUGH, "
        "not a first stop, and the discriminator is whether a route is already known. Confidence is "
        "correctly marked :two-case-inference-explicitly-marked: it rests on exactly two cases, a01A02 "
        "(route known, components sufficed, arXiv empty across ~8 queries) and the retrospective S6 "
        "integral-Minkowski case (route unknown, arXiv necessary). n=2. Do not read a global hit-rate "
        "into it, and note that a surface NOT NEEDED is not a surface that FAILED.")
   "bound-automatic-frontier-descent-when-a-leaf-recurses"
   (str "Owner review: policy inference, correctly marked as such. Ground control applied this by hand "
        "after the a95J01 -> UnivalentDeriv chain recursed to a third level and threatened to starve the "
        "repaired-row cohort behind it. Not yet encoded in the cron - it remains ground control's "
        "discretion each time, which is exactly the note-not-mechanism weakness recorded in the register.")
   "place-section-include-after-a-definition-to-repair-theorems-without-changing-the-definition"
   (str "Owner review: verified in the field, not merely proposed. a00J05's repair placed `include` AFTER "
        "`def cauchyTransform` precisely so the definition kept its `Set R -> C -> C` signature, and the "
        "subsequent run used hK_compact SIX times. The placement detail is the load-bearing part of this "
        "memory: including before the def would have changed the definition's arity and broken every use.")})

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
                      :throw false :timeout 120000})]
    (when (= 503 (:status r))
      (throw (ex-info "store busy — stopping" {:path path})))
    (when-not (or (<= 200 (:status r) 299) (= 409 (:status r)))
      (throw (ex-info "write failed" {:path path :status (:status r)})))
    (Thread/sleep pace-ms)
    (if (= 409 (:status r)) :already-exists true)))

(defn promote! [draft pattern kind distill session-id]
  (let [nm (:name draft)
        ids {:entry (str "e-codexpilot-" nm)
             :hx (str "hx-codexpilot-" nm)
             :review (str "e-review-codexpilot-" nm)}
        b (:body draft)
        subject-ids (mapv (comp str :ref/id) (:subjects draft))
        endpoints (->> (concat [(:entry ids)] subject-ids
                               [pattern distill session-id mission-id])
                       (remove str/blank?) distinct vec)
        note (get review-notes nm)]
    (post! "/api/alpha/evidence"
           {:evidence/id (:entry ids) :evidence/type :memory
            :evidence/claim-type :assert :evidence/author reviewer
            :evidence/session-id session-id
            :evidence/subject (first (:subjects draft))
            :evidence/tags [:memory :mathematics :codex-pilot (:lane draft)]
            :evidence/body {:name nm :hook (hook-for draft) :kind kind :body b
                            :why (or (:problem-class b) (:trigger b) (get-in b [:scope :context]))
                            :how-to-apply (or (:rule b) (:qa-rule b) (:measurement-rule b)
                                              (some->> (:proof-shape b) (str/join " → "))
                                              (some->> (:route b) (str/join " → "))
                                              (some->> (:observed-trajectory b) (str/join " → ")))
                            :scribe-author "codex-5"
                            :draft-source "holes/labs/M-codex-sorry-loop/"
                            :runner-model :codex
                            :promotion (cond-> {:reviewed-by reviewer :verdict :approve :warrant warrant}
                                         note (assoc :review-note note))}})
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
            :evidence/body (cond-> {:review/event :memory-attachment-review
                                    :review/memory-id (:entry ids)
                                    :review/pattern-ids [pattern]
                                    :review/verdict :approve
                                    :review/witness-status (witness-for (:lane draft))
                                    :review/provenance {:kind :operator-delegated-in-use-evaluation
                                                        :reviewer reviewer :date "2026-07-30"
                                                        :warrant warrant}
                                    :review/policy-verdict :pudding-delegated}
                             note (assoc :review/amendment note))
            :evidence/tags [:memory :memory/review :mathematics :codex-pilot]})
    (if (and (exists? (str "/api/alpha/evidence/" (:entry ids)))
             (exists? (str "/api/alpha/evidence/" (:review ids))))
      :promoted :VERIFY-FAILED)))

(let [drafts-by-file (into {} (for [f (distinct (map first plan))]
                                [f (into {} (map (juxt :name identity))
                                         (edn/read-string (slurp (str root "/" f))))]))
      _ (when-not (exists? "/api/alpha/evidence?limit=1")
          (throw (ex-info "store health probe failed" {})))
      results
      (vec (for [[file nm pattern kind] plan
                 :let [draft (get-in drafts-by-file [file nm])]]
             (cond
               (nil? draft) {:name nm :status :DRAFT-NOT-FOUND}
               (exists? (str "/api/alpha/evidence/e-codexpilot-" nm))
               {:name nm :status :skipped-existing}
               (not commit?) {:name nm :status :dry-run :pattern pattern :kind kind
                              :has-note (boolean (get review-notes nm))
                              :distill (first (get-in draft [:evidence :turn-round-ids]))}
               :else {:name nm :pattern pattern
                      :status (promote! draft pattern kind
                                        (first (get-in draft [:evidence :turn-round-ids]))
                                        "M-codex-sorry-loop/duree-lane")})))]
  (spit (str root "/promotion-pass-12-report.edn")
        (pr-str {:at "2026-07-30" :mode (if commit? :commit :dry-run)
                 :reviewer reviewer :patterns :none-minted :results results}))
  (doseq [r results] (println (:status r) "|" (:name r) "|" (:pattern r "")))
  (println "TOTAL:" (frequencies (map :status results))))
