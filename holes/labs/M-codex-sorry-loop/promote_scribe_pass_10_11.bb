#!/usr/bin/env bb
;; Batch promotion: scribe passes 10 (a95J07 Sard/Jacobian) + 11 (a00J01
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
(def pace-ms 150)
(def mission-id "M-codex-sorry-loop")
(def reviewer "claude-9")
(def recall-system :v1.2-normalized)
(def warrant "operator-delegated: in-use evaluation (proof-in-pudding; owner draft review claude-9 2026-07-29/30; Joe's standing direction)")

(def new-patterns {})

(def plan
  [["scribe-pass-10-drafts.edn" "specialize-fixed-dimensional-jacobian-sard-to-one-dimensional-deriv" "math/measure-integration-api" :feedback]
   ["scribe-pass-10-drafts.edn" "treat-not-in-mathlib-comments-as-revision-scoped-search-claims" "math/missing-dependency-protocol" :feedback]
   ["scribe-pass-10-drafts.edn" "record-genuine-statement-check-after-two-vacuity-detections" "math/missing-dependency-protocol" :feedback]
   ["scribe-pass-11-drafts.edn" "prove-eLpNorm-to-essential-supremum-by-superlevel-and-probability-monotonicity" "math/measure-integration-api" :feedback]
   ["scribe-pass-11-drafts.edn" "record-unused-hypothesis-as-stronger-than-stated-signal" "math/missing-dependency-protocol" :feedback]
   ["scribe-pass-11-drafts.edn" "separate-lexical-sorry-count-from-real-proof-hole-count" "math/missing-dependency-protocol" :feedback]])

;; Owner-review amendments carried into the store with the memory.
(def review-notes
  {"treat-not-in-mathlib-comments-as-revision-scoped-search-claims"
   (str "Confidence DOWNGRADED by owner review from :receipt-confirmed to "
        ":commit-confirmed. Outcome receipt 1a77b36d does not record the "
        "stale-comment refutation; the evidence is commit d224a3d. Ground "
        "control's receipt was thin, not the draft. Rule adopted: anything "
        "intended to warrant a memory must enter the receipt at write time, "
        "because the outcome half is write-once per job-id.")
   "separate-lexical-sorry-count-from-real-proof-hole-count"
   (str "Vindicated in the field the same day and again the next: the lexical "
        "meter overstated against a correct runner report on a00J01 (3 vs 2) "
        "and on a01A06 (5 vs 4), each time because a commit rewrote a comment "
        "containing the word. Receipts now carry :sorry-delta-real and "
        ":sorry-delta-lexical separately.")
   "record-unused-hypothesis-as-stronger-than-stated-signal"
   (str "Owner review confirmed the proposition is UNCHANGED by the "
        "hf_top -> _hf_top rename (alpha-equivalent; hypothesis still "
        "required at call sites). Draft correctly declines to claim a "
        "Mathlib-facing result.")})

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
                      :throw false :timeout 30000})]
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
                                        "M-codex-sorry-loop/cron-lane")})))]
  (spit (str root "/promotion-pass-10-11-report.edn")
        (pr-str {:at "2026-07-30" :mode (if commit? :commit :dry-run)
                 :reviewer reviewer :patterns :none-minted :results results}))
  (doseq [r results] (println (:status r) "|" (:name r) "|" (:pattern r "")))
  (println "TOTAL:" (frequencies (map :status results))))
