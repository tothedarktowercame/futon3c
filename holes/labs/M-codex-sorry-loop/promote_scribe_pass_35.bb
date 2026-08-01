#!/usr/bin/env bb
;; Promotion: scribe pass 35 — SEVEN runs — 3 drafts (2 solve, 1 trajectory).
;; Scribe verdicts, all sharpening claude-9's framing:
;;  (1) YoungL2 reachability is a SECOND, STRONGER instance of the existing
;;      construction-delivery memory, NOT a new one - amend it with delivery
;;      states through CONSUMER IMPORT and FIELD USE.
;;  (2) The stale-note class warrants BOTH a standing installed-revision check
;;      AND the calibration "flag the class; do not convict an instance".
;;      NOTE: a memory for this already existed - treat-not-in-mathlib-comments-
;;      as-revision-scoped - so tonight's "finding" was a rediscovery.
;;  (3) Rouche-named declarations proved by another route EXTEND the existing
;;      route-override rule; names may preserve historical scaffolding.
;; plus TWO new terrain patterns (the fix for consecutive recall
;; misses: complex-analysis and topology had no patterns, so no lexical
;; bridge could carry packets to memories). Shapes mirror
;; promote_scribe_pass_1.bb + wire_math_memory_patterns.clj
;; ensure-pattern!/description-entry (mint receipt + description
;; evidence, deterministic ids). RAM caution: health probe, 150ms
;; pacing, idempotent, read-back verify, stop on 503.

(require '[babashka.http-client :as http]
         '[babashka.process :as process]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def root "/home/joe/code/futon3c/holes/labs/M-codex-sorry-loop")
(def base "http://127.0.0.1:7073")
(def commit? (= "--commit" (first *command-line-args*)))
(def pace-ms 400)   ;; store writes are slower under load (measured 2026-07-30)
(def mission-id "M-codex-sorry-loop")
(def reviewer "claude-9")   ;; the OWNER, who reviews
(def drafter  "codex-5")    ;; the SCRIBE, who authored the draft
;; The memory entry must be authored by the DRAFTER, not the reviewer.
;; memory-lifecycle/review-attachment! refuses when the review evidence
;; author equals the memory author, so authoring both as the owner makes
;; the attachment permanently unreviewable — which is why every codex-lane
;; memory has been invisible to recall. Found 2026-07-30.
(def recall-system :v1.2-normalized)
(def warrant "operator-delegated: in-use evaluation (proof-in-pudding; owner draft review claude-6 2026-07-28; Joe's standing direction)")

(def new-patterns {})

(def plan
  ;; Witness status and use channel are reviewer judgements, never functions
  ;; of the lane label. review-attachment! validates and projects both.
  [["scribe-pass-35-drafts.edn" "prove-exponential-cubic-injectivity-by-linear-term-domination" "math/derivative-bounds-api" :feedback :independently-witnessed :substitutive]
   ["scribe-pass-35-drafts.edn" "force-a-sublinear-entire-function-constant-by-Cauchy-derivative-estimates" "math/entire-and-singularity-api" :feedback :independently-witnessed :substitutive]
   ["scribe-pass-35-drafts.edn" "pair-a-machine-checked-refutation-with-a-machine-checked-statement-repair" "math/corpus-trust-protocol" :feedback :independently-witnessed :regulative]])

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
                      :throw false :timeout 120000})]
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
                 {:evidence/id (str "e-pattern-description-v1-" (str/replace pattern-id "/" "-"))
                  :evidence/subject {:ref/type :pattern :ref/id pattern-id}
                  :evidence/type :reflection :evidence/claim-type :observation
                  :evidence/author reviewer
                  :evidence/session-id "M-codex-sorry-loop/recall-vocabulary"
                  :evidence/body {:event :pattern-description :pattern-id pattern-id
                                  :description description :recall-system recall-system}
                  :evidence/tags [:memory :pattern-description :mathematics]})
          :minted))))

(defn apply-review! [memory-id review-id pattern-id]
  (let [{:keys [exit out err]}
        (process/shell
         {:dir "/home/joe/code/futon3c" :out :string :err :string}
         "clojure" "-M" "scripts/review_codex_lane_attachments.clj"
         "--commit" "--memory-id" memory-id
         "--review-evidence-id" review-id "--pattern-id" pattern-id)]
    (when-not (zero? exit)
      (throw (ex-info "lifecycle review failed"
                      {:memory-id memory-id :exit exit :out out :err err})))
    out))

(defn promote! [draft pattern kind witness-status memory-use-kind distill session-id]
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
            :evidence/claim-type :assert :evidence/author drafter
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
                            :review/witness-status witness-status
                            :memory-use/kind memory-use-kind
                            :review/provenance {:kind :operator-delegated-in-use-evaluation
                                                :reviewer reviewer :date "2026-07-28"
                                                :warrant warrant}
                            :review/policy-verdict :pudding-delegated}
            :evidence/tags [:memory :memory/review :mathematics :codex-pilot]})
    ;; This is the A3 seam: the separately authored evidence, not the lane,
    ;; earns witness status.  The lifecycle also makes the attachment visible.
    (apply-review! (:entry ids) (:review ids) pattern)
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
      (vec (for [[file nm pattern kind witness-status memory-use-kind] plan
                 :let [draft (get-in drafts-by-file [file nm])]]
             (cond
               (nil? draft) {:name nm :status :DRAFT-NOT-FOUND}
               (exists? (str "/api/alpha/evidence/e-codexpilot-" nm))
               (if commit?
                 (do
                   (apply-review! (str "e-codexpilot-" nm)
                                  (str "e-review-codexpilot-" nm)
                                  pattern)
                   {:name nm :status :reviewed-existing})
                 {:name nm :status :skipped-existing})
               (not commit?) {:name nm :status :dry-run :pattern pattern :kind kind
                              :witness-status witness-status
                              :memory-use/kind memory-use-kind}
               :else {:name nm :pattern pattern
                      :status (promote! draft pattern kind witness-status
                                        memory-use-kind
                                        (first (get-in draft [:evidence :turn-round-ids]))
                                        "M-codex-sorry-loop/duree-lane")})))]
  (spit (str root "/promotion-pass-23-report.edn")
        (pr-str {:at "2026-07-28" :mode (if commit? :commit :dry-run)
                 :patterns pattern-results :results results}))
  (println "patterns:" pattern-results)
  (doseq [r results] (println (:status r) "|" (:name r) "|" (:pattern r "")))
  (println "TOTAL:" (frequencies (map :status results))))
