;; Apply owner review to codex-lane memory/pattern attachments.
;;
;; WHY THIS EXISTS (2026-07-30). The codex-lane promotion pipeline
;; (holes/labs/M-codex-sorry-loop/promote_scribe_pass_*.bb) writes the memory,
;; the pattern attachment edge, AND a well-formed review evidence record with
;; :review/verdict :approve and :review/witness-status — but it never calls
;; memory-lifecycle/review-attachment! to APPLY that review to the edge. So
;; every edge stayed at :attachment-status :proposed.
;;
;; memory-recall/project-components excludes anything whose attachment-status is
;; not :reviewed. Consequence: EVERY codex-lane memory ever promoted has been
;; invisible to recall-by-endpoints, and therefore to propose-patterns-by-query,
;; and therefore to dispatch-time recall — for the lane's entire history. That is
;; upstream of every "recall empty" datum the lane has recorded.
;;
;; Modelled on scripts/wire_math_memory_patterns.clj, which does this correctly
;; for the zai lane. Idempotent: an already-reviewed attachment reports :existing.
;;
;; INTEGRITY CONSTRAINT. memory-lifecycle refuses a review whose evidence author
;; equals the memory's author — author must not review their own attachment. The
;; codex-lane pipeline sets BOTH to the same constant ("claude-6"), so it cannot
;; produce a reviewable attachment at all; that is the root defect, not a missing
;; call. The review evidence written here is authored by claude-9 and therefore
;; satisfies the guard.
;;
;; It is only written for memories claude-9 PERSONALLY owner-reviewed (passes
;; 12-22, each with a written review note). The 51 earlier memories were reviewed
;; by claude-6 and are NOT backfilled here: asserting a review I did not perform
;; would be exactly the false attestation this guard exists to prevent. They are
;; reported and left for an operator decision.
;;
;; Usage:
;;   clojure -M scripts/review_codex_lane_attachments.clj [--commit] [--names-file F]
;;   clojure -M scripts/review_codex_lane_attachments.clj [--commit]
;;     --memory-id ID --review-evidence-id ID --pattern-id ID
;;
;; The explicit form is the forward promotion seam: a promotion invokes it
;; immediately after appending the separately authored review evidence.  The
;; batch form remains for historical reconciliation.  Default is a genuine
;; dry run; only --commit may apply a review projection.

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[futon3c.peripheral.memory-lifecycle :as lifecycle]
         '[futon3c.substrate.client :as substrate]
         '[futon3c.evidence.futon1b-backend :as f1b]
         '[futon3c.evidence.store :as estore])

(def base-url (or (System/getenv "FUTON_SUBSTRATE_URL") "http://127.0.0.1:7073"))
(def reviewer
  (or (System/getenv "ATTACHMENT_REVIEWER") "claude-9"))
(def review-evidence-prefix
  ;; Preserve the historical backfill default.  Cross-author review jobs set
  ;; this explicitly so the evidence id names the actual reviewer.
  (or (System/getenv "ATTACHMENT_REVIEW_EVIDENCE_PREFIX")
      "e-review-claude9-earlier-"))
(def commit? (some #{"--commit"} *command-line-args*))

(defn- arg-value [flag]
  (second (drop-while #(not= flag %) *command-line-args*)))

(defn- memory-edge [memory-id]
  (first (filter #(= memory-id (get-in % [:hx/props :roles :entry]))
                 ;; The default limit of 10000 is rejected by the server with HTTP 400;
                 ;; a memory has a handful of edges, so cap it small.
                 (substrate/hyperedges-by-end memory-id {:limit 50}))))

(defn- review-evidence-id [memory-name]
  (str review-evidence-prefix memory-name))

(defn- pattern-ids-of [edge]
  (vec (or (get-in edge [:hx/props :roles :patterns])
           (filterv #(str/includes? % "/")
                    (get-in edge [:hx/props :roles :subjects] [])))))

(def names-filter
  (when-let [f (arg-value "--names-file")]
    (set (edn/read-string (slurp f)))))

(def explicit-target
  (let [memory-id (arg-value "--memory-id")
        review-id (arg-value "--review-evidence-id")
        pattern-id (arg-value "--pattern-id")]
    (when (some some? [memory-id review-id pattern-id])
      (when-not (every? #(and (string? %) (not (str/blank? %)))
                        [memory-id review-id pattern-id])
        (throw (ex-info "explicit review requires memory, review-evidence, and pattern ids"
                        {:memory-id memory-id
                         :review-evidence-id review-id
                         :pattern-id pattern-id})))
      {:name memory-id
       :memory-id memory-id
       :review-evidence-id review-id
       :pattern-ids [pattern-id]})))

(defn- codex-lane-memory-ids []
  ;; Names come from the promotion reports, which are the authoritative record of
  ;; what this lane actually promoted; reading drafts would include unpromoted
  ;; ones.
  (let [dir (io/file "holes/labs/M-codex-sorry-loop")
        reports (filter #(re-find #"promotion-pass-.*-report\.edn" (.getName %))
                        (file-seq dir))]
    (->> reports
         (mapcat #(:results (edn/read-string (slurp %))))
         (keep :name)
         distinct
         (filter #(or (nil? names-filter) (contains? names-filter %)))
         (map (fn [name]
                {:name name
                 :memory-id (str "e-codexpilot-" name)}))
         vec)))

(defn -main [& _]
  (let [evidence-store (f1b/make-futon1b-backend base-url)
        ctx {:agent-id reviewer
             :session-id "M-codex-sorry-loop/duree"
             :domain :mathematics
             :evidence-store evidence-store}
        targets (if explicit-target [explicit-target]
                    (codex-lane-memory-ids))
        results
        (mapv
         (fn [{:keys [name memory-id pattern-ids] :as target}]
           (let [edge (memory-edge memory-id)
                 rev-id (or (:review-evidence-id target)
                            (review-evidence-id name))
                 pats (or pattern-ids (pattern-ids-of edge))
                 status (get-in edge [:hx/props :attachment-status])]
             (cond
               (nil? edge) {:name name :result :no-edge}
               (= :reviewed status) {:name name :result :existing}
               (empty? pats) {:name name :result :no-pattern-ids}
               (nil? (estore/get-entry* evidence-store rev-id))
               {:name name :result :no-review-evidence :expected rev-id}

               (not commit?)
               {:name name :result :would-review
                :memory-id memory-id :review-evidence-id rev-id
                :pattern-ids pats}

               :else
               (let [r (lifecycle/review-attachment!
                        ctx
                        {:memory-id memory-id
                         :review-evidence-id rev-id
                         :verdict :approve
                         :pattern-ids pats})]
                 {:name name :result (if (:ok r) :reviewed :failed)
                  :detail (when-not (:ok r) r)}))))
         targets)]
    (prn {:ok true
          :commit? (boolean commit?)
          :total (count targets)
          :tally (frequencies (map :result results))
          :failures (filterv #(#{:failed :no-edge :no-review-evidence :no-pattern-ids} (:result %)) results)})))

(-main)
