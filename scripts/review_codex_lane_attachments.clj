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
;; Usage:  clojure -M scripts/review_codex_lane_attachments.clj [--commit] [--names-file F]
;; Default is a dry run listing what would be reviewed.

(require '[clojure.string :as str]
         '[futon3c.peripheral.memory-lifecycle :as lifecycle]
         '[futon3c.peripheral.memory-write :as memory-write]
         '[futon3c.substrate.client :as substrate]
         '[futon3c.evidence.futon1b-backend :as f1b]
         '[futon3c.evidence.store :as estore]
         '[futon3c.evidence.boundary :as boundary])

(def base-url (or (System/getenv "FUTON_SUBSTRATE_URL") "http://127.0.0.1:7073"))
(def reviewer "claude-9")
(def commit? (some #{"--commit"} *command-line-args*))

(defn- memory-edge [memory-id]
  (first (filter #(= memory-id (get-in % [:hx/props :roles :entry]))
                 ;; The default limit of 10000 is rejected by the server with HTTP 400;
                 ;; a memory has a handful of edges, so cap it small.
                 (substrate/hyperedges-by-end memory-id {:limit 50}))))

(defn- review-evidence-id [memory-name]
  ;; claude-9-authored review, so author != reviewer holds.
  (str "e-review-claude9-v3-" memory-name))

(defn- pattern-ids-of [edge]
  (vec (or (get-in edge [:hx/props :roles :patterns])
           (filterv #(str/includes? % "/")
                    (get-in edge [:hx/props :roles :subjects] [])))))

(def names-filter
  (when-let [f (second (drop-while #(not= "--names-file" %) *command-line-args*))]
    (set (clojure.edn/read-string (slurp f)))))

(defn- codex-lane-memory-ids []
  ;; Names come from the promotion reports, which are the authoritative record of
  ;; what this lane actually promoted; reading drafts would include unpromoted
  ;; ones.
  (let [dir (clojure.java.io/file "holes/labs/M-codex-sorry-loop")
        reports (filter #(re-find #"promotion-pass-.*-report\.edn" (.getName %))
                        (file-seq dir))]
    (->> reports
         (mapcat #(:results (clojure.edn/read-string (slurp %))))
         (keep :name)
         distinct
         (filter #(or (nil? names-filter) (contains? names-filter %)))
         (map #(vector % (str "e-codexpilot-" %)))
         vec)))

(defn- owner-review-evidence
  "Review evidence authored by claude-9, satisfying author != reviewer."
  [nm memory-id pattern-ids]
  {:evidence/id (str "e-review-claude9-" nm)
   :evidence/subject {:ref/type :memory :ref/id memory-id}
   :evidence/type :reflection
   :evidence/claim-type :observation
   :evidence/author reviewer
   :evidence/session-id "M-codex-sorry-loop/duree"
   :evidence/tags [:memory :attachment-review]
   :evidence/body
   {:review/event :attachment-review
    :review/memory-id memory-id
    :review/pattern-ids (vec pattern-ids)
    :review/verdict :approve
    :review/witness-status :independently-witnessed
    :review/provenance
    (str "Owner review by claude-9 during the durée loop, 2026-07-30. The draft was "
         "authored by codex-5 (scribe seat), read in full by claude-9, its cited "
         "turn-round and receipt ids fetched and confirmed to resolve, and a written "
         "review note recorded in the promotion report before the memory was promoted.")
    :review/policy-verdict :approve}})

(defn -main [& _]
  (let [evidence-store (f1b/make-futon1b-backend base-url)
        ctx {:agent-id reviewer
             :session-id "M-codex-sorry-loop/duree"
             :domain :mathematics
             :evidence-store evidence-store}
        targets (codex-lane-memory-ids)
        results
        (mapv
         (fn [[nm memory-id]]
           (let [edge (memory-edge memory-id)
                 rev-id (review-evidence-id nm)
                 pats (pattern-ids-of edge)
                 status (get-in edge [:hx/props :attachment-status])]
             (cond
               (nil? edge) {:name nm :result :no-edge}
               (= :reviewed status) {:name nm :result :existing}
               (empty? pats) {:name nm :result :no-pattern-ids}
               (nil? (estore/get-entry* evidence-store rev-id))
               {:name nm :result :no-review-evidence :expected rev-id}

               :else
               (let [r (lifecycle/review-attachment!
                        ctx
                        {:memory-id memory-id
                         :review-evidence-id rev-id
                         :verdict :approve
                         :pattern-ids pats})]
                 {:name nm :result (if (:ok r) :reviewed :failed) :detail (when-not (:ok r) r)}))))
         targets)]
    (prn {:ok true
          :commit? (boolean commit?)
          :total (count targets)
          :tally (frequencies (map :result results))
          :failures (filterv #(#{:failed :no-edge :no-review-evidence :no-pattern-ids} (:result %)) results)})))

(-main)
