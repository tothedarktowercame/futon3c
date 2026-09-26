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
;; call. This script does not infer who performed a review. Reviewer, session,
;; verdict, and evidence id (or batch prefix) come from the invocation and are
;; checked against the separately authored evidence row before projection.
;;
;; Usage:
;;   clojure -M scripts/review_codex_lane_attachments.clj [--commit] [--names-file F]
;;     --reviewer ID --session-id ID --verdict approve|reject
;;     --review-evidence-prefix PREFIX
;;   clojure -M scripts/review_codex_lane_attachments.clj [--commit]
;;     --reviewer ID --session-id ID --verdict approve|reject|reassign
;;     --memory-id ID --review-evidence-id ID
;;     (--pattern-id ID | --pattern-ids ID[,ID...])
;;
;; The explicit form is the forward promotion seam: a promotion invokes it
;; immediately after appending the separately authored review evidence.  The
;; batch form remains for historical reconciliation.  Default is a genuine
;; dry run; only --commit may apply a review projection.

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[org.httpkit.client :as http]
         '[futon3c.peripheral.memory-lifecycle :as lifecycle]
         '[futon3c.substrate.client :as substrate]
         '[futon3c.evidence.futon1b-backend :as f1b]
         '[futon3c.evidence.store :as estore])

(def base-url (or (System/getenv "FUTON_SUBSTRATE_URL") "http://127.0.0.1:7073"))
(def commit? (some #{"--commit"} *command-line-args*))

(defn- arg-value [flag]
  (second (drop-while #(not= flag %) *command-line-args*)))

(defn- invocation-value [flag env-name]
  (or (arg-value flag) (System/getenv env-name)))

(defn- require-invocation-value [flag env-name]
  (let [value (invocation-value flag env-name)]
    (when-not (and (string? value) (not (str/blank? value)))
      (throw (ex-info (str "missing required review invocation value: " flag
                           " or " env-name)
                      {:flag flag :environment-variable env-name})))
    value))

(def reviewer
  (require-invocation-value "--reviewer" "ATTACHMENT_REVIEWER"))
(def review-session-id
  (require-invocation-value "--session-id" "ATTACHMENT_REVIEW_SESSION_ID"))
(def verdict
  (let [value (require-invocation-value "--verdict" "ATTACHMENT_REVIEW_VERDICT")
        parsed (keyword (str/replace value #"^:" ""))]
    (when-not (contains? #{:approve :reject :reassign} parsed)
      (throw (ex-info "attachment review verdict must be approve, reject, or reassign"
                      {:verdict value :allowed [:approve :reject :reassign]})))
    parsed))
(def review-evidence-prefix
  (invocation-value "--review-evidence-prefix"
                    "ATTACHMENT_REVIEW_EVIDENCE_PREFIX"))

(defn- memory-edge [memory-id]
  (first (filter #(= memory-id (get-in % [:hx/props :roles :entry]))
                 ;; The default limit of 10000 is rejected by the server with HTTP 400;
                 ;; a memory has a handful of edges, so cap it small.
                 (substrate/hyperedges-by-end memory-id {:limit 50}))))

(defn- review-evidence-id [memory-name]
  (when-not (and (string? review-evidence-prefix)
                 (not (str/blank? review-evidence-prefix)))
    (throw (ex-info "batch review requires an evidence-id prefix from the invocation"
                    {:flag "--review-evidence-prefix"
                     :environment-variable "ATTACHMENT_REVIEW_EVIDENCE_PREFIX"})))
  (str review-evidence-prefix memory-name))

(defn- pattern-ids-of [edge]
  (vec (or (get-in edge [:hx/props :roles :patterns])
           (filterv #(str/includes? % "/")
                    (get-in edge [:hx/props :roles :subjects] [])))))

(defn- fetch-pattern [pattern-id]
  (let [encoded (java.net.URLEncoder/encode pattern-id "UTF-8")
        response @(http/get (str (str/replace base-url #"/+$" "")
                                 "/api/alpha/entity/" encoded)
                            {:headers {"Accept" "application/edn"}
                             :timeout 60000})
        body (cond
               (string? (:body response)) (:body response)
               (some? (:body response)) (slurp (:body response))
               :else "")]
    (when-not (= 200 (:status response))
      (throw (ex-info "proposed attachment pattern does not exist"
                      {:pattern-id pattern-id :status (:status response)
                       :body body})))
    (let [pattern (edn/read-string body)]
      {:pattern-id pattern-id
       :hook (or (:pattern/hook pattern) (:hook pattern)
                 (get-in pattern [:entity :pattern/hook])
                 (get-in pattern [:entity :hook])
                 (get-in pattern [:entity :source]))})))

(def names-filter
  (when-let [f (arg-value "--names-file")]
    (set (edn/read-string (slurp f)))))

(def explicit-target
  (let [memory-id (arg-value "--memory-id")
        review-id (arg-value "--review-evidence-id")
        pattern-id (arg-value "--pattern-id")
        pattern-ids-arg (arg-value "--pattern-ids")
        pattern-ids (if pattern-ids-arg
                      (->> (str/split pattern-ids-arg #",")
                           (mapv str/trim)
                           (into [] (remove str/blank?)))
                      (when pattern-id [pattern-id]))]
    (when (some some? [memory-id review-id pattern-id pattern-ids-arg])
      (when-not (every? #(and (string? %) (not (str/blank? %)))
                        [memory-id review-id])
        (throw (ex-info "explicit review requires memory, review-evidence, and pattern ids"
                        {:memory-id memory-id
                         :review-evidence-id review-id
                         :pattern-ids pattern-ids})))
      (when-not (seq pattern-ids)
        (throw (ex-info "explicit review requires at least one pattern id"
                        {:pattern-ids pattern-ids})))
      (when (and (= :reassign verdict) (nil? pattern-ids-arg))
        (throw (ex-info "reassign requires the explicit --pattern-ids form"
                        {:verdict verdict :flag "--pattern-ids"})))
      {:name memory-id
       :memory-id memory-id
       :review-evidence-id review-id
       :pattern-ids pattern-ids})))

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
  (when (and (= :reassign verdict) (nil? explicit-target))
    (throw (ex-info "reassign is available only for one explicit memory"
                    {:verdict verdict :flag "--memory-id"})))
  (let [evidence-store (f1b/make-futon1b-backend base-url)
        ctx {:agent-id reviewer
             :session-id review-session-id
             :domain :mathematics
             :evidence-store evidence-store}
        targets (if explicit-target [explicit-target]
                    (codex-lane-memory-ids))
        results
        (mapv
         (fn [{:keys [name memory-id pattern-ids] :as target}]
           (let [proposed-patterns (when (= :reassign verdict)
                                     (mapv fetch-pattern pattern-ids))
                 edge (memory-edge memory-id)
                 rev-id (or (:review-evidence-id target)
                            (review-evidence-id name))
                 pats (or pattern-ids (pattern-ids-of edge))
                 current-patterns (pattern-ids-of edge)
                 status (get-in edge [:hx/props :attachment-status])
                 review-entry (delay (estore/get-entry* evidence-store rev-id))]
             (cond
               (nil? edge) {:name name :result :no-edge}
               (and (= :approve verdict) (= :reviewed status))
               {:name name :result :existing}
               (empty? pats) {:name name :result :no-pattern-ids}
               (and commit? (nil? @review-entry))
               {:name name :result :no-review-evidence :expected rev-id}

               (not commit?)
               {:name name :result :would-review
                :memory-id memory-id
                :reviewer reviewer :session-id review-session-id
                :verdict verdict
                :current-patterns current-patterns
                :proposed-patterns (or proposed-patterns pats)
                :review-evidence-id rev-id
                :review-evidence-present? (boolean @review-entry)}

               :else
               (let [r (lifecycle/review-attachment!
                        ctx
                        {:memory-id memory-id
                         :review-evidence-id rev-id
                         :verdict verdict
                         :pattern-ids pats})]
                 {:name name :result (if (:ok r)
                                       (if (= :approve verdict)
                                         :reviewed
                                         (if (= :reassign verdict)
                                           :reassigned
                                           :rejection-recorded))
                                       :failed)
                  :detail (when-not (:ok r) r)}))))
         targets)]
    (prn {:ok true
          :commit? (boolean commit?)
          :total (count targets)
          :tally (frequencies (map :result results))
          :results (when-not commit? results)
          :failures (filterv #(#{:failed :no-edge :no-review-evidence :no-pattern-ids} (:result %)) results)})))

(-main)
