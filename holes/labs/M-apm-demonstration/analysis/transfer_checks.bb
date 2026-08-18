#!/usr/bin/env bb
;; transfer_checks.bb — the e-c0a2d2fe checks (M-apm-demonstration W.61).
;;
;; One memory's f7 journey exposed the gaps between RECORDED, REACHABLE,
;; USED, and MEASURED. Each gap is a check here, runnable per frame:
;; known-failing on f7's data, hopefully passing on f8's. The passing
;; fraction over frames is the memory-transfer loss function, measured.
;;
;; Usage: bb transfer_checks.bb <problem-state-dir>
;;   e.g. bb transfer_checks.bb \
;;     data/problem-state/a98A01-93f5be72…  (f7 baseline)
;;
;; Reads the saved cycle state, queries the substrate, and overwrites the
;; stable machine-readable receipt <problem-state-dir>/transfer-checks.edn.

(require '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[babashka.fs :as fs]
         '[cheshire.core :as json]
         '[babashka.http-client :as http])

(def substrate "http://127.0.0.1:7073")

(defn latest-state [dir]
  (let [f (->> (fs/glob dir "v*.edn")
               (sort-by #(parse-long (str/replace (fs/file-name %) #"[v.edn]" "")))
               last)]
    {:file (str f) :state (edn/read-string (slurp (str f)))}))

(defn fetch-entry [id]
  (try (let [r (http/get (str substrate "/api/alpha/evidence/" id)
                         {:throw false})]
         (when (= 200 (:status r)) (edn/read-string (:body r))))
       (catch Exception _ nil)))

(defn projection [endpoint]
  (try (-> (http/post (str substrate "/api/alpha/memory/projection")
                      {:headers {"Content-Type" "application/json"}
                       :body (json/generate-string {:endpoints [endpoint] :limit 100})
                       :throw false})
           :body (json/parse-string true) :groups first :components)
       (catch Exception _ nil)))

(defn steps-of [state tool] (filter #(= tool (:tool %)) (:steps state)))

(defn pull-receipts
  "All pull-offer/pull-use receipts for one dispatch job, straight from the
   substrate (the saved state cannot carry the evidence-store handle, and
   cycle outputs do not join offer receipts at all — the offer denominator
   lives only in the store)."
  [job-id]
  (try (let [r (http/get (str substrate "/api/alpha/evidence/text-search")
                         {:query-params {:q job-id :limit "50"} :throw false})
             body (when (= 200 (:status r)) (edn/read-string (:body r)))]
         (->> (:results body)
              (map :entry)
              (filter #(and (#{:memory-pull-offer :memory-pull-use}
                             (get-in % [:evidence/body :event]))
                            (= job-id (get-in % [:evidence/body :dispatch-id]))))
              vec))
       (catch Exception _ [])))

(defn run-checks [dir]
  (let [{:keys [file state]} (latest-state dir)
        outputs (:cycle/outputs state)
        reg (:registration outputs)
        problem (get-in reg [:problem :problem-id])
        guide-seat (:reg/guide-seat reg)
        interventions (->> (steps-of state :write-intervention)
                           (concat (steps-of state :store-write))
                           (keep #(get-in % [:result :memory-id])))
        ;; interventions may be recorded under the intervene advance payload
        intervention-ids (or (seq interventions)
                             (some-> (get-in outputs [:intervention :memory-id]) vector))
        promotions (->> (:promotion-result outputs)
                        (keep (fn [promotion]
                                (when-let [memory-id (or (:memory-id promotion)
                                                        (:promo/artifact-id promotion))]
                                  {:memory-id memory-id
                                   :promo-id (:promo/id promotion)
                                   :step-index
                                   (some->> (:promo/id promotion)
                                            str
                                            (re-find #"/(\d+)$")
                                            second
                                            parse-long)})))
                        vec)
        promo-ids (mapv :memory-id promotions)
        student-steps (steps-of state :dispatch-student-fresh)
        student-dispatch-step
        (first (keep-indexed (fn [index step]
                               (when (= :dispatch-student-fresh (:tool step))
                                 index))
                             (:steps state)))
        pull-uses (:pull-uses outputs)
        checks
        [;; C1 — deposit attribution names the seat (packet queued; expect FAIL until built)
         (let [authors (keep #(some-> % fetch-entry :evidence/author) intervention-ids)]
           {:check :C1-deposit-attribution
            :pass? (and (seq authors) (every? #(= guide-seat %) authors))
            :evidence {:authors (vec authors) :guide-seat guide-seat}})
         ;; C2 — solver-phase deposits REVIEWED before the first student dispatch
         (let [statuses (for [id intervention-ids]
                          (let [comps (projection problem)
                                edge (some #(when (str/includes? (str %) (str id)) %) comps)]
                            (get-in edge [:edge :hx/props :attachment-status])))]
           {:check :C2-promoted-before-student
            :pass? (and (seq statuses) (every? #(= "reviewed" (str (some-> % name))) statuses))
            :evidence {:intervention-ids (vec intervention-ids) :statuses (vec statuses)}})
         ;; C3 — student eligibility includes cycle-promoted ids with provenance
         ;; (f8 finding: the eligible set lives under [:result :recall …], one
         ;;  level below where the first draft looked — instrument false-fail)
         (let [receipts (keep #(or (get-in % [:result :recall])
                                   (get-in % [:result]))
                              student-steps)
               provenanced (filter :eligible-memory-provenance receipts)
               in-scope-promotions
               (filter #(or (nil? student-dispatch-step)
                            (nil? (:step-index %))
                            (< (:step-index %) student-dispatch-step))
                       promotions)
               late-promotions
               (filter #(and (some? student-dispatch-step)
                             (some? (:step-index %))
                             (>= (:step-index %) student-dispatch-step))
                       promotions)
               promo-ids-in-scope (mapv :memory-id in-scope-promotions)
               union-ok (some (fn [r]
                                (let [elig (set (:eligible-memory-ids r))]
                                  (and (seq promo-ids-in-scope)
                                       (every? elig promo-ids-in-scope))))
                              receipts)]
           {:check :C3-eligibility-includes-promoted
            :pass? (boolean (and (seq provenanced) union-ok))
            :evidence {:receipts (count receipts) :with-provenance (count provenanced)
                       :eligible-counts (mapv #(count (:eligible-memory-ids %)) receipts)
                       :promo-ids (vec promo-ids)
                       :student-dispatch-step student-dispatch-step
                       :promo-ids-in-scope promo-ids-in-scope
                       :promo-ids-excluded-late (mapv :memory-id late-promotions)
                       :promo-ids-unparseable
                       (->> promotions
                            (filter #(nil? (:step-index %)))
                            (mapv :memory-id))}})
         ;; C4 — pull activity receipted and joinable (f8 diagnosis: outputs
         ;; carry only USE receipts; OFFER receipts — the denominator, incl.
         ;; empty-result searches — exist only in the store keyed by dispatch
         ;; id. Pass = the receipt pipeline measured this frame's pulls; zero
         ;; USES with a recorded empty OFFER is a loss-function reading, not a
         ;; pipeline failure.)
         (let [job-ids (keep #(get-in % [:result :job-id])
                             (concat (steps-of state :dispatch-solver) student-steps))
               receipts (mapcat pull-receipts job-ids)
               offers (filter #(= :memory-pull-offer (get-in % [:evidence/body :event])) receipts)
               uses (concat (or pull-uses [])
                            (filter #(= :memory-pull-use (get-in % [:evidence/body :event])) receipts))]
           {:check :C4-pull-uses-receipted
            :pass? (boolean (or (seq uses) (seq offers)))
            :evidence {:pull-offers (count offers)
                       :pull-uses (count uses)
                       :surfaced (vec (distinct (mapcat #(get-in % [:evidence/body :pull-surfaced-ids]) offers)))}})
         ;; C5 — projection completeness: every :current problem-subject memory projects
         (let [comps (projection problem)
               projected-entries (set (keep #(get-in % [:edge :hx/props :roles :entry]) comps))
               ;; sample: the intervention ids must project
               missing (remove projected-entries intervention-ids)]
           {:check :C5-projection-completeness
            :pass? (and (seq intervention-ids) (empty? missing))
            :evidence {:projected (count projected-entries) :missing (vec missing)}})
         ;; C6 — canonical subject vocabulary on this frame's evidence
         (let [subs (keep #(some-> % fetch-entry :evidence/subject :ref/type) intervention-ids)]
           {:check :C6-canonical-subject
            :pass? (and (seq subs) (every? #(= :problem %) subs))
            :evidence {:subject-types (vec (distinct subs))}})]]
    {:state-file file :problem problem
     :checks checks
     :score (str (count (filter :pass? checks)) "/" (count checks))}))

(defn write-receipt! [dir result]
  (let [receipt-path (fs/path dir "transfer-checks.edn")
        receipt {:problem-id (:problem result)
                 :state-file (:state-file result)
                 :taken-at (str (java.time.Instant/now))
                 :checks (:checks result)
                 :score (:score result)}]
    ;; One current reading per problem-state directory. Overwrite rather than
    ;; append: a rerun replaces the complete EDN value, so it cannot duplicate
    ;; a frame or leave readers to guess which entry is authoritative.
    (spit (str receipt-path) (str (pr-str receipt) "\n"))
    (str receipt-path)))

(let [dir (first *command-line-args*)]
  (when-not dir (println "usage: bb transfer_checks.bb <problem-state-dir>") (System/exit 2))
  (let [{:keys [state-file problem checks score] :as result} (run-checks dir)]
    (println "== transfer checks ==" problem "(" state-file ")")
    (doseq [{:keys [check pass? evidence]} checks]
      (println (format "%-32s %s  %s" (name check) (if pass? "PASS" "FAIL") (pr-str evidence))))
    (println "score:" score)
    (write-receipt! dir result)))
