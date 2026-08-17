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
;; Read-only: reads the saved cycle state and queries the substrate.

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
        promo-ids (->> (:promotion-result outputs)
                       (keep #(or (:memory-id %) (:promo/artifact-id %))))
        student-steps (steps-of state :dispatch-student-fresh)
        first-student-idx (some (fn [[i s]] (when (= :dispatch-student-fresh (:tool s)) i))
                                (map-indexed vector (:steps state)))
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
         (let [receipts (keep #(get-in % [:result]) student-steps)
               provenanced (filter :eligible-memory-provenance receipts)
               union-ok (some (fn [r]
                                (let [elig (set (:eligible-memory-ids r))]
                                  (and (seq promo-ids)
                                       (every? elig promo-ids))))
                              receipts)]
           {:check :C3-eligibility-includes-promoted
            :pass? (boolean (and (seq provenanced) union-ok))
            :evidence {:receipts (count receipts) :with-provenance (count provenanced)
                       :promo-ids (vec promo-ids)}})
         ;; C4 — pull uses receipted in outputs
         {:check :C4-pull-uses-receipted
          :pass? (boolean (seq pull-uses))
          :evidence {:pull-uses (count (or pull-uses []))}}
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

(let [dir (first *command-line-args*)]
  (when-not dir (println "usage: bb transfer_checks.bb <problem-state-dir>") (System/exit 2))
  (let [{:keys [state-file problem checks score]} (run-checks dir)]
    (println "== transfer checks ==" problem "(" state-file ")")
    (doseq [{:keys [check pass? evidence]} checks]
      (println (format "%-32s %s  %s" (name check) (if pass? "PASS" "FAIL") (pr-str evidence))))
    (println "score:" score)))
