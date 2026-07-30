#!/usr/bin/env bb
;; Promote S4 scribe-pass-1 drafts into the store (owner pass, claude-6,
;; 2026-07-28). Shapes mirror futon3c.peripheral.memory-write
;; (memory-hyperedge / entry-body) and scripts/wire_math_memory_patterns.clj
;; (review-entry warrant form) — read before writing, I-4.
;; RAM caution (standing, post-OOM): health probe first, ~150ms pacing,
;; deterministic ids (idempotent re-run), read-back verify, stop on 503.
;; Only :memory/assert edges are created (the one registered type);
;; frontier's non-assert edge proposals remain body DATA; cross-memory
;; connectivity rides shared endpoints (e-dfea2de9 et al. as subjects).

(require '[babashka.http-client :as http]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def root "/home/joe/code/futon3c/holes/labs/M-codex-sorry-loop")
(def base "http://127.0.0.1:7073")
(def commit? (= "--commit" (first *command-line-args*)))
(def pace-ms 150)
(def session-id "M-codex-sorry-loop/pilot-1")
(def mission-id "M-codex-sorry-loop")
(def distill-id "e-codexroll-019f9b12-t007")
(def reviewer "claude-6")
(def warrant "operator-delegated: in-use evaluation (proof-in-pudding; owner draft review claude-6 2026-07-28; Joe's standing direction)")

(def pattern-for
  {"derive-integrable-from-nonzero-bochner-integral" "math-formalization/tactic-algebra-interference"
   "prove-eLpNorm-translation-through-the-underlying-lintegral" "math-formalization/tactic-algebra-interference"
   "reduce-probability-kernel-L2-contraction-to-young" "math/proof-architecture"
   "specify-invariant-measure-when-lintegral-translation-stalls" "math-formalization/tactic-algebra-interference"
   "descend-through-eLpNorm-rpow-before-lintegral-rewrite" "math-formalization/tactic-algebra-interference"
   "state-the-absolute-integral-normalization-explicitly" "math-formalization/tactic-algebra-interference"
   "probe-the-high-risk-integral-assembly-before-polishing-young-leaves" "math/missing-dependency-protocol"
   "integral-minkowski-eLpNorm-bochner" "math/missing-dependency-protocol"})

(def extra-memory-subjects
  {"probe-the-high-risk-integral-assembly-before-polishing-young-leaves"
   ["e-9751e537-f5b7-4c40-a857-0c0b699b93a2"
    "e-dfea2de9-8979-4f8f-9343-caabb48487e6"
    "e-bb16ffa8-e6da-4956-81c3-011e312d5302"]
   "integral-minkowski-eLpNorm-bochner"
   ["e-dfea2de9-8979-4f8f-9343-caabb48487e6"]})

(def witness-for
  (fn [lane] (if (#{:solve-lane :arc-lane} lane)
               :independently-witnessed :self-asserted)))

(defn hook-for [d]
  (let [b (:body d)]
    (or (:rule b) (:problem-class b) (:statement b) (:trigger b)
        (get-in b [:scope :context]) (:name d))))

(defn frontier-updates [d]
  (if (= "integral-minkowski-eLpNorm-bochner" (:name d))
    (-> d
        (assoc :status :dormant-bypassed)
        (update :body assoc
                :demand-recomputed
                {:at "2026-07-28"
                 :confirmed-unblocks-remaining 0
                 :note "Young main theorem DISCHARGED via route (a) ce77d41 (frontier bypassed); a96A04 heat contraction unblocked via now-unconditional corollary. 6 further census rows cited but unverified as Minkowski-gated; demand may return with new blocked targets."
                 :status :dormant-bypassed}))
    d))

(defn ids-for [name]
  {:entry (str "e-codexpilot-" name)
   :hx (str "hx-codexpilot-" name)
   :review (str "e-review-codexpilot-" name)})

(defn entry-for [d ids]
  (let [b (:body d)]
    {:evidence/id (:entry ids)
     :evidence/type :memory
     :evidence/claim-type :assert
     :evidence/author reviewer
     :evidence/session-id session-id
     :evidence/subject (first (:subjects d))
     :evidence/tags [:memory :mathematics :codex-pilot (:lane d)]
     :evidence/body
     {:name (:name d)
      :hook (hook-for d)
      :kind (if (= :frontier (:level d)) :reference :feedback)
      :body b
      :why (or (:problem-class b) (:trigger b) (get-in b [:scope :context]))
      :how-to-apply (or (:rule b)
                        (some->> (:proof-shape b) (str/join " → "))
                        (some->> (:observed-trajectory b) (str/join " → ")))
      :scribe-author "codex-5"
      :draft-source "holes/labs/M-codex-sorry-loop/scribe-pass-1-drafts.edn"
      :runner-model :codex
      :promotion {:reviewed-by reviewer :verdict :approve :warrant warrant}}}))

(defn hyperedge-for [d ids]
  (let [pattern (pattern-for (:name d))
        subject-ids (vec (concat (mapv (comp str :ref/id) (:subjects d))
                                 (extra-memory-subjects (:name d) [])))
        endpoints (->> (concat [(:entry ids)] subject-ids [pattern distill-id
                                                          session-id mission-id])
                       (remove str/blank?) distinct vec)]
    {:hx/id (:hx ids)
     :hx/type :memory/assert
     :hx/endpoints endpoints
     :hx/props {:roles {:entry (:entry ids)
                        :subjects (vec (distinct (conj subject-ids pattern)))
                        :distills [distill-id]
                        :session session-id
                        :mission mission-id
                        :patterns [pattern]}
                :kind (if (= :frontier (:level d)) :reference :feedback)
                :name (:name d)
                :hook (hook-for d)
                :volatile? (= :frontier (:level d))
                :state :current
                :domain :mathematics
                :witness-status (witness-for (:lane d))
                :attachment-status :proposed}}))

(defn review-for [d ids]
  {:evidence/id (:review ids)
   :evidence/subject {:ref/type :memory :ref/id (:entry ids)}
   :evidence/type :memory
   :evidence/claim-type :observation
   :evidence/author reviewer
   :evidence/session-id "M-codex-sorry-loop/operator-review"
   :evidence/body {:review/event :memory-attachment-review
                   :review/memory-id (:entry ids)
                   :review/pattern-ids [(pattern-for (:name d))]
                   :review/verdict :approve
                   :review/witness-status (witness-for (:lane d))
                   :review/provenance {:kind :operator-delegated-in-use-evaluation
                                       :reviewer reviewer
                                       :date "2026-07-28"
                                       :warrant warrant}
                   :review/policy-verdict :pudding-delegated}
   :evidence/tags [:memory :memory/review :mathematics :codex-pilot]})

(defn exists? [path]
  (= 200 (:status (http/get (str base path)
                            {:headers {"accept" "application/edn"}
                             :throw false :timeout 15000}))))

(defn post! [path payload]
  (let [r (http/post (str base path)
                     {:headers {"content-type" "application/edn"
                                "accept" "application/edn"
                                "x-penholder" "api"}
                      :body (pr-str payload)
                      :throw false :timeout 30000})]
    (when (= 503 (:status r))
      (throw (ex-info "store busy — stopping, do not hammer" {:path path})))
    (when-not (<= 200 (:status r) 299)
      (throw (ex-info "write failed" {:path path :status (:status r)
                                      :body (subs (str (:body r)) 0
                                                  (min 200 (count (str (:body r)))))})))
    (Thread/sleep pace-ms)
    true))

(let [drafts (mapv frontier-updates
                   (edn/read-string (slurp (str root "/scribe-pass-1-drafts.edn"))))
      _ (assert (= 8 (count drafts)))
      _ (when-not (exists? "/api/alpha/evidence?limit=1")
          (throw (ex-info "store health probe failed" {})))
      results
      (vec
       (for [d drafts
             :let [ids (ids-for (:name d))
                   already? (exists? (str "/api/alpha/evidence/" (:entry ids)))]]
         (if already?
           {:name (:name d) :status :skipped-existing}
           (if-not commit?
             {:name (:name d) :status :dry-run
              :pattern (pattern-for (:name d))
              :witness (witness-for (:lane d))
              :entry-id (:entry ids)}
             (do (post! "/api/alpha/evidence" (entry-for d ids))
                 (post! "/api/alpha/hyperedge" (hyperedge-for d ids))
                 (post! "/api/alpha/evidence" (review-for d ids))
                 (let [entry-ok (exists? (str "/api/alpha/evidence/" (:entry ids)))
                       review-ok (exists? (str "/api/alpha/evidence/" (:review ids)))]
                   {:name (:name d)
                    :status (if (and entry-ok review-ok) :promoted :VERIFY-FAILED)
                    :entry-id (:entry ids)}))))))]
  (spit (str root "/promotion-pass-1-report.edn")
        (pr-str {:at "2026-07-28" :mode (if commit? :commit :dry-run)
                 :results results}))
  (doseq [r results] (println (:status r) "|" (:name r) "|" (:pattern r "")))
  (println "TOTAL:" (frequencies (map :status results))))
