#!/usr/bin/env -S clojure -M
(ns a97j02-promote (:require [clojure.string :as str]))

;; Reuse the established append/read-back machinery, replacing only its plan,
;; stable IDs, provenance, and current-store pattern preflight behavior.
(load-string (str/replace
  (slurp "holes/labs/M-zai-learning-loop/a96J02-scribe/promote.clj")
  "(apply -main *command-line-args*)" ""))
(def pns (the-ns 'promote))
(defn vr [sym value] (alter-var-root (ns-resolve pns sym) (constantly value)))

(def root "holes/labs/M-zai-learning-loop/a97J02-scribe")
(def jobs ["invoke-1785874642221-31-4f1e14a4"
           "invoke-1785874703950-33-68989489"
           "invoke-1785878598130-35-9f6c865c"])
(def commit-id "c3763609b5753bdea1a3d1172aed6f6bd55f4655")
(def pattern "math/measure-integration-api")
(def plan
  [{:slug "closed-cthickening-measure-convergence-api"
    :file "closed-cthickening-measure-convergence-api.md"
    :level :lemma-location :kind :reference
    :hook "Use the packaged closed-cthickening measure limit instead of rebuilding continuity from above"
    :tags [:measure :continuity :limit :convergence :Lebesgue :closed :compact
           :cthickening :neighborhood :volume :finite-measure
           :tendsto_measure_cthickening_of_isClosed]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :turn-evidence ["e-pull-offer-2805c07a-92bf-4ccd-a2b1-7b2829280c0d"
                    "e-pull-offer-169bf575-f754-41e7-8de3-abf918bb26dd"
                    "e-pull-offer-2ae33621-a9f2-4c74-9c12-ef61dd36a1b9"
                    "e-pull-offer-6d5e013b-36dd-4926-a06a-723ef2fcad3f"]
    :pattern pattern
    :attachment-why "Locates the exact closed-neighborhood measure convergence dependency and finite-measure premise."}
   {:slug "measure-finite-union-closed-grid-cells"
    :file "measure-finite-union-closed-grid-cells.md"
    :level :tactic :kind :reference
    :hook "Sum measures of closed grid cells by proving endpoint overlaps are a.e. disjoint"
    :tags [:grid :partition :interval :union :measure :closed :cells :AEDisjoint
           :measure_biUnion_finset₀ :null-boundary :singleton :Real.volume_Icc]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :turn-evidence ["e-pull-offer-5b8d797e-13a2-4724-ad36-f28b7a963954"]
    :pattern pattern
    :attachment-why "Packages the a.e.-disjoint finite-union calculation for null-boundary overlaps."}
   {:slug "select-grid-cell-with-nat-ceil"
    :file "select-grid-cell-with-nat-ceil.md"
    :level :tactic :kind :reference
    :hook "Select a covering closed grid cell with max 1 and Nat.ceil under an N ≥ 1 guard"
    :tags [:grid :cell :coverage :Nat.ceil :ceiling :atTop :eventually
           :N-positive :interval :partition :squeeze :endpoint]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :turn-evidence ["e-pull-offer-5b8d797e-13a2-4724-ad36-f28b7a963954"]
    :pattern pattern
    :attachment-why "Supplies constructive grid coverage and records the necessary positive-index guard."}])

(vr 'root root)
(vr 'base-url (or (System/getenv "FUTON_SUBSTRATE_URL") "http://127.0.0.1:7074"))
(vr 'asserter "ams-codex-1")
(vr 'session-id "M-zai-learning-loop/a97J02-scribe")
(vr 'problem-id "a97J02")
(vr 'job-ids jobs)
(vr 'commit-id commit-id)
(vr 'plan plan)
(vr 'evidence-id (fn [item] (str "e-a97j02-" (:slug item))))
(vr 'hyperedge-id (fn [item] (str "hx-a97j02-" (:slug item))))

(let [request! (var-get (ns-resolve pns 'request!))
      ensure! (var-get (ns-resolve pns 'ensure-item!))
      session-entries (var-get (ns-resolve pns 'session-entries))
      evidence-id (var-get (ns-resolve pns 'evidence-id))
      commit? (var-get (ns-resolve pns 'commit?))]
  (when-not (= 200 (:status (request! :get "/health")))
    (throw (ex-info "store health probe failed" {})))
  ;; :7074 has reviewed edges ending at this curated pattern, while its entity
  ;; projection returns 404. Existing reviewed hyperedges were the read-only
  ;; preflight; attachment writes remain proposed.
  (let [results (mapv ensure! plan)
        entries (session-entries)
        queries (mapv
                 (fn [item]
                   (let [wanted (set (:tags item)) expected (evidence-id item)
                         found (->> entries
                                    (filter #(every? (set (:evidence/tags %)) wanted))
                                    (mapv :evidence/id))]
                     {:tags (:tags item) :expected expected
                      :found (filterv #{expected} found)
                      :verified? (contains? (set found) expected)})) plan)
        report {:date "2026-08-04" :mode (if commit? :commit :dry-run)
                :asserter "ams-codex-1"
                :attachment-reviewer-pending "claude-10"
                :source {:job-ids jobs :commit commit-id}
                :results results :tag-query-verification queries}]
    (when (and commit? (not-every? :verified? queries))
      (throw (ex-info "tag retrieval verification failed" {:queries queries})))
    (spit (str root "/promotion-report.edn") (str (pr-str report) "\n"))
    (prn report)))
