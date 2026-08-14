#!/usr/bin/env -S clojure -M
(ns a97j07-promote (:require [clojure.string :as str]))

;; Reuse the established append/read-back machinery, replacing only the plan,
;; stable IDs, provenance, and current-store pattern preflight behavior.
(load-string (str/replace
  (slurp "holes/labs/M-zai-learning-loop/a96J02-scribe/promote.clj")
  "(apply -main *command-line-args*)" ""))
(def pns (the-ns 'promote))
(defn vr [sym value] (alter-var-root (ns-resolve pns sym) (constantly value)))

(def root "holes/labs/M-zai-learning-loop/a97J07-scribe")
(def jobs ["invoke-1785936794200-80-c77d3618" "invoke-1785936855225-82-84503052"])
(def commit-id "6f12f79c59d40b007e5d946ae198cf6b65e15737")
(def pattern "math/holomorphic-disk-api")
(def plan
  [{:slug "reflection-product-geometric-mean" :file "reflection-product-geometric-mean.md"
    :level :strategy :kind :reference :hook "Multiply reflected boundary factors to turn two arc bounds into a geometric-mean centre bound"
    :tags [:reflection :symmetry :product :function :upper :lower :semicircle :geometric :mean :two :constants :theorem :maximum-modulus :analytic :disk :complex-analysis]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :turn-evidence ["e-pull-offer-16c65e73-9f2b-4a33-bb28-9955a3ca6112" "e-pull-offer-66cb9a25-d8ae-4dd2-b28c-c3eac71f8b44"]
    :pattern pattern :attachment-why "Quantitative involution/product use of maximum modulus."}
   {:slug "maximum-modulus-frontier-api" :file "maximum-modulus-frontier-api.md"
    :level :lemma-location :kind :reference :hook "Use Complex.norm_le_of_forall_mem_frontier_norm_le after DiffContOnCl.mk_ball"
    :tags [:maximum :modulus :principle :analytic :function :disk :boundary :bound :interior :point :Mathlib :maximum-modulus :complex-analysis :DiffContOnCl :frontier]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :turn-evidence ["e-pull-offer-51bafee6-40da-45d7-a76f-e76849810fd4" "e-pull-offer-66cb9a25-d8ae-4dd2-b28c-c3eac71f8b44"]
    :pattern pattern :attachment-why "Exact frontier-bound API and ball packaging."}
   {:slug "reflection-regularity-through-negation" :file "reflection-regularity-through-negation.md"
    :level :tactic :kind :reference :hook "Transfer differentiability and closed-ball continuity through z ↦ -z and multiplication"
    :tags [:reflection :symmetry :product :function :upper :lower :semicircle :geometric :mean :two :constants :theorem :negation :DifferentiableOn :ContinuousOn :DiffContOnCl]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :turn-evidence ["e-pull-offer-16c65e73-9f2b-4a33-bb28-9955a3ca6112"]
    :pattern pattern :attachment-why "Regularity adapter for the reflected product."}])

(vr 'root root)
(vr 'base-url (or (System/getenv "FUTON_SUBSTRATE_URL") "http://127.0.0.1:7074"))
(vr 'asserter "ams-codex-1")
(vr 'session-id "M-zai-learning-loop/a97J07-scribe")
(vr 'problem-id "a97J07")
(vr 'job-ids jobs)
(vr 'commit-id commit-id)
(vr 'plan plan)
(vr 'evidence-id (fn [item] (str "e-a97j07-" (:slug item))))
(vr 'hyperedge-id (fn [item] (str "hx-a97j07-" (:slug item))))

(let [request! (var-get (ns-resolve pns 'request!))
      ensure! (var-get (ns-resolve pns 'ensure-item!))
      session-entries (var-get (ns-resolve pns 'session-entries))
      evidence-id (var-get (ns-resolve pns 'evidence-id))
      commit? (var-get (ns-resolve pns 'commit?))]
  (when-not (= 200 (:status (request! :get "/health")))
    (throw (ex-info "store health probe failed" {})))
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
        report {:date "2026-08-05" :mode (if commit? :commit :dry-run)
                :asserter "ams-codex-1" :attachment-reviewer-pending "claude-10"
                :source {:job-ids jobs :commit commit-id}
                :results results :tag-query-verification queries}]
    (when (and commit? (not-every? :verified? queries))
      (throw (ex-info "tag retrieval verification failed" {:queries queries})))
    (spit (str root "/promotion-report.edn") (str (pr-str report) "\n"))
    (prn report)))
