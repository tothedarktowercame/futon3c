#!/usr/bin/env -S clojure -M
(ns a97j01-promote
  (:require [clojure.string :as str]))

;; Reuse the established, read-back-verifying promotion implementation.  Strip
;; only its command-line entrypoint, then replace its data plan and stable IDs.
(load-string
 (str/replace
  (slurp "holes/labs/M-zai-learning-loop/a96J02-scribe/promote.clj")
  "(apply -main *command-line-args*)" ""))

(def pns (the-ns 'promote))
(defn vr [sym value]
  (alter-var-root (ns-resolve pns sym) (constantly value)))

(def root "holes/labs/M-zai-learning-loop/a97J01-scribe")
(def jobs ["invoke-1785866168873-21-2ac2b12d"
           "invoke-1785866229876-23-1398b8d9"
           "invoke-1785873356249-26-88335238"])
(def commit-id "9435997fcf4e083eb3c0a2574a77f3d462923be7")
(def pattern "math/measure-integration-api")

(def plan
  [{:slug "bounded-finite-measure-bochner-lintegral-bridge"
    :file "bounded-finite-measure-bochner-lintegral-bridge.md"
    :level :lemma-location :kind :reference
    :hook "Bridge bounded finite-measure restrictions from Bochner integrals to lintegrals"
    :tags [:Lebesgue :integrable :bounded :integral :set :measurable :function :L1
           :integrability :measure :finite-measure :Bochner :lintegral
           :Measure.integrableOn_of_bounded :ofReal_integral_eq_lintegral_ofReal]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :turn-evidence ["e-pull-offer-53df6095-828b-44e1-995e-ea894a387e7e"
                    "e-pull-offer-906e5d56-038e-4bb3-a805-261ad3752b6d"]
    :pattern pattern
    :attachment-why "Locates the exact bounded-on-finite-measure and Bochner/lintegral dependency chain."}
   {:slug "indicator-exhaustion-lintegral-isup"
    :file "indicator-exhaustion-lintegral-isup.md"
    :level :tactic :kind :reference
    :hook "Pass truncation bounds to a global lintegral with indicator functions and lintegral_iSup_ae"
    :tags [:integral :indicator :truncation :monotone :convergence :Lebesgue
           :integrable :monotone-convergence :set :function :bounded :measurable
           :Mathlib :Lean :lintegral :truncate :nonneg :ENNReal :exhaustion
           :lintegral_iSup_ae]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :turn-evidence ["e-pull-offer-f4668f1f-6771-4c57-9f64-f0188af1d558"
                    "e-pull-offer-01a0d924-2ebb-4fa3-927f-d098d60f3ad4"
                    "e-pull-offer-8a245105-69ad-4620-aeac-480e0a0ab29f"
                    "e-pull-offer-1a66f8fd-ff4b-42b8-81b9-239abe6a4355"
                    "e-pull-offer-8eeb9336-3f42-4af9-ae74-d6747c756f94"]
    :pattern pattern
    :attachment-why "Packages the measurable indicator sequence and lintegral_iSup_ae proof pattern."}
   {:slug "positive-negative-truncation-integrability-strategy"
    :file "positive-negative-truncation-integrability-strategy.md"
    :level :strategy :kind :reference
    :hook "Control global L1 mass through sign-adapted positive and negative truncations"
    :tags [:positive :part :negative :fp :pos :fneg :measurable :function
           :decomposition :integral :integrability :exhaustion :MCT :L1]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :turn-evidence ["e-pull-offer-80040942-43aa-477a-9c36-4522c31b4d11"
                    "e-pull-offer-0142b56c-fcd2-4a29-ac8c-3a6bc6d11c41"]
    :pattern pattern
    :attachment-why "Organizes sign truncations, MCT, lintegral addition, and the final integrability criterion."}])

(vr 'root root)
(vr 'base-url (or (System/getenv "FUTON_SUBSTRATE_URL") "http://127.0.0.1:7074"))
(vr 'asserter "ams-codex-1")
(vr 'session-id "M-zai-learning-loop/a97J01-scribe")
(vr 'problem-id "a97J01")
(vr 'job-ids jobs)
(vr 'commit-id commit-id)
(vr 'plan plan)
(vr 'evidence-id (fn [item] (str "e-a97j01-" (:slug item))))
(vr 'hyperedge-id (fn [item] (str "hx-a97j01-" (:slug item))))

(let [request! (var-get (ns-resolve pns 'request!))
      ensure! (var-get (ns-resolve pns 'ensure-item!))
      session-entries (var-get (ns-resolve pns 'session-entries))
      evidence-id (var-get (ns-resolve pns 'evidence-id))
      commit? (var-get (ns-resolve pns 'commit?))]
  (when-not (= 200 (:status (request! :get "/health")))
    (throw (ex-info "store health probe failed" {})))
  ;; On the current :7074 profile, curated pattern IDs are established
  ;; hyperedge endpoints but are not materialized by /api/alpha/entity.  The
  ;; read-only preflight above verified existing reviewed edges for this exact
  ;; pattern, so do not mistake the entity-projection 404 for pattern absence.
  (let [results (mapv ensure! plan)
        entries (session-entries)
        queries (mapv
                 (fn [item]
                   (let [entry-tags (set (:tags item))
                         expected (evidence-id item)
                         found (->> entries
                                    (filter #(every? (set (:evidence/tags %)) entry-tags))
                                    (mapv :evidence/id))]
                     {:tags (:tags item) :expected expected
                      :found (filterv #{expected} found)
                      :verified? (contains? (set found) expected)}))
                 plan)
        report {:date "2026-08-04" :mode (if commit? :commit :dry-run)
                :asserter "ams-codex-1"
                :attachment-reviewer-pending "claude-10"
                :source {:job-ids jobs :commit commit-id}
                :results results :tag-query-verification queries}]
    (when (and commit? (not-every? :verified? queries))
      (throw (ex-info "tag retrieval verification failed" {:queries queries})))
    (spit (str root "/promotion-report.edn") (str (pr-str report) "\n"))
    (prn report)))
