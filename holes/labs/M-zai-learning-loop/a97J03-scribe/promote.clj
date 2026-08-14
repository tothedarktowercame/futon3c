#!/usr/bin/env -S clojure -M
(ns a97j02-promote (:require [clojure.string :as str]))

;; Reuse the established append/read-back machinery, replacing only its plan,
;; stable IDs, provenance, and current-store pattern preflight behavior.
(load-string (str/replace
  (slurp "holes/labs/M-zai-learning-loop/a96J02-scribe/promote.clj")
  "(apply -main *command-line-args*)" ""))
(def pns (the-ns 'promote))
(defn vr [sym value] (alter-var-root (ns-resolve pns sym) (constantly value)))

(def root "holes/labs/M-zai-learning-loop/a97J03-scribe")
(def jobs ["invoke-1785931097145-40-484817cb" "invoke-1785931218106-42-c36f29f8" "invoke-1785931526692-44-a2147f71"])
(def commit-id "1f93f8652da95ff5b41f407b30dc1743d0ab1a2b")
(def pattern "math/measure-integration-api")
(def plan
  [{:slug "lp-translation-continuity-through-pairing" :file "lp-translation-continuity-through-pairing.md"
    :level :tactic :kind :reference :hook "Make convolution continuous through measure-preserving Lp translations and lpPairing"
    :tags [:translation :continuity :Lp :symmetric-difference :convolution :Holder :Mathlib :API :integral :product :Lq :conjugate :exponent :bounded :bilinear :lpPairing]
    :confidence {:n-instances 2 :scope :documented-cross-problem-reuse}
    :turn-evidence ["e-pull-offer-d11cb533-7ec2-46c4-b35e-523366cc5580" "e-pull-offer-36021f8f-b0f5-431e-8fa7-a00bdaadd0c2" "e-pull-offer-aee4baa0-25af-406c-815c-83b53f648039"]
    :pattern pattern
    :attachment-why "Packages the measure-preserving Lp translation and Hölder pairing bridge."}
   {:slug "compact-support-density-pairing-cocompact" :file "compact-support-density-pairing-cocompact.md"
    :level :strategy :kind :reference :hook "Transfer compactly supported convolution vanishing through uniform lpPairing error bounds"
    :tags [:convolution :Lp :Lq :Holder :inequality :continuous :vanishes :infinity :translation :compact-support :density :cocompact :e-codexpilot-prove-Holder-convolution-vanishes-at-infinity-by-compact-support-density]
    :confidence {:n-instances 2 :scope :documented-cross-problem-reuse}
    :turn-evidence ["e-pull-offer-bfe34a9f-2c35-470b-b0ae-63997cccdd1e" "e-pull-offer-5992e3a0-5267-43e0-b220-e875f34b9ce4"]
    :pattern pattern
    :attachment-why "Packages compact-support density and uniform bilinear error control."}
   {:slug "cocompact-limit-to-atTop-atBot" :file "cocompact-limit-to-atTop-atBot.md"
    :level :lemma-location :kind :reference :hook "Restrict a cocompact real limit to atTop and atBot"
    :tags [:cocompact :atTop :atBot :limit :C0 :e-codexpilot-prove-Holder-convolution-vanishes-at-infinity-by-compact-support-density]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :turn-evidence ["e-pull-offer-68cd0f4f-bc45-4b98-822c-becfe1eb3086" "e-pull-offer-6bb607c6-c4d5-43ca-92f2-d214d2d7dd5c"]
    :pattern pattern
    :attachment-why "Connects C0-style convergence to both one-sided real filters."}])

(vr 'root root)
(vr 'base-url (or (System/getenv "FUTON_SUBSTRATE_URL") "http://127.0.0.1:7074"))
(vr 'asserter "ams-codex-1")
(vr 'session-id "M-zai-learning-loop/a97J03-scribe")
(vr 'problem-id "a97J03")
(vr 'job-ids jobs)
(vr 'commit-id commit-id)
(vr 'plan plan)
(vr 'evidence-id (fn [item] (str "e-a97j03-" (:slug item))))
(vr 'hyperedge-id (fn [item] (str "hx-a97j03-" (:slug item))))

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
        report {:date "2026-08-05" :mode (if commit? :commit :dry-run)
                :asserter "ams-codex-1"
                :attachment-reviewer-pending "claude-10"
                :source {:job-ids jobs :commit commit-id}
                :results results :tag-query-verification queries}]
    (when (and commit? (not-every? :verified? queries))
      (throw (ex-info "tag retrieval verification failed" {:queries queries})))
    (spit (str root "/promotion-report.edn") (str (pr-str report) "\n"))
    (prn report)))
