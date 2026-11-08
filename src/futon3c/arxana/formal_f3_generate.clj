(ns futon3c.arxana.formal-f3-generate
  "F3c: generated explanation, explicitly synthetic contradiction, and F3b residues."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.arxana.formal-f3 :as f3]
            [futon3c.arxana.formal-f3-correspond :as f3b]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service]))

(def generated-artifact-id "formal-f3:a93J02/generated-informal")
(def synthetic-artifact-id "formal-f3:a93J02/SYNTHETIC-mutation")
(def witness-artifact-id "formal-f3:a93J02/SYNTHETIC-lean-witness")
(def synthetic-label :SYNTHETIC-negative)
;; When codex-21 authored these records: the commit time of futon3c ac85c41e5.
(def f3c-authored-at "2026-11-08T17:13:39Z")

(def generation-prompt
  (str "Explain the proof in the exact Lean artifact problems/a93J02/lean/Main.lean "
       "at commit 2dd18b2c3547897cb07f8a35084f535b9da7f4c7 for a mathematically "
       "literate reader. State the theorem, identify the alternating-series and Abel-limit "
       "steps actually used, distinguish this route from the authored dominated-convergence "
       "argument, and do not present the explanation as authored prose or mathematical authority."))

(def model
  {:provider "OpenAI" :family "GPT-5/Codex"
   :serving-snapshot :not-exposed})
(def generation-configuration
  {:mode :single-pass :temperature :not-exposed :reasoning-effort :not-exposed})

(defn resource-bytes [name]
  (java.nio.file.Files/readAllBytes
   (.toPath (io/file "test/resources/arxana/formal_arxana" name))))

(defn read-kernel-run []
  (edn/read-string (slurp (io/file "test/resources/arxana/formal_arxana"
                                  "a93J02-SYNTHETIC-kernel-run.edn"))))

(defn- dispatch-record [cfg prefix operation principal record]
  (service/dispatch
   cfg {:protocol/version "arxana.v1" :message/id (str "formal-f3c:" prefix ":" (:id record))
        :operation operation :principal/id principal :body {:record record}}))

(defn- scope [revision id document quote]
  (let [utf16-start (.indexOf ^String document ^String quote)
        _ (when (neg? utf16-start)
            (throw (ex-info "scope quote absent from manifestation" {:scope/id id :quote quote})))
        utf16-end (+ utf16-start (.length ^String quote))
        start (.codePointCount ^String document 0 utf16-start)
        end (.codePointCount ^String document 0 utf16-end)]
    {:artifact/revision-id revision :scope/id id :scope/kind :generated-statement
     :scope/quote quote :codepoint-range [start end]}))

(defn generation-run [lean-revision generated-revision]
  (let [output (String. ^bytes (resource-bytes "a93J02-generated-informal.md") "UTF-8")]
    (records/make-clusion
     {:type-revision [:generation-run 1]
      :endpoints [{:role :input :kind :artifact-revision
                   :target {:artifact/revision-id lean-revision}}
                  {:role :output :kind :artifact-revision
                   :target {:artifact/revision-id generated-revision}}]
      :asserted-content {:model model :prompt generation-prompt
                         :configuration generation-configuration
                         :output output}})))

(defn mutation-record [generated-revision synthetic-revision]
  (records/make-clusion
   {:type-revision [:synthetic-mutation 1]
    :endpoints [{:role :source :kind :artifact-revision
                 :target {:artifact/revision-id generated-revision}}
                {:role :mutation :kind :artifact-revision
                 :target {:artifact/revision-id synthetic-revision}}]
    :asserted-content {:label synthetic-label
                       :diff [{:from "converge to `log 2`" :to "converge to `log 3`"}
                              {:from "is `log 2`" :to "is `log 3`"}]}}))

(defn explanation-correspondence [revision]
  (let [document (String. ^bytes (resource-bytes "a93J02-generated-informal.md") "UTF-8")]
    (records/make-clusion
     {:type-revision [:correspondence 1]
      :endpoints [{:role :source :kind :manifestation-scope
                 :target (scope revision "generated/conclusion" document
                                "converge to `log 2` as `n` tends to infinity.")}
                {:role :target :kind :entity
                 :target {:declaration/id (f3/declaration-id "alternating_harmonic_eq_log_two")
                          :declaration/name "alternating_harmonic_eq_log_two"}}]
      :asserted-content {:direction :informal-to-formal
                         :relation/kind :generated-explanation}})))

(defn synthetic-correspondence [revision]
  (let [document (String. ^bytes (resource-bytes "a93J02-SYNTHETIC-mutation.md") "UTF-8")]
    (records/make-clusion
     {:type-revision [:correspondence 1]
      :endpoints [{:role :source :kind :manifestation-scope
                 :target (assoc (scope revision "SYNTHETIC/log-three" document
                                       "converge to `log 3` as `n` tends to infinity.")
                                :fixture/label synthetic-label)}
                {:role :target :kind :entity
                 :target {:declaration/id (f3/declaration-id "alternating_harmonic_eq_log_two")
                          :declaration/name "alternating_harmonic_eq_log_two"}}]
      :asserted-content {:direction :informal-to-formal
                         :relation/kind :SYNTHETIC-statement-contradiction
                         :fixture/label synthetic-label}})))

(defn- model-candidate-records [claim]
  [(records/make-attestation
    {:subject-clusion-id (:id claim) :producer "codex-21/F3c-generation"
     :basis :model :standing :unverified :coverage :complete
     :method {:id "formal-f3/generated-correspondence" :version 1}
     :content {:model model :candidate? true} :source-refs []})
   (records/make-assessment
    {:subject-clusion-id (:id claim) :verdict :candidate :actor "codex-21"
     :policy {:id "formal-f3/model-output-is-candidate" :version 1}
     :reason "Generated informalisation is model output, not acceptance."
     :time f3c-authored-at :basis :model})])

(defn contradiction-assessment [claim kernel-attestation-id]
  (records/make-assessment
   {:subject-clusion-id (:id claim) :verdict :rejected :actor "codex-21"
    :policy {:id "formal-f3/SYNTHETIC-negative-fixture" :version 1}
    :reason "The deliberately mutated log 3 statement is contradicted by the cited Lean kernel witness."
    :time f3c-authored-at :basis :fixture-author :standing :contradicted
    :fixture/label synthetic-label :kernel/attestation-id kernel-attestation-id
    :source-refs [{:attestation/id kernel-attestation-id}]}))

(defn residue-records [plan]
  (let [ambiguous (for [{:keys [step/id claim evidence]} (:records plan)
                        :when (= :ambiguous (get-in evidence [:content :resolution/state]))]
                    (records/make-clusion
                     {:type-revision [:resolution-observation 1]
                      :endpoints [{:role :subject :kind :clusion :target {:clusion/id (:id claim)}}]
                      :asserted-content {:resolution/state :ambiguous :step/id id}}))
        unaligned (for [name (:unaligned-declarations plan)]
                    (records/make-clusion
                     {:type-revision [:formalization-coverage 1]
                      :endpoints [{:role :declaration :kind :entity
                                   :target {:declaration/id (f3/declaration-id name)
                                            :declaration/name name}}]
                      :asserted-content {:coverage/state :unaligned
                                         :reason "No judged informal proof-step alignment."}}))]
    {:ambiguous (vec (distinct ambiguous)) :unaligned (vec unaligned)}))

(defn materialise!
  [cfg {:keys [lean-revision informal-revision alignment]}]
  (let [plans [{:artifact/id generated-artifact-id :media-type "text/markdown; charset=utf-8"
                :bytes (resource-bytes "a93J02-generated-informal.md")}
               {:artifact/id synthetic-artifact-id
                :media-type "text/markdown; charset=utf-8; fixture=SYNTHETIC-negative"
                :bytes (resource-bytes "a93J02-SYNTHETIC-mutation.md")}
               {:artifact/id witness-artifact-id
                :media-type "text/x-lean; charset=utf-8; fixture=SYNTHETIC-negative"
                :bytes (resource-bytes "a93J02-SYNTHETIC-lean-witness.lean")}]
        ingested (f3/ingest-manifestations! cfg plans)
        revisions (into {} (map (juxt :artifact/id :artifact/revision-id) ingested))
        generated-revision (revisions generated-artifact-id)
        synthetic-revision (revisions synthetic-artifact-id)
        witness-revision (revisions witness-artifact-id)
        generation (generation-run lean-revision generated-revision)
        mutation (mutation-record generated-revision synthetic-revision)
        generated-claim (explanation-correspondence generated-revision)
        [generated-evidence generated-assessment] (model-candidate-records generated-claim)
        synthetic-claim (synthetic-correspondence synthetic-revision)
        kernel (f3/append-kernel-run! cfg witness-revision (read-kernel-run))
        contradiction (contradiction-assessment synthetic-claim (get-in kernel [:attestation :id]))
        f3b-plan (f3b/plan alignment informal-revision)
        residues (residue-records f3b-plan)
        claims (concat [generation mutation generated-claim synthetic-claim]
                       (:ambiguous residues) (:unaligned residues))
        claim-writes (mapv #(dispatch-record cfg "claim" :clusion/assert
                                             "formal-f3c/materialiser" %) claims)
        evidence-write (dispatch-record cfg "generated-evidence" :evidence/append
                                        "formal-f3c/materialiser" generated-evidence)
        candidate-write (dispatch-record cfg "generated-candidate" :assessment/append
                                         "formal-f3c/materialiser" generated-assessment)
        contradiction-write (dispatch-record cfg "contradiction" :assessment/append
                                             "formal-f3c/materialiser" contradiction)]
    {:ingested ingested :revisions revisions :generation generation :mutation mutation
     :generated-claim generated-claim :generated-evidence generated-evidence
     :generated-assessment generated-assessment :synthetic-claim synthetic-claim
     :kernel kernel :contradiction contradiction :residues residues
     :writes (vec (concat claim-writes [evidence-write candidate-write contradiction-write]))}))
