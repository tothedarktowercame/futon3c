(ns futon3c.arxana.formal-f3-correspond
  "F3b: attributable a93J02 step-to-declaration correspondence records."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.formal-f3 :as f3]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service]))

(def alignment-sha256
  "87c7d08668f200301dfed8a7f7bee85d3571545226576d31cfa8870cf074b1db")
(def joe-evidence-id "emacs-8fdea78f5fab37bcf3fc65aa4aded28e")
(def joe-turn-id "turn-YoF9El")
(def joe-recorded-at "2026-11-08T01:27:29Z")
(def joe-quote
  "So, I looked at it, not deeply, but I agree with the judge's readings.  The point at the end is crucial: \"a methodological divergence\".  It looks like the mark8 reader was really reaching to find some connections that weren't there.  Note also that there was never a request in the formalisation work that the formal proof *should* follow the informal proof, so divergence is expected.  This puts some feedback pressure on mark8, but that's a separate issue.")

(def mark8-method {:id "mark8/a93J02-align" :version "r2-judged"})
(def judge-method {:id "mark8/judge" :version "r2-judged"})
(def joe-policy {:id "formal-arxana/operator-review" :version 1})

(defn read-alignment [path]
  (let [bytes (java.nio.file.Files/readAllBytes
               (.toPath (io/file path)))
        actual (digest/sha256-hex bytes)]
    (when-not (= alignment-sha256 actual)
      (throw (ex-info "judged alignment digest mismatch"
                      {:expected alignment-sha256 :actual actual :path path})))
    (json/parse-string (String. ^bytes bytes java.nio.charset.StandardCharsets/UTF_8)
                       true)))

(defn- source-endpoint [informal-revision {:keys [edge step]}]
  {:role :source :kind :manifestation-scope
   :target {:artifact/revision-id informal-revision
            :scope/id (str "mark8:a93J02/" edge)
            :scope/kind :informal-proof-step
            :scope/quote step}})

(defn- target-endpoint [declaration]
  {:role :target :kind :entity
   :target {:declaration/id (f3/declaration-id declaration)
            :declaration/name declaration}})

(defn correspondence
  ([informal-revision step candidate]
   (correspondence informal-revision step candidate :step-to-declaration))
  ([informal-revision step candidate relation-kind]
   (records/make-clusion
    {:type-revision [:correspondence 1]
     :endpoints [(source-endpoint informal-revision step)
                 (target-endpoint (:decl candidate))]
     :asserted-content {:direction :informal-to-formal
                        :relation/kind relation-kind}})))

(defn- resolution-state [step]
  (if (:judged step) :unique :ambiguous))

(defn model-attestation [claim step candidate]
  (records/make-attestation
   {:subject-clusion-id (:id claim)
    :producer "mark8"
    :basis :model
    :standing (if (= (:decl candidate) (:judged step)) :verified :disputed)
    :coverage (if (:judged step) :complete :partial)
    :method mark8-method
    :content {:step/id (:edge step)
              :candidate/declaration (:decl candidate)
              :candidate/score (:score candidate)
              :candidate/shared (:shared candidate)
              :candidate/set (mapv :decl (:candidates step))
              :retrieval/lemma (:lemma step)
              :judge/verdict (:judged step)
              :judge/reason (:judge_reason step)
              :resolution/state (resolution-state step)}
    :source-refs [{:alignment/sha256 alignment-sha256
                   :step/id (:edge step)}]}))

(defn model-assessment [claim step candidate]
  (records/make-assessment
   {:subject-clusion-id (:id claim)
    :verdict (if (= (:decl candidate) (:judged step)) :candidate :rejected)
    :actor "mark8-judge"
    :policy judge-method
    :reason (:judge_reason step)
    :time "r2-judged"
    :basis :model
    :candidate/set (mapv :decl (:candidates step))
    :source-refs [{:alignment/sha256 alignment-sha256 :step/id (:edge step)}]}))

(defn joe-assessment [claim step candidate]
  (records/make-assessment
   {:subject-clusion-id (:id claim)
    :verdict (if (= (:decl candidate) (:judged step)) :accepted :rejected)
    :actor "joe"
    :policy joe-policy
    :reason joe-quote
    :time joe-recorded-at
    :basis :human
    :quote joe-quote
    :source-refs [{:turn/id joe-turn-id :evidence/id joe-evidence-id}]}))

(defn- divergence-records [informal-revision step]
  (let [candidate {:decl "tendsto_tsum_powerSeries_nhdsWithin_lt"
                   :score nil :shared []}
        claim (correspondence informal-revision step candidate
                              :methodological-divergence)
        evidence (records/make-attestation
                  {:subject-clusion-id (:id claim)
                   :producer "formal-f3/divergence-adapter"
                   :basis :human :standing :disputed :coverage :partial
                   :method {:id "formal-f3/method-comparison" :version 1}
                   :content {:informal/method :dominated-convergence
                             :formal/method :abel-limit-theorem
                             :resolution/state :unique
                             :outcome :methodological-divergence
                             :contradiction? false}
                   :source-refs [{:alignment/sha256 alignment-sha256
                                  :step/id (:edge step)}]})
        assessment (records/make-assessment
                    {:subject-clusion-id (:id claim) :verdict :accepted
                     :actor "joe" :policy joe-policy :reason joe-quote
                     :time joe-recorded-at :basis :human :quote joe-quote
                     :source-refs [{:turn/id joe-turn-id
                                    :evidence/id joe-evidence-id}]})]
    {:step/id (:edge step) :candidate candidate :claim claim
     :evidence evidence :model-assessment nil :human-assessment assessment
     :divergence? true}))

(defn plan [alignment informal-revision]
  (let [steps (:non_hole_steps alignment)
        ordinary
        (mapcat
         (fn [step]
           (map (fn [candidate]
                  (let [claim (correspondence informal-revision step candidate)]
                    {:step/id (:edge step) :candidate candidate :claim claim
                     :evidence (model-attestation claim step candidate)
                     :model-assessment (model-assessment claim step candidate)
                     :human-assessment (joe-assessment claim step candidate)
                     :divergence? false}))
                (:candidates step)))
         steps)
        dct-step (first (filter #(= "e7" (:edge %)) steps))]
    {:records (vec (concat ordinary [(divergence-records informal-revision dct-step)]))
     :steps (mapv :edge steps)
     :unaligned-declarations (vec (:unaligned_decls alignment))}))

(defn- dispatch-record [cfg prefix operation record]
  (service/dispatch
   cfg {:protocol/version "arxana.v1"
        :message/id (str "formal-f3b:" prefix ":" (:id record))
        :operation operation :principal/id "formal-f3b/materialiser"
        :body {:record record}}))

(defn materialise! [cfg plan]
  (mapv
   (fn [{:keys [claim evidence model-assessment human-assessment] :as row}]
     (assoc row
            :writes
            (cond-> [(dispatch-record cfg "claim" :clusion/assert claim)
                     (dispatch-record cfg "evidence" :evidence/append evidence)]
              model-assessment
              (conj (dispatch-record cfg "model-assessment"
                                     :assessment/append model-assessment))
              human-assessment
              (conj (dispatch-record cfg "human-assessment"
                                     :assessment/append human-assessment)))))
   (:records plan)))
