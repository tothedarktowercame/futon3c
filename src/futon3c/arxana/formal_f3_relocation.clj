(ns futon3c.arxana.formal-f3-relocation
  "F3d relocation of immutable correspondence claims across authored-prose edits."
  (:require [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f1-relocation :as f1r]
            [futon3c.arxana.service :as service])
  (:import [java.nio.charset StandardCharsets]))

(def anchors
  {"e1" "∫₀¹ 1/(1+x) dx = log(2)."
   "e2" "The geometric series gives, for x ∈ [0,1): 1/(1+x) = Σₙ₌₀^∞ (-1)ⁿxⁿ."
   "e3" "∫₀¹ (-1)ᵏxᵏ dx = (-1)ᵏ/(k+1)."
   "e4" "So ∫₀¹ Sₙ(x) dx = Σₖ₌₀ⁿ (-1)ᵏ/(k+1)."
   "e5" "For each x ∈ [0,1), Sₙ(x) → 1/(1+x) (geometric series)."
   "e6" "The constant function 2 is integrable on [0,1]."
   "e7" "∫₀¹ Sₙ(x) dx → ∫₀¹ 1/(1+x) dx."
   "e8" "The Abel summation theorem gives the same result"
   "e9" "Therefore Σₙ₌₀^∞ (-1)ⁿ/(n+1) = log 2."})

(defn- cp-range [^String text ^String quote]
  (let [s (.indexOf text quote)]
    (when (neg? s) (throw (ex-info "F3 anchor absent" {:quote quote})))
    [(.codePointCount text 0 s) (.codePointCount text 0 (+ s (.length quote)))]))

(defn exported [text revision-id step->claims]
  {:markdown text :markdown/revision-id revision-id
   :manifest {:turns
              [{:turn/id "a93J02"
                :interpretations
                (vec (for [[step claim] step->claims
                           :let [q (anchors step)]]
                       {:clusion/id (:id claim) :step/id step
                        :range (cp-range text q)}))}]}})

(defn edit-successor [exported]
  (let [text (:markdown exported)
        duplicate (anchors "e5") missing (anchors "e6")
        edited (str "Editorial note: notation normalised; mathematics unchanged.\n\n"
                    (.replace text missing "") "\n\nRepeated explanatory quotation:\n" duplicate "\n")]
    {:markdown edited :bytes (.getBytes edited StandardCharsets/UTF_8)
     :revision-id (artifact/revision-id-for-bytes (.getBytes edited StandardCharsets/UTF_8))}))

(defn ingest-successor! [cfg source-rid edited]
  (service/dispatch cfg {:protocol/version "arxana.v1"
                         :message/id (str "f3d:successor:" (:revision-id edited))
                         :operation :revision/ingest :principal/id "formal-f3-relocator"
                         :body {:artifact/id "formal-f3:a93J02/authored-informal"
                                :bytes (:markdown edited)
                                :media-type "text/markdown; charset=utf-8"
                                :expected/revision-id source-rid}}))

(defn relocate! [cfg provider exported target-rid]
  (let [registry (f1r/register-strategies exported)
        result (f1r/relocate provider registry (:markdown/revision-id exported)
                             target-rid target-rid target-rid)
        writes (f1r/append-relocations! cfg result)]
    {:registry registry :result result :writes writes}))
