(ns futon3c.arxana.formal-f1
  "F1a materialisation of settled Xiang turns into Formal Arxana records."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service]))

(def fixture-turns ["turn-4qzbnC" "turn-pHxXNI" "turn-PdGx4U" "turn-iWVzvT"])

(def golden-targets
  {"turn-4qzbnC" {:ref/store :agency :ref/id "claude-19"}
   "turn-pHxXNI" {:ref/store :mission :ref/id "M-formal-arxana"}
   "turn-PdGx4U" {:ref/store :mission :ref/id "M-formal-arxana"}
   "turn-iWVzvT" {:ref/store :mission :ref/id "M-formal-arxana"}})

(defn- read-json [path]
  (json/parse-string (slurp path) true))

(defn- fragment-locator [turn-id sentence-id]
  (if (#{"turn-4qzbnC" "turn-pHxXNI"} turn-id)
    {:sentence 1}
    {:fragment sentence-id}))

(defn- interpretation [turn-id sentence-id intent]
  (records/make-clusion
   {:type-revision [:interpretation 1]
    :endpoints [{:role :source :kind :manifestation-scope
                 :target {:artifact/revision-id (str turn-id "@settled")
                          :locator (fragment-locator turn-id sentence-id)}}
                {:role :intent :kind :entity :target {:intent/id (keyword intent)}}
                {:role :target :kind :external-ref :target (golden-targets turn-id)}]
    :asserted-content {:reading/state :candidate}}))

(defn- attestation [analysis claim fragment mark-records]
  (records/make-attestation
   {:subject-clusion-id (:id claim)
    :producer (:labeller analysis)
    :basis :model :standing :unverified :coverage :complete
    :method {:id "xiang/read" :version 1
             :parameters {:interpretation-version (:interpretation_version analysis)
                          :vocabulary-version (:vocabulary_version analysis)}}
    :content {:rationale (:rationale fragment) :target (:target fragment)}
    :source-refs (into [{:evidence/id (:evidence_id analysis)
                         :source/sha256 (:source_sha256 analysis)
                         :range [(:start fragment) (:end fragment)]}]
                       (map (fn [mark] {:mark-occurrence/id (:id mark)}) mark-records))}))

(defn- resolution [claim state]
  (records/make-clusion
   {:type-revision [:resolution-observation 1]
    :endpoints [{:role :subject :kind :clusion :target {:clusion/id (:id claim)}}]
    :asserted-content {:resolution/state state}}))

(def mark-pattern
  (re-pattern "([㊥㊭㊣㊟🈖🈯㊢㊩㊬🈹🈡🈳🈲🈸㊯㊝㊮🈕🈰🈝🈘㊫])(:?)"))

(defn mark-occurrences
  "Return one revision-pinned entity per mark occurrence. Matcher offsets are
  converted from JVM UTF-16 indexes to Unicode codepoint offsets."
  [turn-id text]
  (let [matcher (re-matcher mark-pattern text)]
    (loop [out []]
      (if (.find matcher)
        (let [utf16-start (.start matcher 1)
              start (.codePointCount ^String text 0 utf16-start)
              glyph (.group matcher 1)
              colon (.group matcher 2)]
          (recur (conj out
                       (records/make-mark-occurrence
                        {:glyph glyph :form (if (seq colon) :colon :plain)
                         :artifact/revision-id (str turn-id "@settled")
                         :locator {:codepoint-range [start (inc start)]}}))))
        out))))

(defn- external-reference [turn-id kind record-id digest]
  (records/make-external-reference
   {:ref/kind kind :store/id :xiang-turn-store
    :store/endpoint "file://~/.emacs-graph/session-turn-analysis"
    :record/id record-id :schema/authority "xiang/v3"
    :valid-time "2026-11-08" :revision/digest digest
    :turn/id turn-id}))

(def operator-judgement
  "Joe's judgement of turn-iWVzvT's reading, verbatim from its evidence record."
  {:turn/id "turn-0ozt4x"
   :evidence/id "emacs-5d1f629e9f6fcd876a973fad309feff9"
   :time "2026-11-08T01:18:45Z"
   :quote (str "s2 and s3 are good; s1 is plausible, but 🈲 (constrain) would "
               "probably be better, because 🈸 is typically used for informational "
               "requests, where is this \"please\" isn't really an ask but a boundary")})

(defn- assessment [claim verdict reason]
  (records/make-assessment
   {:subject-clusion-id (:id claim) :verdict verdict :actor "joe"
    :policy {:id "operator-judgement" :revision 1}
    :reason reason :quote (:quote operator-judgement)
    :source-refs [(select-keys operator-judgement [:turn/id :evidence/id])]
    :time (:time operator-judgement)}))

(defn plan-turn
  "Pure deterministic plan from one settled Xiang turn and its analysis."
  [turn-id turn analysis]
  (let [mark-records (mark-occurrences turn-id (:source_text turn))
        fragments (for [sentence (:sentences analysis)
                        fragment (:fragments sentence)]
                    [(:id sentence) fragment])
        claims (mapv (fn [[sid f]] (interpretation turn-id sid (:intent f))) fragments)
        attestations (mapv (fn [[_ f] c] (attestation analysis c f mark-records))
                           fragments claims)
        resolutions (mapv #(resolution % :unique) claims)]
    {:turn/id turn-id
     :artifact/id (str "xiang:" turn-id)
     :logical-revision/id (str turn-id "@settled")
     :bytes (.getBytes ^String (:source_text turn) "UTF-8")
     :marks mark-records
     :references [(external-reference turn-id :turn turn-id (:source_sha256 analysis))
                  (external-reference turn-id :interpreter (:labeller analysis)
                                      (:request_sha256 analysis))]
     :claims claims :attestations attestations :resolutions resolutions}))

(defn plan-fixtures
  ([] (plan-fixtures (str (System/getProperty "user.home")
                          "/.emacs-graph/session-turn-analysis")))
  ([root]
   (mapv (fn [turn-id]
           (plan-turn turn-id
                      (read-json (io/file root (str turn-id ".json")))
                      (read-json (io/file root (str turn-id ".json.analysis.json")))))
         fixture-turns)))

(defn add-operator-judgement [plans]
  (mapv
   (fn [plan]
     (if-not (= "turn-iWVzvT" (:turn/id plan))
       plan
       (let [[s1 s2 s3] (:claims plan)
             successor (interpretation "turn-iWVzvT" "s1" "constrain")
             supersedes (records/make-clusion
                         {:type-revision [:supersedes 1]
                          :endpoints [{:role :successor :kind :clusion
                                       :target {:clusion/id (:id successor)}}
                                      {:role :predecessor :kind :clusion
                                       :target {:clusion/id (:id s1)}}]
                          :asserted-content {}})
             reason "the 'please' sets a boundary rather than asking for an action"]
         (-> plan
             (update :claims into [successor supersedes])
             (update :attestations conj
                     (records/make-attestation
                      {:subject-clusion-id (:id successor) :producer "joe"
                       :basis :human :standing :verified :coverage :complete
                       :method {:id "operator/judgement" :version 1}
                       :content {:reason reason}
                       :source-refs [{:turn/id "turn-0ozt4x"
                                      :evidence/id "emacs-5d1f629e9f6fcd876a973fad309feff9"}]}))
             (assoc :assessments [(assessment s1 :rejected reason)
                                  (assessment s2 :accepted "s2 is good")
                                  (assessment s3 :accepted "s3 is good")])
             (assoc :resolutions (into [(resolution s1 :ambiguous)
                                        (resolution successor :ambiguous)]
                                       (rest (:resolutions plan))))))))
   plans))

(defn- request [mid op record]
  {:protocol/version "arxana.v1" :message/id mid :operation op
   :principal/id "formal-f1-materialiser" :body {:record record}})

(defn materialize!
  "Execute a plan through the public JVM service. Equal replay is a no-op.
  The byte revision and logical settled identity are deliberately separate."
  [cfg plans]
  (mapv
   (fn [plan]
     (let [rid (artifact/revision-id-for-bytes (:bytes plan))
           ingest-msg {:protocol/version "arxana.v1"
                       :message/id (str "f1a:ingest:" (:turn/id plan))
                       :operation :revision/ingest
                       :principal/id "formal-f1-materialiser"
                       :body {:artifact/id (:artifact/id plan) :bytes (:bytes plan)
                              :media-type "text/plain; charset=utf-8"
                              :expected/revision-id (or (:ingested/revision plan) :absent)}}
           ingest (service/dispatch cfg ingest-msg)
           ingest (if (and (:error ingest)
                           (= rid (get-in ingest [:error :error/detail :current/revision-id])))
                    {:ok {:artifact/revision-id rid :created? false}}
                    ingest)
           writes (for [[op records] [[:clusion/assert (:claims plan)]
                                      [:clusion/assert (:resolutions plan)]
                                      [:evidence/append (:attestations plan)]
                                      [:assessment/append (:assessments plan)]
                                      [:mark-occurrence/assert (:marks plan)]
                                      [:external-reference/assert (:references plan)]]
                        record records]
                    (service/dispatch cfg (request (str "f1a:" (:id record)) op record)))]
       {:turn/id (:turn/id plan) :ingest ingest :writes (vec writes)}))
   plans))
