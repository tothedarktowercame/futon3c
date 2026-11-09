(ns futon3c.arxana.codedocs
  "Pinned code-definition scopes for the code↔docs demonstration."
  (:require [clojure.string :as str]
            [futon3c.arxana.correspond :as corr]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service]))

(def finder-method {:id "codedocs/clojure-definition" :version 1})

(def definition-forms
  ["defn-" "defn" "def" "defmulti" "defmethod" "defprotocol" "defrecord" "deftype"])

(def definition-pattern
  (re-pattern
   (str "(?m)^[\\t ]*\\((" (str/join "|" definition-forms) ")\\s+"
        "(?:(?:\\^(?::)?[^\\s(){}]+|\\^\\{[^\\n]*\\})\\s+)*"
        "([^\\s\\n()]+)")))

(defn- codepoint-offset [^String s char-index]
  (.codePointCount s 0 char-index))

(defn- form-end
  "Return the UTF-16 index just after the balanced top-level form at START."
  [^String text start]
  (loop [i start depth 0 string? false escape? false comment? false]
    (when (>= i (.length text))
      (throw (ex-info "unterminated definition form" {:start start})))
    (let [ch (.charAt text i)]
      (cond
        comment? (recur (inc i) depth string? false (not= ch \newline))
        (and string? escape?) (recur (inc i) depth true false false)
        (and string? (= ch \\)) (recur (inc i) depth true true false)
        (= ch \u0022) (recur (inc i) depth (not string?) false false)
        string? (recur (inc i) depth true false false)
        (= ch \;) (recur (inc i) depth false false true)
        (= ch \() (recur (inc i) (inc depth) false false false)
        (= ch \)) (if (= depth 1) (inc i)
                       (recur (inc i) (dec depth) false false false))
        :else (recur (inc i) depth false false false)))))

(defn definitions
  "Parse selected top-level definitions. Ranges are half-open Unicode codepoints."
  [^String text wanted]
  (let [m (re-matcher definition-pattern text)]
    (loop [found {}]
      (if (.find m)
        (let [symbol (.group m 2)]
          (recur
           (if (contains? wanted symbol)
             (let [start (.start m) end (form-end text start)]
               (assoc found symbol
                      {:symbol/name symbol :definition/form (keyword (.group m 1))
                       :codepoint-range [(codepoint-offset text start)
                                         (codepoint-offset text end)]}))
             found)))
        (do
          (when-let [missing (seq (remove (set (keys found)) wanted))]
            (throw (ex-info "fixture definitions missing" {:symbols (vec missing)})))
          found)))))

(defn scope-record [revision-id {:keys [symbol/name definition/form]}]
  (records/make-clusion
   {:type-revision [:code-definition 1] :endpoints []
    :asserted-content {:artifact/revision-id revision-id
                       :scope/kind :code-definition
                       :symbol/name name :definition/form form}}))

(defn range-attestation [scope revision-id range]
  (records/make-attestation
   {:subject-clusion-id (:id scope) :producer "futon3c.arxana.codedocs"
    :basis :parser :standing :verified :coverage :complete
    :method finder-method
    :content {:artifact/revision-id revision-id :codepoint-range range}
    :source-refs [{:artifact/revision-id revision-id}]}))

(defn- request [mid operation body]
  {:protocol/version "arxana.v1" :message/id mid :operation operation
   :principal/id "codedocs/materializer" :body body})

(defn- call [cfg mid operation body]
  (service/dispatch cfg (request mid operation body)))

(defn- register-position! [repo artifact-id revision-id scope range]
  (let [subject-id (:id scope)
        source (corr/register-manifestation!
                repo {:manifestation/authority "formal-arxana/codedocs"
                      :artifact/family-id artifact-id
                      :manifestation/role-key (str revision-id "/" subject-id "/source")
                      :manifestation/label (get-in scope [:asserted-content :symbol/name])
                      :manifestation/media-role :text/x-clojure
                      :manifestation/role :authored :manifestation/subject subject-id})
        target (corr/register-manifestation!
                repo {:manifestation/authority "formal-arxana/codedocs"
                      :artifact/family-id artifact-id
                      :manifestation/role-key (str revision-id "/" subject-id "/identity")
                      :manifestation/label (get-in scope [:asserted-content :symbol/name])
                      :manifestation/media-role :application/x-arxana-scope
                      :manifestation/role :authored :manifestation/subject subject-id})]
    (corr/assert-correspondence!
     repo {:source {:manifestation/id (:manifestation/id source)
                    :artifact/revision-id revision-id
                    :locator {:kind :codepoint-range :start (first range) :end (second range)}}
           :target {:manifestation/id (:manifestation/id target)
                    :artifact/revision-id revision-id
                    :locator {:kind :quote-context
                              :value (get-in scope [:asserted-content :symbol/name])}}
           :direction :source-to-target :cardinality :one-to-one
           :method [:exact-range] :verification :verified
           :evidence [{:type :parser :method finder-method}]
           :producer {:tool "futon3c.arxana.codedocs" :version "1"}})))

(defn ingest-file!
  "Ingest historical then current text and store selected definition scopes."
  [cfg repo {:keys [artifact-id historical current symbols]}]
  (loop [versions [historical current] expected :absent results []]
    (if-let [{:keys [sha text]} (first versions)]
      (let [ingest (call cfg (str "codedocs:ingest:" artifact-id ":" sha)
                         :revision/ingest
                         {:artifact/id artifact-id :bytes text
                          :media-type "text/x-clojure; charset=utf-8"
                          :expected/revision-id expected})
            revision-id (get-in ingest [:ok :artifact/revision-id])
            parsed (definitions text (set symbols))
            scopes (into (sorted-map)
                         (for [[symbol d] parsed
                               :let [scope (scope-record revision-id d)
                                     att (range-attestation scope revision-id (:codepoint-range d))]]
                           (do
                             (call cfg (str "codedocs:scope:" (:id scope))
                                   :clusion/assert {:record scope})
                             (call cfg (str "codedocs:range:" (:id att))
                                   :evidence/append {:record att})
                             (register-position! repo artifact-id revision-id scope
                                                 (:codepoint-range d))
                             [symbol {:scope scope :attestation att
                                      :codepoint-range (:codepoint-range d)}])))]
        (when-not revision-id
          (throw (ex-info "code revision ingest failed" {:response ingest})))
        (recur (next versions) revision-id
               (conj results {:sha sha :revision-id revision-id :scopes scopes})))
      results)))
