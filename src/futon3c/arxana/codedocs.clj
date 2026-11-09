(ns futon3c.arxana.codedocs
  "Pinned code-definition scopes for the code↔docs demonstration."
  (:require [clojure.string :as str]
            [futon3c.arxana.correspond :as corr]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service]))

(def finder-method {:id "codedocs/clojure-definition" :version 1})
(def doc-finder-method {:id "codedocs/def-doc-bullet" :version 1})
(def code-doc-match-method {:id "codedocs/source-path+symbol" :version 1})
(def lean-finder-method {:id "codedocs/lean-declaration" :version 1})
(def lean-citation-method {:id "codedocs/lean-backtick-citation" :version 1})

(def definition-forms
  ["defn-" "defn" "def" "defmulti" "defmethod" "defprotocol" "defrecord" "deftype"])

(def definition-pattern
  (re-pattern
   (str "(?m)^[\\t ]*\\((" (str/join "|" definition-forms) ")\\s+"
        "(?:(?:\\^(?::)?[^\\s(){}]+|\\^\\{[^\\n]*\\})\\s+)*"
        "([^\\s\\n()]+)")))

(defn- codepoint-offset [^String s char-index]
  (.codePointCount s 0 char-index))

(defn- property-value [text property]
  (some-> (re-find (re-pattern (str "(?m)^:" (java.util.regex.Pattern/quote property)
                                    ":\\s*(.*?)\\s*$")) text)
          second))

(defn doc-entries
  "Parse selected def-doc bullets. Ranges end at the next bullet or heading."
  [^String text wanted]
  (let [entry-id (property-value text "ENTRY_ID")
        source-path (property-value text "SOURCE_PATH")
        boundary (re-pattern "(?m)^(?:- |\\*+ )")
        starts (->> (re-seq #"(?m)^- ([^:\n]+):" text)
                    (map (fn [[whole symbol]]
                           (let [char-start (.indexOf text whole)]
                             {:symbol/name symbol :char-start char-start})))
                    ;; indexOf alone is ambiguous for duplicate identical headers.
                    (reduce (fn [{:keys [from rows]} {:keys [symbol/name]}]
                              (let [needle (str "- " name ":")
                                    at (.indexOf text needle from)]
                                {:from (+ at (count needle))
                                 :rows (conj rows {:symbol/name name :char-start at})}))
                            {:from 0 :rows []})
                    :rows)]
    (when-not (and entry-id source-path)
      (throw (ex-info "def-doc drawer is incomplete"
                      {:entry/id entry-id :source/path source-path})))
    (into (sorted-map)
          (for [{:keys [symbol/name char-start]} starts
                :when (contains? wanted name)
                :let [m (re-matcher boundary text)
                      _ (.region m (inc char-start) (.length text))
                      char-end (if (.find m) (.start m) (.length text))]]
            [name {:entry/id entry-id :source/path source-path :symbol/name name
                   :codepoint-range [(codepoint-offset text char-start)
                                     (codepoint-offset text char-end)]}]))))

(defn- comment-end
  "UTF-16 index after a nested Lean block comment beginning at START."
  [^String text start]
  (loop [i (+ start 2) depth 1]
    (when (>= i (.length text))
      (throw (ex-info "unterminated Lean block comment" {:start start})))
    (cond
      (and (< (inc i) (.length text)) (= \/ (.charAt text i)) (= \- (.charAt text (inc i))))
      (recur (+ i 2) (inc depth))
      (and (< (inc i) (.length text)) (= \- (.charAt text i)) (= \/ (.charAt text (inc i))))
      (if (= depth 1) (+ i 2) (recur (+ i 2) (dec depth)))
      :else (recur (inc i) depth))))

(defn- lean-mask
  "Mask strings and comments without changing UTF-16 positions; retain doc spans."
  [^String text]
  (let [masked (char-array text)
        blank! (fn [start end]
                 (doseq [i (range start end)]
                   (when-not (= \newline (aget masked i)) (aset masked i \space))))]
    (loop [i 0 docs [] lines []]
      (if (>= i (.length text))
        {:text (String. masked) :doc-comments docs :line-comments lines}
        (let [next? #(< (inc i) (.length text))
              a (.charAt text i)
              b (when (next?) (.charAt text (inc i)))]
          (cond
            (= a \u0022)
            (let [end (loop [j (inc i) escaped? false]
                        (when (>= j (.length text))
                          (throw (ex-info "unterminated Lean string" {:start i})))
                        (let [ch (.charAt text j)]
                          (cond escaped? (recur (inc j) false)
                                (= ch \\) (recur (inc j) true)
                                (= ch \u0022) (inc j)
                                :else (recur (inc j) false))))]
              (blank! i end) (recur end docs lines))

            (and (= a \-) (= b \-))
            (let [nl (.indexOf text "\n" (+ i 2)) end (if (neg? nl) (.length text) nl)]
              (blank! i end) (recur end docs (conj lines {:char-range [i end]})))

            (and (= a \/) (= b \-))
            (let [end (comment-end text i)
                  doc? (and (< (+ i 2) (.length text)) (= \- (.charAt text (+ i 2))))]
              (blank! i end)
              (recur end (cond-> docs doc? (conj {:char-range [i end]})) lines))

            :else (recur (inc i) docs lines)))))))

(def lean-declaration-pattern
  #"(?m)^(theorem|lemma|def|abbrev|structure|inductive|instance)\s+([^\s:(]+)")

(defn lean-declarations
  "Return top-level, namespace-qualified Lean declarations with attached doc comments."
  [^String text cited-symbols]
  (let [{masked :text docs :doc-comments line-comments :line-comments} (lean-mask text)
        namespace-pattern #"(?m)^(namespace)\s+([^\s]+)|^(end)(?:\s+[^\s]+)?\s*$"
        events (sort-by :start
                        (concat
                         (let [m (re-matcher namespace-pattern masked)]
                           (loop [xs []]
                             (if (.find m)
                               (recur (conj xs {:kind (if (.group m 1) :namespace :end)
                                                :name (.group m 2) :start (.start m)}))
                               xs)))
                         (let [m (re-matcher lean-declaration-pattern masked)]
                           (loop [xs []]
                             (if (.find m)
                               (recur (conj xs {:kind :declaration :keyword (.group m 1)
                                                :name (.group m 2) :start (.start m)}))
                               xs)))))
        attached-doc (fn [start]
                       (last (filter (fn [{[_ end] :char-range}]
                                       (and (<= end start)
                                            (str/blank? (.substring text end start))))
                                     docs)))
        ;; A body ends at the next declaration, or at the doc comment attached
        ;; to it; doc comments inside a body (on constructors) do not end it.
        boundaries (sort (map (fn [{:keys [kind start]}]
                                (if (= kind :declaration)
                                  (or (some-> (attached-doc start) :char-range first) start)
                                  start))
                              events))
        next-boundary (fn [start]
                        (or (some #(when (> % start) %) boundaries) (.length text)))]
    (:declarations
     (reduce
      (fn [{:keys [namespaces declarations] :as state} event]
        (case (:kind event)
          :namespace (assoc state :namespaces (conj namespaces (:name event)))
          :end (assoc state :namespaces (vec (butlast namespaces)))
          :declaration
          (let [start (:start event)
                attached (attached-doc start)
                raw-end (next-boundary start)
                end (loop [i raw-end]
                      (if (and (> i start) (Character/isWhitespace (.charAt text (dec i))))
                        (recur (dec i)) i))
                fqn (str/join "." (concat namespaces [(:name event)]))
                ;; Citing text, as legacy `arxana-browser-code--lean-declarations`:
                ;; the attached doc comment, then the `--` line comments in the
                ;; body up to the next declaration or doc comment. Code, strings
                ;; and `/- -/` blocks do not cite. One citation per symbol: its
                ;; first token in that order.
                regions (concat (when attached [(:char-range attached)])
                                (keep (fn [{[a b] :char-range}]
                                        (when (and (>= a start) (<= b raw-end)) [a b]))
                                      line-comments))
                citations
                (vec
                 (for [symbol (sort cited-symbols)
                       :let [token (str "`" symbol "`")
                             hit (some (fn [[a b]]
                                         (let [local (.indexOf (.substring text a b) token)]
                                           (when-not (neg? local) (+ a local))))
                                       regions)]
                       :when hit]
                   {:symbol/name symbol :token token
                    :codepoint-range [(codepoint-offset text hit)
                                      (codepoint-offset text (+ hit (count token)))]}))]
            (assoc state :declarations
                   (conj declarations
                         {:declaration/fqn fqn :declaration/name (:name event)
                          :declaration/keyword (keyword (:keyword event))
                          :codepoint-range [(codepoint-offset text start)
                                            (codepoint-offset text end)]
                          :doc-comment-range (when attached
                                               (mapv #(codepoint-offset text %)
                                                     (:char-range attached)))
                          :citations citations})))))
      {:namespaces [] :declarations []} events))))

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

(defn scope-membership [revision-id scope kind]
  (records/make-clusion
   {:type-revision [:scope-in-revision 1]
    :endpoints [{:role :revision :kind :artifact-revision
                 :target {:artifact/revision-id revision-id}}
                {:role :scope :kind :clusion :target {:clusion/id (:id scope)}}]
    :asserted-content {:scope/kind kind}}))

(defn range-attestation [scope revision-id range]
  (records/make-attestation
   {:subject-clusion-id (:id scope) :producer "futon3c.arxana.codedocs"
    :basis :parser :standing :verified :coverage :complete
    :method finder-method
    :content {:artifact/revision-id revision-id :codepoint-range range}
    :source-refs [{:artifact/revision-id revision-id}]}))

(defn doc-scope-record [revision-id {:keys [entry/id symbol/name]}]
  (records/make-clusion
   {:type-revision [:doc-entry 1] :endpoints []
    :asserted-content {:artifact/revision-id revision-id
                       :entry/id id :symbol/name name}}))

(defn doc-range-attestation
  [scope revision-id {:keys [entry/id source/path symbol/name codepoint-range]}]
  (records/make-attestation
   {:subject-clusion-id (:id scope) :producer "futon3c.arxana.codedocs"
    :basis :parser :standing :verified :coverage :complete
    :method doc-finder-method
    :content {:artifact/revision-id revision-id :codepoint-range codepoint-range
              :entry/id id :source/path path :symbol/name name}
    :source-refs [{:artifact/revision-id revision-id}]}))

(defn code-doc-correspondence [code-scope doc-scope]
  (records/make-clusion
   {:type-revision [:correspondence 1]
    :endpoints [{:role :source :kind :manifestation-scope
                 :target {:entity/id (:id code-scope)}}
                {:role :target :kind :entity
                 :target {:entity/id (:id doc-scope)}}]
    :asserted-content {:direction :source-to-target
                       :relation/kind :code-documents}}))

(defn code-doc-attestation [correspondence code-scope doc-scope source-path]
  (records/make-attestation
   {:subject-clusion-id (:id correspondence)
    :producer "futon3c.arxana.codedocs" :basis :parser
    :standing :verified :coverage :complete :method code-doc-match-method
    :content {:source/path source-path
              :symbol/name (get-in code-scope [:asserted-content :symbol/name])
              :source/scope-id (:id code-scope) :target/scope-id (:id doc-scope)}
    :source-refs [{:entity/id (:id code-scope)} {:entity/id (:id doc-scope)}]}))

(defn lean-scope-record [revision-id {:keys [declaration/fqn]}]
  (records/make-clusion
   {:type-revision [:lean-declaration 1] :endpoints []
    :asserted-content {:artifact/revision-id revision-id :declaration/fqn fqn}}))

(defn lean-range-attestation [scope revision-id declaration]
  (records/make-attestation
   {:subject-clusion-id (:id scope) :producer "futon3c.arxana.codedocs"
    :basis :parser :standing :unverified :coverage :complete
    :method lean-finder-method
    :content {:artifact/revision-id revision-id
              :declaration/fqn (:declaration/fqn declaration)
              :declaration/keyword (:declaration/keyword declaration)
              :codepoint-range (:codepoint-range declaration)
              :doc-comment-range (:doc-comment-range declaration)}
    :source-refs [{:artifact/revision-id revision-id}]}))

(defn lean-code-correspondence [lean-scope code-scope]
  (records/make-clusion
   {:type-revision [:correspondence 1]
    :endpoints [{:role :source :kind :manifestation-scope
                 :target {:entity/id (:id lean-scope)}}
                {:role :target :kind :entity :target {:entity/id (:id code-scope)}}]
    :asserted-content {:direction :source-to-target :relation/kind :cites-code}}))

(defn lean-citation-attestation
  [correspondence revision-id lean-scope code-scope citation]
  (records/make-attestation
   {:subject-clusion-id (:id correspondence) :producer "futon3c.arxana.codedocs"
    :basis :parser :standing :unverified :coverage :complete
    :method lean-citation-method
    :content {:artifact/revision-id revision-id :token (:token citation)
              :token/codepoint-range (:codepoint-range citation)
              :symbol/name (:symbol/name citation)
              :source/scope-id (:id lean-scope) :target/scope-id (:id code-scope)}
    :source-refs [{:artifact/revision-id revision-id}
                  {:entity/id (:id lean-scope)} {:entity/id (:id code-scope)}]}))

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
                                     membership (scope-membership revision-id scope :code-definition)
                                     att (range-attestation scope revision-id (:codepoint-range d))]]
                           (do
                             (call cfg (str "codedocs:scope:" (:id scope))
                                   :clusion/assert {:record scope})
                             (call cfg (str "codedocs:scope-membership:" (:id membership))
                                   :clusion/assert {:record membership})
                             (call cfg (str "codedocs:range:" (:id att))
                                   :evidence/append {:record att})
                             (register-position! repo artifact-id revision-id scope
                                                 (:codepoint-range d))
                             [symbol {:scope scope :membership membership :attestation att
                                      :codepoint-range (:codepoint-range d)}])))]
        (when-not revision-id
          (throw (ex-info "code revision ingest failed" {:response ingest})))
        (recur (next versions) revision-id
               (conj results {:sha sha :revision-id revision-id :scopes scopes})))
      results)))

(defn ingest-doc!
  "Ingest one pinned def-doc stub and store its selected entry scopes."
  [cfg {:keys [artifact-id sha text symbols]}]
  (let [ingest (call cfg (str "codedocs:doc-ingest:" artifact-id ":" sha)
                     :revision/ingest
                     {:artifact/id artifact-id :bytes text
                      :media-type "text/org; charset=utf-8"
                      :expected/revision-id :absent})
        revision-id (get-in ingest [:ok :artifact/revision-id])]
    (when-not revision-id
      (throw (ex-info "doc revision ingest failed" {:response ingest})))
    {:sha sha :revision-id revision-id
     :source-path (property-value text "SOURCE_PATH")
     :scopes
     (into (sorted-map)
           (for [[symbol entry] (doc-entries text (set symbols))
                 :let [scope (doc-scope-record revision-id entry)
                       att (doc-range-attestation scope revision-id entry)]]
             (do (call cfg (str "codedocs:doc-scope:" (:id scope))
                       :clusion/assert {:record scope})
                 (call cfg (str "codedocs:doc-range:" (:id att))
                       :evidence/append {:record att})
                 [symbol {:scope scope :attestation att
                          :codepoint-range (:codepoint-range entry)}])))}))

(defn link-code-docs!
  "Link exact SOURCE_PATH + symbol matches. CODE-BY-PATH holds current scopes."
  [cfg code-by-path doc-results]
  (vec
   (for [{:keys [source-path scopes]} doc-results
         [symbol {:keys [scope]}] scopes
         :let [code-scope (get-in code-by-path [source-path symbol :scope])]
         :when code-scope
         :let [correspondence (code-doc-correspondence code-scope scope)
               att (code-doc-attestation correspondence code-scope scope source-path)]]
     (do (call cfg (str "codedocs:correspondence:" (:id correspondence))
               :clusion/assert {:record correspondence})
         (call cfg (str "codedocs:correspondence-evidence:" (:id att))
               :evidence/append {:record att})
         {:symbol symbol :code-scope code-scope :doc-scope scope
          :correspondence correspondence :attestation att}))))

(defn ingest-lean!
  "Ingest Lean bytes and store only declarations with exact backticked citations."
  [cfg {:keys [artifact-id sha text symbols code-scopes]}]
  (let [ingest (call cfg (str "codedocs:lean-ingest:" artifact-id ":" sha)
                     :revision/ingest
                     {:artifact/id artifact-id :bytes text
                      :media-type "text/x-lean; charset=utf-8"
                      :expected/revision-id :absent})
        revision-id (get-in ingest [:ok :artifact/revision-id])]
    (when-not revision-id
      (throw (ex-info "Lean revision ingest failed" {:response ingest})))
    (let [declarations (filterv (comp seq :citations)
                                (lean-declarations text (set symbols)))
          scopes
          (into (sorted-map)
                (for [declaration declarations
                      :let [scope (lean-scope-record revision-id declaration)
                            att (lean-range-attestation scope revision-id declaration)]]
                  (do (call cfg (str "codedocs:lean-scope:" (:id scope))
                            :clusion/assert {:record scope})
                      (call cfg (str "codedocs:lean-range:" (:id att))
                            :evidence/append {:record att})
                      [(:declaration/fqn declaration)
                       {:scope scope :attestation att :declaration declaration}])))
          citations
          (vec
           (for [[_ {:keys [scope declaration]}] scopes
                 citation (:citations declaration)
                 :let [code-scope (get code-scopes (:symbol/name citation))
                       correspondence (lean-code-correspondence scope code-scope)
                       att (lean-citation-attestation correspondence revision-id scope
                                                      code-scope citation)]]
             (do
               (when-not code-scope
                 (throw (ex-info "citation has no current code scope" citation)))
               (call cfg (str "codedocs:lean-citation:" (:id correspondence))
                     :clusion/assert {:record correspondence})
               (call cfg (str "codedocs:lean-citation-evidence:" (:id att))
                     :evidence/append {:record att})
               {:fqn (:declaration/fqn declaration) :symbol (:symbol/name citation)
                :lean-scope scope :code-scope code-scope
                :correspondence correspondence :attestation att})))]
      {:sha sha :revision-id revision-id :scopes scopes :citations citations})))
