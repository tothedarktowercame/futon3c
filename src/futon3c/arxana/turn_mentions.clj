(ns futon3c.arxana.turn-mentions
  "Pure exact mentions; no store reads, writes, or model inference.

  TEXT is one exact field revision, not a concatenation of operator/reply
  texts. EVIDENCE-REF is a complete external-reference record identifying
  that field revision, with SHA-256 of its UTF-8 text bytes. The settled
  packet only holds witnesses (turn_service.clj:391-444); its canonical
  turn-record :source_text or :reply_text is the text authority.

  ITEM-IDS is a collection of external item IDs. As workplan/item-view does,
  each resolves purely to the [:workplan/item 1] clusion keyed by :item/id.
  This does not assert that the clusion is present in a store."
  (:require [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as records])
  (:import [java.nio.charset StandardCharsets]
           [java.util.regex Pattern]))

(defn- text-digest [^String text]
  (digest/sha256-hex (.getBytes text StandardCharsets/UTF_8)))

(defn- token-char? [cp]
  ;; Unicode letters, marks, numbers and connector punctuation, plus hyphen:
  ;; an ID cannot be a prefix/suffix of another ID or identifier. Punctuation
  ;; such as parentheses, commas, quotes and full stops delimits a token.
  (or (Character/isLetterOrDigit (int cp))
      (contains? #{Character/NON_SPACING_MARK Character/COMBINING_SPACING_MARK
                   Character/ENCLOSING_MARK Character/CONNECTOR_PUNCTUATION}
                 (Character/getType (int cp)))
      (= cp (int \-))))

(defn- whole-token? [^String text start end]
  (and (or (zero? start) (not (token-char? (.codePointBefore text start))))
       (or (= end (.length text)) (not (token-char? (.codePointAt text end))))))

(defn turn-mentions
  "Return occurrence-ordered v2 clusions. Offsets are zero-based codepoints,
  end-exclusive (reply_target/operator-marks convention), never UTF-16 units.
  Role :mentioned denotes an exact lexical occurrence, not inferred intent.
  Duplicate inventory IDs are ignored; input inventory order is immaterial.
  A mismatching evidence pin is refused, never silently repinned."
  [text evidence-ref item-ids]
  (when-not (and (string? text) (= (:revision/digest evidence-ref) (text-digest text)))
    (throw (ex-info "text does not match evidence pin" {:error :mention/evidence-mismatch})))
  (let [reference (records/make-external-reference evidence-ref)]
    (when (and (:id evidence-ref) (not= (:id evidence-ref) (:id reference)))
      (throw (ex-info "external reference id does not match content" {:error :mention/reference-id})))
    (->> (for [item-id (sort (distinct item-ids))]
           (do
             (when-not (and (string? item-id) (not (empty? item-id)))
               (throw (ex-info "item id must be nonempty text" {:error :mention/item-id})))
             (let [item (records/make-clusion
                         {:type-revision [:workplan/item 1] :endpoints []
                          :asserted-content {:item/id item-id}})
                   matcher (.matcher (Pattern/compile (Pattern/quote item-id)) text)]
               (loop [found []]
                 (if (.find matcher)
                   (let [start (.start matcher) end (.end matcher)]
                     (recur
                      (if (whole-token? text start end)
                        (conj found
                              (records/make-clusion
                               {:type-revision [:workplan/mention 2]
                                :endpoints [{:role :item :kind :clusion :target {:clusion/id (:id item)}}
                                            {:role :evidence :kind :external-ref
                                             :target {:ref/id (:id reference)
                                                      :revision/digest (:revision/digest reference)}}]
                                :asserted-content
                                {:mention/role :mentioned
                                 :mention/range [(.codePointCount ^String text 0 start)
                                                 (.codePointCount ^String text 0 end)]
                                 :mention/quote (.substring ^String text start end)}}))
                        found)))
                   found)))))
         (apply concat)
         (sort-by (juxt #(get-in % [:asserted-content :mention/range]) :id))
         vec)))

(defn mention-valid?
  "Validate v2 shape/identity, evidence text pin and exact codepoint quote.
  A false result includes malformed/out-of-bounds ranges; no I/O is needed."
  [text record]
  (try
    (let [[start end :as span] (get-in record [:asserted-content :mention/range])
          evidence (second (:endpoints record))]
      (boolean
       (and (string? text)
            (= [:workplan/mention 2] (:type-revision record))
            (= (:id record) (records/clusion-id record))
            (= :evidence (:role evidence))
            (= (text-digest text) (get-in evidence [:target :revision/digest]))
            (vector? span) (= 2 (count span)) (integer? start) (integer? end)
            (<= 0 start end (.codePointCount ^String text 0 (.length ^String text)))
            (= (get-in record [:asserted-content :mention/quote])
               (.substring ^String text (.offsetByCodePoints ^String text 0 (int start))
                           (.offsetByCodePoints ^String text 0 (int end)))))))
    (catch Exception _ false)))
