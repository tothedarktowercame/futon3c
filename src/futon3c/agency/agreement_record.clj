(ns futon3c.agency.agreement-record
  "Pure classical acceptance resolution and immutable agreement records.

   The acceptance grammar deliberately limits option tokens to decimal ids.
   This keeps ordinary replies such as `yes please` out of the command path;
   offer writers may use any string id, but only decimal ids are addressable by
   the short textual grammar in this packet."
  (:require [clojure.string :as str]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.act-stamp :as act-stamp]
            [futon3c.agency.offer-record :as offer-record])
  (:import [java.time Instant]))

(def agreement-type :agreement/record)
(def operator-stamp
  {:executor "joe" :signer "joe" :authority {:operator true}
   :executor-basis :session-bound})

(def ^:private record-keys
  #{:id :kind :agreement/offer :agreement/acceptance-evidence
    :agreement/option-id :agreement/scope :agreement/offeror
    :agreement/acceptor :agreement/at :act/stamp :act/harness})

(defn- refuse! [reason field]
  (throw (ex-info "Invalid agreement record" {:reason reason :field field})))

(defn- text? [value]
  (and (string? value) (not (str/blank? value))))

(defn- act-id? [value]
  (and (text? value) (str/starts-with? value "act:")))

(defn- parse-instant! [value reason field]
  (try (Instant/parse value)
       (catch Exception _ (refuse! reason field))))

(defn parse-acceptance
  "Parse only the classical P11 acceptance grammar, optionally prefixed by
   the mark `🈸:`. The `yes` token is case-insensitive; offer and option ids
   retain their spelling."
  [input]
  (when (string? input)
    (let [trimmed (str/trim input)
          ;; "🈸:yes" answers an agent's 🈸 paragraph in the reply-proforma
          ;; marks (Joe, 2026-10-01); the prefix is dropped before the grammar.
          without-punctuation (-> (str/replace trimmed #"[.!]$" "")
                                  (str/replace #"^🈸:\s*" "")
                                  str/trim)]
      (when-let [[_ offer-id option-after-offer option-only]
                 (re-matches #"(?i:yes)(?:\s+(?:(act:[^\s]+)(?:\s+([0-9]+))?|([0-9]+)))?"
                             without-punctuation)]
        {:offer-id offer-id
         :option-id (or option-after-offer option-only)}))))

;; ---------------------------------------------------------------------------
;; Asks written in an agent reply

(def ^:private ask-label-max 300)

(defn asks-a-question?
  "True when PARAGRAPH asks something, read by its punctuation: a `?` that
   ends a sentence (followed by whitespace or the end), outside the leading
   bracketed target, inline code and quotation marks. So `?q=` in a URL, a
   quoted question of Joe's and a question in a bracket do not count."
  [paragraph]
  (let [body (-> (str paragraph)
                 (str/replace #"^\S+\s*\([^)]*\)" "")   ; mark and bracketed target
                 (str/replace #"`[^`]*`" "")
                 (str/replace #"\"[^\"]*\"" "")
                 (str/replace #"“[^”]*”" ""))]
    (boolean (re-find #"\?(?:\s|$)" body))))

(defn reply-asks
  "The asking paragraphs of an agent reply TEXT, in order: those opening with
   the ask-action mark 🈸, and any other paragraph that asks a question by
   its punctuation (`asks-a-question?`), whatever its mark. A paragraph is a
   run of lines between blank lines; fenced code is skipped. Joe, 2026-10-05:
   claude-4 asked \"May I push …? And should stop C be the first fix?\" under
   🈳 and the \"yes\" that answered it found no offer; parse the question
   classically rather than constrain which mark may carry it."
  [text]
  (if-not (string? text)
    []
    (->> (str/split (str/replace text #"(?s)```.*?```" "") #"\n\s*\n")
         (map str/trim)
         (filter #(or (str/starts-with? % "🈸") (asks-a-question? %)))
         vec)))

(defn reply-offer-record
  "The offer an agent made by writing 🈸 paragraphs in the reply recorded as
   REPLY-EVIDENCE (a futon1b chat-turn entry), or nil when it has none. One
   option per ask, numbered from 1, so `yes` takes a single ask and `yes N`
   picks one of several. The offer is dated at the reply and keyed to it, so
   a second acceptance of the same reply finds the same offer."
  [reply-evidence agent session]
  (let [body (:evidence/body reply-evidence)
        text (or (get body :text) (get body "text"))
        asks (reply-asks text)]
    (when (seq asks)
      {:kind :offer/record :author agent :addressee "joe"
       :seat {:agent agent :session session}
       :at (str (:evidence/at reply-evidence))
       :options (vec (map-indexed
                      (fn [i ask]
                        {:option/id (str (inc i))
                         :option/label (subs ask 0 (min ask-label-max (count ask)))
                         :option/scope {:reply-evidence (str (:evidence/id reply-evidence))}})
                      asks))})))

(defn- candidate [offer option]
  {:offer-id (:id offer) :option-id (:option/id option)})

(defn resolve-acceptance
  "Resolve PARSED against `offer-record/active-offers-as-of` output. Never use
   record order or recency to break a tie."
  [visible-offers parsed]
  (let [offers (vec (:offers visible-offers))
        offer-id (:offer-id parsed)
        option-id (:option-id parsed)]
    (cond
      (and offer-id (not-any? #(= offer-id (:id %)) offers))
      {:refused {:reason :unknown-offer}}

      (and (nil? offer-id) (empty? offers))
      {:refused {:reason :no-visible-offer}}

      ;; `yes <option>` names no offer, so it is read only when the seat has
      ;; exactly one visible offer; an option id that happens to exist in
      ;; just one of several offers does not pick that offer.
      (and (nil? offer-id) option-id (< 1 (count offers)))
      {:ambiguous {:candidates (mapv (fn [offer] {:offer-id (:id offer)
                                                  :option-id option-id})
                                     offers)}}

      :else
      (let [selected-offers (if offer-id
                              (filterv #(= offer-id (:id %)) offers)
                              offers)
            pairs (vec (for [offer selected-offers
                             option (:options offer)
                             :when (or (nil? option-id)
                                       (= option-id (:option/id option)))]
                         [offer option]))]
        (cond
          (and option-id (empty? pairs))
          {:refused {:reason :unknown-option}}

          (= 1 (count pairs))
          (let [[offer option] (first pairs)]
            {:accept {:offer offer :option option}})

          :else
          {:ambiguous {:candidates (mapv (fn [[offer option]]
                                           (candidate offer option))
                                         pairs)}})))))

(defn validate!
  "Validate the closed schema-1 plain agreement shape."
  [record]
  (when-not (map? record) (refuse! :invalid-record :record))
  (when-let [key (first (remove record-keys (keys record)))]
    (refuse! :unexpected-key key))
  (when-let [key (first (remove #(contains? record %) record-keys))]
    (refuse! :missing-field key))
  (when-not (= agreement-type (:kind record))
    (refuse! :wrong-record-kind :kind))
  (when-not (act-id? (:id record)) (refuse! :invalid-act-id :id))
  (when-not (act-id? (:agreement/offer record))
    (refuse! :invalid-offer-id :agreement/offer))
  (doseq [field [:agreement/acceptance-evidence :agreement/option-id
                 :agreement/offeror :agreement/acceptor]]
    (when-not (text? (get record field)) (refuse! :missing-field field)))
  (when-not (map? (:agreement/scope record))
    (refuse! :invalid-scope :agreement/scope))
  (when-not (= "joe" (:agreement/acceptor record))
    (refuse! :acceptor-not-operator :agreement/acceptor))
  (parse-instant! (:agreement/at record) :invalid-at :agreement/at)
  (act-stamp/validate! (:act/stamp record))
  (when-not (= operator-stamp (:act/stamp record))
    (refuse! :invalid-operator-stamp :act/stamp))
  (act-harness/validate! (:act/harness record))
  record)

(defn validate-against-offer!
  "Validate AGREEMENT and prove that its copied choice matches OFFER exactly."
  [agreement offer]
  (validate! agreement)
  (offer-record/validate! offer)
  (when-not (= (:id offer) (:agreement/offer agreement))
    (refuse! :offer-mismatch :agreement/offer))
  (let [option (some #(when (= (:agreement/option-id agreement) (:option/id %)) %)
                     (:options offer))]
    (when-not option (refuse! :unknown-option :agreement/option-id))
    (when-not (= (:option/scope option) (:agreement/scope agreement))
      (refuse! :scope-mismatch :agreement/scope)))
  (when-not (= (:author offer) (:agreement/offeror agreement))
    (refuse! :offeror-mismatch :agreement/offeror))
  (let [agreement-at (parse-instant! (:agreement/at agreement)
                                     :invalid-at :agreement/at)
        offer-at (parse-instant! (:at offer) :invalid-offer-at :at)
        until (some-> (:until offer)
                      (parse-instant! :invalid-offer-until :until))]
    (when (.isBefore ^Instant agreement-at ^Instant offer-at)
      (refuse! :agreement-before-offer :agreement/at))
    (when (and until (not (.isBefore ^Instant agreement-at ^Instant until)))
      (refuse! :offer-not-visible :agreement/at)))
  agreement)

(defn record->hyperedge
  "Map a validated agreement to a schema-1 hyperedge while retaining valid time
   in props for LIST readback."
  [record]
  (let [record (validate! record)]
    {:hx/id (:id record)
     :hx/type agreement-type
     :hx/valid-time (:agreement/at record)
     :hx/endpoints [(:agreement/offer record)
                    (str "agent:" (:agreement/offeror record))
                    "agent:joe"
                    (:agreement/acceptance-evidence record)]
     :hx/props (-> record
                   (dissoc :id :kind)
                   (assoc :agreement/schema 1))}))

(defn hyperedge->record
  "Map one schema-1 agreement hyperedge to its lossless plain record."
  [hyperedge]
  (when-not (= agreement-type (:hx/type hyperedge))
    (refuse! :wrong-record-kind :hx/type))
  (when-not (= 1 (get-in hyperedge [:hx/props :agreement/schema]))
    (refuse! :unsupported-schema :agreement/schema))
  (let [props (:hx/props hyperedge)
        record (assoc (dissoc props :agreement/schema :agreement/at)
                      :id (:hx/id hyperedge)
                      :kind agreement-type
                      :agreement/at (or (:hx/valid-time hyperedge)
                                        (:agreement/at props)))]
    (validate! record)))
