(ns futon3c.arxana.draft-edition
  "Pinned external spans and proposed support, built in memory. No store writes.
  A review accepts an annotation, never proves the benefit it quotes."
  (:require [clojure.edn :as edn] [clojure.java.io :as io] [clojure.string :as str]
            [cheshire.core :as json] [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as r]))
(defn fail [reason data] (throw (ex-info "Draft edition refused" (assoc data :reason reason))))
(defn sha [text] (digest/sha256-hex (.getBytes ^String text "UTF-8")))
(defn slice [^String text start end]
  (let [n (.codePointCount text 0 (.length text))]
    (when-not (and (integer? start) (integer? end) (<= 0 start end n) (< start end))
      (fail :invalid-span {:start start :end end}))
    (subs text (.offsetByCodePoints text 0 start) (.offsetByCodePoints text 0 end))))
(defn reference [path coordinate digest at kind]
  (r/make-external-reference
   {:ref/kind kind :store/id "external-files" :store/endpoint path
    :record/id coordinate :schema/authority :draft-edition/codepoint-span-v1
    :valid-time at :revision/digest digest}))
(defn resolve-span [ref]
  (let [c (:record/id ref) path (:store/endpoint ref)
        text (case (:ref/kind ref)
               :file-span (slurp path :encoding "UTF-8")
               :json-field-span
               (let [raw (slurp path :encoding "UTF-8")
                     obj (json/parse-string (if-let [line (:record-line c)]
                                              (nth (str/split-lines raw) (dec line)) raw) true)
                     id (or (:id obj) (:evidence_id obj) (:turn_id obj))]
                 (when-not (= (:source/id c) id) (fail :source-id-mismatch {:path path}))
                 (get obj (keyword (:text-field c))))
               (fail :unsupported-span {:kind (:ref/kind ref)}))]
    (when-not (and (string? text) (= (:revision/digest ref) (sha text)))
      (fail :digest-mismatch {:reference/id (:id ref)}))
    (slice text (:start c) (:end c))))
(defn review [link-id actor decision time scope]
  (r/make-assessment {:subject-clusion-id link-id :actor actor :verdict decision
                      :time time :policy {:scope scope :meaning :annotation-review}
                      :reason "Review of the support annotation, not benefit realization"}))
(defn review-state [reviews link-id]
  (let [rs (filter #(= link-id (:subject-clusion-id %)) reviews)
        joe (last (sort-by (juxt :time :id) (filter #(= "joe" (:actor %)) rs)))]
    {:joe (if joe (:verdict joe) :none) :acts (vec rs)}))
(defn- unique [rs] (vec (vals (into (sorted-map) (map (juxt :id identity) rs)))))
(defn build
  "Inputs are pinned, already parsed data; every external span is independently
  resolved. Snapshot truncation is refusal, never a partial edition."
  [{:keys [draft-path draft-digest scope-digest at segmentation pins anchors candidates truncated?]}]
  (when truncated? (fail :truncated {}))
  (let [text (slurp draft-path :encoding "UTF-8")
        scope (apply str (take 134 (re-seq #"[^\n]*\n|[^\n]+$" text)))
        _ (when-not (and (= draft-digest (sha text)) (= scope-digest (sha scope)))
            (fail :digest-mismatch {:path draft-path}))
        header (r/make-clusion {:type-revision [:annotation/edition 1] :endpoints []
                               :asserted-content {:draft/digest draft-digest :scope/digest scope-digest
                                                  :segmentation segmentation :input/pins pins}})
        refs (mapv (fn [a]
                     (let [ref (reference draft-path {:start (:draft-start a) :end (:draft-end a)
                                                      :segmentation segmentation :scope/digest scope-digest}
                                          draft-digest at :file-span)]
                       (when-not (= (:sentence a) (resolve-span ref)) (fail :draft-span-mismatch {:segment (:index a)})) ref)) anchors)
        by-segment (zipmap (map :index anchors) refs)
        lexical (for [a anchors :when (#{:edited :verbatim} (:class a))]
                  {:segment (:index a) :source (get-in a [:match :source])
                   :span [(get-in a [:match :span :start]) (get-in a [:match :span :end])]
                   :text (get-in a [:match :span :text]) :kind :lexical-correspondence
                   :query {:method :lexical-run :minimum-words (:edited-minimum-words a)}})
        items (mapv (fn [c]
                      (let [s (:source c) [start end] (:span c)
                            target (get by-segment (:segment c))
                            _ (when-not target (fail :unknown-segment {:segment (:segment c)}))
                            source (reference (:file s) {:source/id (:id s) :record-line (:record-line s)
                                                       :text-field (:text-field s) :start start :end end}
                                              (:text-sha256 s) at :json-field-span)
                            actual (resolve-span source)
                            _ (when-not (= actual (:text c)) (fail :source-span-mismatch {:segment (:segment c)}))
                            kind (or (:kind c) :support-proposal)
                            link (r/make-clusion {:type-revision [:annotation/support 1]
                                                  :endpoints [{:role :draft :kind :external-ref :target {:ref/id (:id target)}}
                                                              {:role :source :kind :external-ref :target {:ref/id (:id source)}}]
                                                  :asserted-content {:relation/kind kind}})
                            evidence (r/make-attestation
                                      {:subject-clusion-id (:id link) :producer {:builder "draft-edition/v1"}
                                       :basis (if (= kind :lexical-correspondence) :parser :model)
                                       :standing :unverified :coverage :partial
                                       :method {:run/id (:id header) :input/pins pins :query (:query c)}
                                       :content {:meaning :candidate-not-ancestry :source-span-verified true}
                                       :source-refs [(:source c)]})]
                        {:segment (:segment c) :kind kind :reference source :link link :evidence evidence
                         :text actual :query (:query c)})) (concat lexical (:clips candidates)))
        run (r/make-attestation {:subject-clusion-id (:id header) :producer {:builder "draft-edition/v1"}
                                :basis :parser :standing :verified :coverage :partial
                                :method {:procedure :pinned-input-validation :input/pins pins}
                                :content (select-keys candidates [:empty-queries :empty-embedding-searches :segments-without-candidates])
                                :source-refs (vec (vals pins))})]
    {:status :ok :header header :anchors anchors :target-refs refs :items items :reviews []
     :records (vec (concat (unique (concat refs (map :reference items))) [header]
                           (unique (map :link items)) (unique (map :evidence items)) [run]))}))
(defn edition-view [snapshot]
  (if (or (not= :ok (:status snapshot)) (:truncated? snapshot))
    {:status :inconclusive :reason (if (:truncated? snapshot) :truncated (or (:reason snapshot) :unavailable))}
    {:status :ok :authority :pinned-external-files :stored? false
     :segments (mapv (fn [a]
                       (let [items (filter #(= (:index a) (:segment %)) (:items snapshot))
                             show (fn [x] {:id (get-in x [:link :id]) :source (:reference x) :text (:text x)
                                           :method (:query x) :review (review-state (:reviews snapshot) (get-in x [:link :id]))})]
                         {:index (:index a) :text (:sentence a) :benefit-status (when (:benefit? a) :projected)
                          :lexical (mapv show (filter #(= :lexical-correspondence (:kind %)) items))
                          :proposals (mapv show (filter #(= :support-proposal (:kind %)) items))
                          :no-candidate? (empty? items)})) (:anchors snapshot))}))
(defn load-inputs [config]
  (let [read-pin (fn [k] (let [{:keys [path sha256]} (get-in config [:pins k]) text (slurp path :encoding "UTF-8")]
                            (when-not (= sha256 (sha text)) (fail :digest-mismatch {:path path})) text))]
    (doseq [k (keys (:pins config))] (read-pin k))
    (assoc config :anchors (edn/read-string (read-pin :anchors)) :candidates (json/parse-string (read-pin :candidates) true))))
(defn read-edition [config]
  (try (edition-view (build (load-inputs config)))
       (catch Exception e {:status :inconclusive :reason (or (:reason (ex-data e)) :unavailable)})))
(defn default-config [] (edn/read-string (slurp (io/resource "arxana/draft2-inputs.edn"))))
