(ns futon3c.arxana.lexicon-loader
  "Load claude-6's generated lexicon and the Agency survey as immutable Arxana records."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [jsonista.core :as json]
            [futon3c.arxana.json :as ajson]
            [futon3c.arxana.records :as records])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]))

(def lexicon-path "/home/joe/notes/cascade-live-outline/lexicon/merge/all.json")
(def lexicon-dir "/home/joe/notes/cascade-live-outline/lexicon")
(def survey-path "/home/joe/code/futon4/holes/labs/M-jvm-arxana/AGENCY-survey.md")
(def lexicon-producer "claude-6 lexicon 2026-11-08")
(def survey-producer "Agency survey c9789e2")

(defn read-lexicon []
  (json/read-value (slurp lexicon-path) json/keyword-keys-object-mapper))

(defn- markdown-files []
  (sort-by #(.getName %) (filter #(and (.isFile %) (str/ends-with? (.getName %) ".md"))
                                  (file-seq (io/file lexicon-dir)))))

(defn read-live-tier []
  (into {}
        (for [f (markdown-files)
              block (re-seq #"(?s)```\s*\n(.*?)\n```" (slurp f))
              :let [fields (into {} (keep (fn [[_ k v]] [(keyword k) (str/trim v)])
                                           (re-seq #"(?m)^@(entity|live|tier)\s+(.+)$" (second block))))]
              :when (:entity fields)]
          [(:entity fields) (select-keys fields [:live :tier])])))

(defn- split-outside-parens [s delimiter]
  (loop [cs (seq s) depth 0 token (StringBuilder.) out []]
    (if-let [c (first cs)]
      (cond
        (= c \() (recur (next cs) (inc depth) (.append token c) out)
        (= c \)) (recur (next cs) (max 0 (dec depth)) (.append token c) out)
        (and (= c delimiter) (zero? depth))
        (recur (next cs) depth (StringBuilder.) (conj out (str/trim (str token))))
        :else (recur (next cs) depth (.append token c) out))
      (cond-> out (pos? (.length token)) (conj (str/trim (str token)))))))

(defn parse-relations [row]
  (vec
   (mapcat
    (fn [segment]
      (when-let [[_ verb targets] (re-matches #"(\S+)\s+(.+)" segment)]
        (for [raw-target (split-outside-parens targets \,)
              :let [[_ target note] (or (re-matches #"(?s)(.*?)\s*\((.*)\)\s*" raw-target)
                                        [nil raw-target nil])]]
          {:source (:entity row) :verb verb :target (str/trim target)
           :note note :raw segment})))
    (split-outside-parens (or (:relations row) "") \;))))

(defn- aliases [row]
  (remove str/blank? (concat [(:entity row) (:name row)]
                             (map str/trim (str/split (or (:aka row) "") #",")))))

(defn resolver [rows]
  (reduce (fn [m row] (reduce #(assoc %1 (str/lower-case %2) (:entity row)) m (aliases row))) {} rows))

(defn resolved-relations [rows]
  (let [resolve (resolver rows)]
    (reduce (fn [acc rel]
              (let [target (get resolve (str/lower-case (:target rel)))]
                (cond
                  (nil? target) (update acc :unresolved conj rel)
                  ;; An alias can resolve a relation back to its own source
                  ;; ("arxana part-of futon4", where futon4 is an alias of
                  ;; arxana).  A self-referent says nothing; report it.
                  (= target (:source rel)) (update acc :unresolved conj (assoc rel :reason :self-via-alias))
                  :else (update acc :resolved conj (assoc rel :target target)))))
            {:resolved [] :unresolved []}
            (mapcat parse-relations rows))))

(defn- entity [slug]
  (records/make-clusion {:type-revision [:lexicon/entity 1] :endpoints []
                         :asserted-content {:entity/slug slug}}))
(defn- referent [source verb target]
  (records/make-clusion {:type-revision [:workplan/referent 1]
                         :endpoints [{:role :entity :kind :clusion :target {:clusion/id (:id (entity source))}}
                                     {:role :target :kind :clusion :target {:clusion/id (:id (entity target))}}]
                         :asserted-content {:referent/role verb}}))

(defn- attestation [subject producer method content]
  (records/make-attestation {:subject-clusion-id (:id subject) :producer producer :basis :model
                             :method {:id method :version 1} :standing :unverified
                             :coverage :complete :content content}))

(defn- lexicon-records [rows live-tier]
  (mapcat
   (fn [row]
     ;; No word rule, member or forms attestation for lexicon entities
     ;; (claude-13 review): a newer forms attestation would supersede the
     ;; trial rules' forms (newest wins), switching "Agency" off and replacing
     ;; "kangaroo"; and 251 catalog members would make sessionista fetch every
     ;; view on each refresh.  A word rule comes only from Joe's [[X]].
     (let [e (entity (:entity row)) lt (get live-tier (:entity row))]
       [{:kind :clusion :record e}
        {:kind :attestation :record
         (attestation e lexicon-producer "lexicon/entity-survey"
                      (merge (select-keys row [:name :aka :status :what :where :evidence :since :src]) lt))}]))
   rows))

(defn- relation-records [relations producer method source-ref]
  (mapcat
   (fn [{:keys [source verb target raw note]}]
     (let [r (referent source verb target)]
       [{:kind :clusion :record r}
        {:kind :attestation :record
         (attestation r producer method
                      (cond-> {:relation/raw raw :relation/verb verb :relation/target target
                               :source/ref source-ref :candidate? true}
                        note (assoc :relation/note note)))}]))
   ;; One relation per tuple even if prose repeats it.
   (sort-by (juxt :source :verb :target)
            (vals (into {} (map (juxt #(select-keys % [:source :verb :target]) identity) relations))))))

(defn read-survey-entities []
  (->> (str/split-lines (slurp survey-path))
       (keep (fn [line]
               (when (and (str/starts-with? line "|") (str/includes? line "**(new"))
                 (let [cols (mapv str/trim (str/split line #"\|"))
                       feature (get cols 1) what (get cols 2)
                       slug (second (re-find #"`([^`]+)`" feature))]
                   {:entity slug :name (str/trim (first (str/split feature #"/"))) :what what}))))))

(def survey-corrections
  [{:id 1 :relations [{:source "kangaroo-pouch" :verb "in-namespace" :target "agency"
                       :raw "in-namespace agency"}
                      {:source "kangaroo-pouch" :verb "extends" :target "agency"
                       :raw "extends agency"}]}
   {:id 2 :entity "federation" :content {:survey/qualification "code-backed; live use not established"}}
   {:id 3 :entity "work-orders" :content {:survey/qualification "route and open records are live; Giraffe relation unresolved"}}
   {:id 4 :entity "agency-clock" :content {:survey/qualification "clock_lineage still defaults to retired futon1a"}}
   {:id 5 :entity "test-registry" :content {:survey/qualification "served by Agency JVM, not in Agency namespace"}}
   {:id 6 :relations [{:source "agency" :verb "uses" :target "evidence-landscape"
                       :raw "agency uses evidence-landscape"}]}
   {:id 7 :relations [{:source "emacs-repl" :verb "uses" :target "agency" :raw "emacs-repl uses agency"}
                      {:source "park" :verb "delivers-to" :target "emacs-repl" :raw "park delivers-to emacs-repl"}]}
   {:id 8 :entity "multi-watcher" :content {:survey/qualification "co-resident, not in Agency namespace"}}
   {:id 9 :entity "inbox-zero" :content {:survey/qualification "co-resident rather than an Agency feature"}}
   {:id 10 :entity "pattern-search" :content {:survey/qualification "alias of pattern-catalog-search at this granularity"}}])

(defn- survey-records [lexicon-slugs]
  (let [new-rows (read-survey-entities)
        new-records (mapcat (fn [row]
                              (let [e (entity (:entity row))]
                                [{:kind :clusion :record e}
                                 {:kind :attestation :record
                                  (attestation e survey-producer "agency-survey/entity"
                                               {:name (:name row) :what (:what row)
                                                :survey/ref "AGENCY-survey.md feature inventory"})}]))
                            new-rows)
        relation-corrections (mapcat :relations survey-corrections)
        relation-records (relation-records relation-corrections survey-producer
                                           "agency-survey/correction" "AGENCY-survey.md claims 1-10")
        att-records (mapcat (fn [{:keys [id content] entity-slug :entity}]
                              (when entity-slug
                                (let [e (entity entity-slug)]
                                  ;; New survey entities were asserted above; existing ones come from the lexicon.
                                  (when (or (contains? lexicon-slugs entity-slug)
                                            (some #(= entity-slug (:entity %)) new-rows))
                                    [{:kind :attestation :record
                                      (attestation e survey-producer "agency-survey/correction"
                                                   (assoc content :survey/correction id
                                                          :survey/ref "AGENCY-survey.md claims 1-10"))}]))))
                            survey-corrections)]
    {:new-rows new-rows :records (vec (concat new-records relation-records att-records))}))

(defn plan []
  (let [rows (read-lexicon) live-tier (read-live-tier)
        {:keys [resolved unresolved]} (resolved-relations rows)
        survey (survey-records (set (map :entity rows)))
        records (concat (lexicon-records rows live-tier)
                        (relation-records resolved lexicon-producer "lexicon/relation"
                                          "lexicon/merge/all.json relations")
                        (:records survey))
        priority (fn [{:keys [kind record]}]
                   (if (= kind :attestation) 3
                       (case (first (:type-revision record))
                         (:lexicon/entity :workplan/catalog) 0
                         :workplan/entity-match-rule 1
                         2)))
        deduped (vals (into {} (map (juxt #(get-in % [:record :id]) identity) records)))]
    {:rows rows :live-tier live-tier :resolved resolved :unresolved unresolved
     :survey survey
     ;; Multiple sources may assert the same immutable clusion. Attestations
     ;; remain distinct; exact duplicate records are written only once.
     :records (vec (sort-by (juxt priority #(get-in % [:record :id])) deduped))}))

(defn- request [run-id {:keys [kind record]}]
  {:protocol/version "arxana.v1" :message/id (str "lexicon:" run-id ":" (:id record))
   :principal/id "workplan/lexicon-loader"
   :operation (if (= kind :attestation) :evidence/append :clusion/assert)
   :body {:record record}})

(defn materialise! [dispatch!]
  (let [{:keys [rows live-tier resolved unresolved survey records]} (plan)
        run-id (random-uuid)
        responses (mapv (fn [x] [x (dispatch! (request run-id x))]) records)
        failures (mapv (fn [[x r]] {:record/id (get-in x [:record :id]) :error (:error r)})
                       (filter (comp :error second) responses))
        created (count (filter #(true? (get-in % [1 :ok :created?])) responses))]
    {:counts {:lexicon/entities (count rows)
              :lexicon/entity-attestations (count rows)
              :relations/parsed (+ (count resolved) (count unresolved))
              :relations/written (count (set (map (juxt :source :verb :target) resolved)))
              :relations/unresolved (count unresolved)
              :live/found (count (filter (comp :live val) live-tier))
              :tier/found (count (filter (comp :tier val) live-tier))
              :survey/entities (count (:new-rows survey))
              :survey/corrections (count survey-corrections)
              :records/planned (count records)
              :records/created created}
     :sessionista {:stored-disabled-forms 0 :exposed-pending-forms 0
                   :decision :not-exposed-without-joe-introduction}
     :unresolved unresolved :failures failures}))

(defn http-dispatch [base-url]
  (let [client (HttpClient/newHttpClient) endpoint (str (str/replace base-url #"/$" "") "/arxana/v1")]
    (fn [message]
      (let [req (-> (HttpRequest/newBuilder (URI/create endpoint))
                    (.header "Content-Type" "application/json")
                    (.POST (HttpRequest$BodyPublishers/ofString (ajson/encode message))) .build)
            response (.send client req (HttpResponse$BodyHandlers/ofString))]
        (when-not (<= 200 (.statusCode response) 299)
          (throw (ex-info "Arxana HTTP request failed" {:status (.statusCode response)})))
        (ajson/decode (.body response))))))

(defn -main [& [base-url]]
  (let [report (materialise! (http-dispatch (or base-url "http://127.0.0.1:7094")))]
    (println (pr-str report))
    (when (seq (:failures report)) (System/exit 1))))
