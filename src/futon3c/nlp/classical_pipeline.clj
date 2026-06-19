(ns futon3c.nlp.classical-pipeline
  "Classical turn-stream NLP: affect events + turn->mission proposals.

   No LLM calls. Evidence writes go through futon3c.evidence.boundary/append!.
   Term-similarity attribution is proposal-only; it never silently switches the
   active clock."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [futon0.rhythm.affect :as affect]
            [futon3c.evidence.backend :as backend]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.http-backend :as http-backend])
  (:import (java.security MessageDigest)
           (java.time Instant LocalDate ZoneId)))

(def default-base-url "http://localhost:7070")
(def default-output "/home/joe/code/storage/futon0/vitality/affect.jsonl")
(def code-root "/home/joe/code")

(def stopwords
  #{"the" "and" "for" "that" "this" "with" "from" "into" "onto" "about"
    "mission" "missions" "scope" "status" "done" "todo" "work" "note"
    "notes" "open" "close" "will" "would" "could" "should" "have" "has"
    "been" "being" "over" "under" "between" "through" "there" "their"
    "what" "when" "where" "which" "while" "then" "than" "because" "are"
    "all" "also" "any" "both" "but" "can" "did" "didn" "does" "doesn"
    "don" "each" "had" "here" "how" "isn" "its" "just" "may" "more"
    "must" "not" "now" "off" "only" "our" "out" "own" "same" "some"
    "such" "them" "these" "they" "those" "too" "until" "very" "was"
    "wasn" "were" "weren" "who" "why" "without" "you" "your"})

(defn sha256 [s]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn parse-long-safe [x]
  (when x
    (try (Long/parseLong (str x))
         (catch Exception _ nil))))

(defn today-start []
  (str (.toInstant (.atStartOfDay (LocalDate/now (ZoneId/of "Europe/London"))
                                  (ZoneId/of "Europe/London")))))

(defn parse-args [args]
  (loop [args args
         opts {:base-url default-base-url
               :limit 100
               :output default-output}]
    (if (empty? args)
      opts
      (let [a (first args)
            b (second args)
            more (nnext args)]
        (case a
          "--base-url" (recur more (assoc opts :base-url b))
          "--since" (recur more (assoc opts :since (if (= b "today") (today-start) b)))
          "--limit" (recur more (assoc opts :limit (parse-long-safe b)))
          "--output" (recur more (assoc opts :output b))
          "--fixture" (recur (rest args) (assoc opts :fixture? true))
          "--dry-run" (recur (rest args) (assoc opts :dry-run? true))
          "--help" (assoc opts :help? true)
          (throw (ex-info "Unknown argument" {:arg a})))))))

(defn token-set [text]
  (->> (str/split (str/lower-case (or text "")) #"[^a-z0-9]+")
       (remove #(or (str/blank? %) (< (count %) 3) (stopwords %)
                    (re-matches #"\d+" %)))
       set))

;; Same matching model as futon6/scripts/spot-terms.bb: direct token lookup for
;; single words; phrase presence for multi-word terms after content-token gate.
(defn spot-terms [terms text]
  (let [lower (str/lower-case (or text ""))
        tokens (token-set text)]
    (->> terms
         (keep (fn [term]
                 (let [words (str/split term #"\s+")]
                   (cond
                     (= 1 (count words))
                     (when (contains? tokens term) term)

                     (every? #(contains? tokens %) (filter #(>= (count %) 3) words))
                     (when (str/includes? lower term) term)))))
         set)))

(defn kind-for-id [id]
  (cond
    (str/starts-with? id "C-") :campaign
    (str/starts-with? id "M-") :mission
    (str/starts-with? id "E-") :excursion))

(defn target-file? [file]
  (when-let [name (some-> file .getName)]
    (re-matches #"[CME]-[^.]+\.md" name)))

(defn mission-files []
  (let [roots (->> (file-seq (io/file code-root))
                   (filter #(.isDirectory %))
                   (filter #(re-matches #"futon[0-9a-z].*" (.getName %)))
                   vec)]
    (->> roots
         (mapcat (fn [root]
                   (let [holes (io/file root "holes")]
                     (when (.exists holes)
                       (file-seq holes)))))
         (filter #(and (.isFile %) (target-file? %)))
         vec)))

(defn id-from-file [file]
  (second (re-matches #"([CME]-[^.]+)\.md" (.getName file))))

(defn profile-terms [id text]
  (let [id-terms (-> id
                     (str/replace #"^[CME]-" "")
                     (str/replace #"[-_]" " ")
                     token-set)
        text-terms (token-set text)]
    (set/union id-terms (set (take 120 (sort text-terms))))))

(defn load-mission-profiles
  ([] (load-mission-profiles (mission-files)))
  ([files]
   (->> files
        (map (fn [file]
               (let [id (id-from-file file)
                     text (slurp file)]
                 [id {:id id
                      :kind (kind-for-id id)
                      :file (.getAbsolutePath file)
                      :terms (profile-terms id text)}])))
        (into {}))))

(def explicit-token-re
  #"(?<![A-Za-z0-9_-])([CME]-[A-Za-z0-9_-]+)(?![A-Za-z0-9_-])")

(defn explicit-target [profiles text]
  (let [tokens (distinct (map second (re-seq explicit-token-re (or text ""))))
        resolved (keep (fn [t] (when-let [p (get profiles t)] [(:kind p) t p])) tokens)
        unresolved (remove profiles tokens)
        by-kind (group-by first resolved)]
    (when (and (seq resolved)
               (empty? unresolved)
               (every? #(<= (count %) 1) (vals by-kind)))
      (let [target {:campaign-id (some-> by-kind :campaign first second)
                    :mission-id (some-> by-kind :mission first second)
                    :excursion-id (some-> by-kind :excursion first second)}]
        {:kind :resolved
         :target target
         :witness {:rule "explicit"
                   :tokens (vec tokens)
                   :new-target target}}))))

(defn term-similarity-proposal
  [profiles text]
  (let [query-terms (token-set text)
        scored (->> profiles
                    vals
                    (map (fn [p]
                           (let [hits (set/intersection query-terms (:terms p))]
                             (assoc p :hits hits :score (count hits)))))
                    (filter #(pos? (:score %)))
                    (sort-by (juxt (comp - :score) :id))
                    vec)
        top (first scored)
        next-score (or (:score (second scored)) 0)]
    (when (and top
               (>= (:score top) 3)
               (>= (:score top) (+ next-score 2)))
      {:kind :proposal
       :target {:campaign-id (when (= :campaign (:kind top)) (:id top))
                :mission-id (when (= :mission (:kind top)) (:id top))
                :excursion-id (when (= :excursion (:kind top)) (:id top))}
       :witness {:rule "term-similarity"
                 :score (:score top)
                 :matched-terms (vec (sort (:hits top)))
                 :file (:file top)
                 :new-target (:id top)
                 :proposal-only true}})))

(defn resolve-turn [profiles {:keys [text]}]
  (or (explicit-target profiles text)
      (term-similarity-proposal profiles text)))

(defn body-of [entry] (or (:evidence/body entry) (:body entry) {}))
(defn text-of [entry]
  (let [body (body-of entry)]
    (or (:text body) (:prompt body) (:response body) (:content body) (:message body))))
(defn author-of [entry] (or (:evidence/author entry) (:author entry) "unknown"))
(defn session-of [entry] (or (:evidence/session-id entry) (:session-id entry)))
(defn id-of [entry] (or (:evidence/id entry) (:id entry) (sha256 (pr-str entry))))
(defn at-of [entry] (or (:evidence/at entry) (:at entry) (str (Instant/now))))

(defn turn-entry? [entry]
  (let [body (body-of entry)]
    (and (string? (text-of entry))
         (not (str/blank? (text-of entry)))
         (or (= "chat-turn" (:event body))
             (= :chat-turn (:event body))))))

(defn fetch-turns [{:keys [base-url since limit]}]
  (let [backend (http-backend/make-http-backend base-url)]
    (->> (backend/-query backend {:query/since since
                                  :query/limit (or limit 100)})
         (filter turn-entry?)
         vec)))

(def fixture-turns
  [{:evidence/id "fixture-please"
    :evidence/at "2026-06-09T00:00:00Z"
    :evidence/author "joe"
    :evidence/session-id "fixture"
    :evidence/body {:event "chat-turn" :text "please send the receipt" :role "user"}}
   {:evidence/id "fixture-interesting"
    :evidence/at "2026-06-09T00:01:00Z"
    :evidence/author "joe"
    :evidence/session-id "fixture"
    :evidence/body {:event "chat-turn" :text "interesting, park it for later" :role "user"}}
   {:evidence/id "fixture-not-happy"
    :evidence/at "2026-06-09T00:02:00Z"
    :evidence/author "joe"
    :evidence/session-id "fixture"
    :evidence/body {:event "chat-turn" :text "I am not happy with that result" :role "user"}}
   {:evidence/id "fixture-joy"
    :evidence/at "2026-06-09T00:03:00Z"
    :evidence/author "joe"
    :evidence/session-id "fixture"
    :evidence/body {:event "chat-turn" :text "I am happy and delighted with this" :role "user"}}
   {:evidence/id "fixture-inspiration"
    :evidence/at "2026-06-09T00:04:00Z"
    :evidence/author "joe"
    :evidence/session-id "fixture"
    :evidence/body {:event "chat-turn" :text "This inspires a new proposal for the resolver" :role "user"}}
   {:evidence/id "fixture-explicit"
    :evidence/at "2026-06-09T00:05:00Z"
    :evidence/author "joe"
    :evidence/session-id "fixture"
    :evidence/body {:event "chat-turn" :text "please clock this to M-foo" :role "user"}}
   {:evidence/id "fixture-term"
    :evidence/at "2026-06-09T00:06:00Z"
    :evidence/author "joe"
    :evidence/session-id "fixture"
    :evidence/body {:event "chat-turn" :text "the alpha beta gamma design needs the resolver" :role "user"}}
   {:evidence/id "fixture-offtopic"
    :evidence/at "2026-06-09T00:07:00Z"
    :evidence/author "joe"
    :evidence/session-id "fixture"
    :evidence/body {:event "chat-turn" :text "make tea after lunch" :role "user"}}])

(def fixture-profiles
  {"M-foo" {:id "M-foo" :kind :mission :file "/tmp/holes/missions/M-foo.md"
            :terms #{"foo" "clock" "target"}}
   "M-alpha-beta-gamma" {:id "M-alpha-beta-gamma" :kind :mission
                         :file "/tmp/holes/missions/M-alpha-beta-gamma.md"
                         :terms #{"alpha" "beta" "gamma" "design" "resolver"}}})

(defn legacy-affect-label [text]
  (cond
    (re-find #"(?i)\bpleas" text) "joy"
    (re-find #"(?i)\binterest" text) "attraction"
    (re-find #"(?i)\bhappy\b" text) "joy"
    :else "-"))

(defn affect-row [entry]
  (let [text (text-of entry)
        detected (affect/detect-affect text)]
    {:id (id-of entry)
     :before (legacy-affect-label text)
     :after (or (some-> detected :type name) "-")
     :event-type (or (some-> detected :event-type name) "-")}))

(defn affect-json-row [entry detected]
  {:timestamp (at-of entry)
   :marker (name (:type detected))
   :event-type (name (:event-type detected))
   :value (double (:conf detected))
   :source "classical-nlp-pipeline"
   :transition_id (id-of entry)
   :session_id (session-of entry)
   :author (author-of entry)
   :capacity_terms []
   :trigger_text (text-of entry)})

(defn affect-evidence-entry [_entry row]
  {:evidence-id (str "affect-event/" (sha256 (str (:transition_id row) "/" (:marker row) "/" (:event-type row))))
   :subject {:ref/type :evidence :ref/id (:transition_id row)}
   :type :reflection
   :claim-type :observation
   :author "classical-nlp"
   :at (str (Instant/now))
   :session-id (:session_id row)
   :body {:event "affect-event"
          :event-type (:event-type row)
          :marker (:marker row)
          :value (:value row)
          :source-turn (:transition_id row)
          :trigger-text (:trigger_text row)
          :classical-bound "text-bound detector; performed-affect resistance out of scope"}
   :tags [:classical-nlp :affect :arrow-witness]})

(defn mission-evidence-entry [entry resolution]
  (let [w (:witness resolution)]
    {:evidence-id (str "turn-mission-proposal/" (sha256 (str (id-of entry) "/" (pr-str w))))
     :subject {:ref/type :evidence :ref/id (id-of entry)}
     :type :coordination
     :claim-type :observation
     :author "classical-nlp"
     :at (str (Instant/now))
     :session-id (session-of entry)
     :body {:event "turn-mission-attribution"
            :resolution-kind (name (:kind resolution))
            :target (:target resolution)
            :witness w
            :source-turn (id-of entry)
            :never-silent-mislock true}
     :tags [:classical-nlp :turn-mission
            (if (= :proposal (:kind resolution)) :proposal :resolved)]}))

(defn entry-id [entry]
  (or (:evidence-id entry) (:evidence/id entry)))

(defn append-row! [backend entry]
  (let [eid (entry-id entry)]
    (if (and eid (backend/-exists? backend eid))
      {:ok true :evidence/id eid :idempotent? true}
      (boundary/append! backend entry))))

(defn write-jsonl! [file rows]
  (let [f (io/file file)]
    (when-let [parent (.getParentFile f)] (.mkdirs parent))
    (with-open [w (io/writer f :append true)]
      (doseq [row rows]
        (json/write row w)
        (.write w "\n")))))

(defn print-table [title rows columns]
  (println title)
  (println (str/join "\t" columns))
  (doseq [row rows]
    (println (str/join "\t" (map #(get row %) columns)))))

(defn run-pipeline [opts]
  (let [turns (if (:fixture? opts) fixture-turns (fetch-turns opts))
        profiles (if (:fixture? opts) fixture-profiles (load-mission-profiles))
        backend (http-backend/make-http-backend (:base-url opts))
        affect-rows (mapv affect-row turns)
        detections (keep (fn [entry]
                           (when-let [detected (affect/detect-affect (text-of entry))]
                             [entry (affect-json-row entry detected)]))
                         turns)
        resolutions (keep (fn [entry]
                            (when-let [r (resolve-turn profiles {:text (text-of entry)})]
                              {:id (id-of entry)
                               :kind (name (:kind r))
                               :target (pr-str (:target r))
                               :witness (pr-str (:witness r))
                               :entry entry
                               :resolution r}))
                          turns)
        affect-receipts (when-not (:dry-run? opts)
                          (doall
                           (for [[entry row] detections]
                             (append-row! backend (affect-evidence-entry entry row)))))
        mission-receipts (when-not (:dry-run? opts)
                           (doall
                            (for [{:keys [entry resolution]} resolutions]
                              (append-row! backend (mission-evidence-entry entry resolution)))))]
    (write-jsonl! (:output opts) (map second detections))
    (print-table "AFFECT BEFORE/AFTER" affect-rows [:id :before :after :event-type])
    (print-table "TURN->MISSION RESOLUTIONS" resolutions [:id :kind :target :witness])
    (println "JSONL rows:" (count detections) "->" (:output opts))
    (println "affect receipts:" (if (:dry-run? opts) "dry-run" (pr-str (mapv #(select-keys % [:ok :evidence/id :idempotent? :error/code]) affect-receipts))))
    (println "mission receipts:" (if (:dry-run? opts) "dry-run" (pr-str (mapv #(select-keys % [:ok :evidence/id :idempotent? :error/code]) mission-receipts))))
    {:turns turns :affect affect-rows :resolutions resolutions
     :affect-receipts affect-receipts :mission-receipts mission-receipts}))

(defn -main [& args]
  (let [opts (parse-args args)]
    (when (:help? opts)
      (println "Usage: clojure -M -m futon3c.nlp.classical-pipeline [--since today|ISO] [--limit n] [--fixture] [--dry-run]")
      (System/exit 0))
    (run-pipeline opts)))
