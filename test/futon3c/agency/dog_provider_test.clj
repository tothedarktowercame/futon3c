(ns futon3c.agency.dog-provider-test
  "狗 pre-hook tests. minus40 is stubbed by RECORDED responses: the three
   fixtures in test/fixtures/dog/ are real text-search answers from the
   minus40 store (:7093) for three real operator turns of session
   ed69fb38, captured 2026-11-11."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.dog-provider :as dog]
            [futon3c.agency.prompt-line :as prompt-line]
            [futon3c.xiang.turn-store :as turn-store])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- fixture [name]
  (json/parse-string (slurp (io/resource (str "fixtures/dog/" name))) true))

(def fixture-05cXMM (delay (fixture "turn-05cXMM.json")))
(def fixture-02Hrgm (delay (fixture "turn-02Hrgm.json")))
(def fixture-1jgPTg (delay (fixture "turn-1jgPTg.json")))

(defn- stub-request
  "A :request-fn serving the recorded response of the FIRST fixture, for
   any query (the fixtures are recordings of real m40 answers; the recall
   derives its own keywords from whatever text the test passes)."
  [fixtures]
  (fn [_url] (:response (first fixtures))))

(defn- tmp-store []
  {:dir (str (Files/createTempDirectory "dog-test" (into-array FileAttribute [])))})

(defn- write-operator-record!
  [store id source-text]
  (turn-store/write-record-id! store id
                               {:origin "operator" :source_text source-text
                                :session_id "sess-test" :agent_id "kimi-1"
                                :created_at "2026-11-11T18:00:00Z"}))

(use-fixtures :each
  (fn [f]
    (prompt-line/reset-registry!)
    (dog/reset-cache!)
    (f)
    (prompt-line/reset-registry!)
    (dog/reset-cache!)))

(deftest keywords-extract-distinctive-terms
  (let [q (dog/keywords "Sure, please write up a TN-wm-tokens-and-sessionista-terms.md and we will have that on the record")]
    (is (re-find #"OR" q))
    (is (re-find #"TN-wm-tokens-and-sessionista-terms" q))
    (is (not (re-find #"(?i)\bthe\b" q)) "stopwords dropped"))
  (is (= "" (dog/keywords "the and of to in")))
  (is (not (re-find #"[\"(){}:^*~]" (dog/keywords "!!! \"quoted\" (parens) {braces} ^boost ~fuzzy")))
      "FTS5-significant characters stripped — a raw operator text made the store answer 500"))

(deftest recall-groups-recorded-hits-by-memory
  (doseq [fix [@fixture-05cXMM @fixture-02Hrgm @fixture-1jgPTg]]
    (let [result (dog/recall "operator text" {:request-fn (stub-request [fix])})]
      (is (nil? (:status result)) (str "a hit, not a typed absence: " (:query fix)))
      (is (<= 0 (:query-ms result)))
      (is (<= 1 (count (:items result)) dog/max-items))
      (doseq [{:keys [memory why score]} (:items result)]
        (is (re-matches #"[^/]+/[^/]+" memory) "project/slug")
        (is (<= (count (clojure.string/split why #"\s+")) 8) "why ≤ 8 words")
        (is (some? score))))))

(deftest recall-typed-absences
  (is (= "none" (:status (dog/recall "text" {:request-fn (stub-request [])})))
      "no fixture matches → empty response → none")
  (is (= "none" (:status (dog/recall "the and of" {}))) "no keywords → none, no request")
  (is (= "failed" (:status (dog/recall "some operator text about giraffes"
                                       {:request-fn (fn [_] (throw (ex-info "store down" {})))})))))

(defn- prepare!
  "Synchronously prepare the cache for RECORD-ID from STORE's record."
  [opts store record-id]
  (let [record (turn-store/read-record store record-id)]
    (#'dog/publish! record-id (dog/recall (:source_text record) opts))))

(defn- memory-fact
  "The http.clj Prompt-line fact for a :memory segment (kept in step with
   transport.http/memory-prompt-fact)."
  [segment]
  (let [header (:segment/header segment)
        status (get-in segment [:segment/dog-recall :status])]
    (cond
      (not (clojure.string/blank? header)) (str "memory " header)
      (= "not-ready" status) "memory: not ready"
      :else nil)))

(defn- render-with-dog
  "Render with only the dog provider registered (the each-fixture cleared
   the registry)."
  [ctx]
  (dog/register!)
  (prompt-line/render ctx (prompt-line/providers)))

(deftest provider-renders-recalled-memories
  (let [store (tmp-store)
        record-id "turn-dogTEST1"]
    (write-operator-record! store record-id "benefits have a time-limited nature")
    (prepare! {:request-fn (stub-request [@fixture-05cXMM])} store record-id)
    (let [result (render-with-dog {:agent-id "kimi-1" :session-id "sess-test"
                                   :turn-record-id record-id :render-at "2026-11-11T18:30:00Z"})
          segment (first (:segments result))]
      (is (= :memory (:segment/id segment)))
      (is (re-find #"^memory ~[^/]+/[^ ]+ \(.+\)$" (memory-fact segment))
          "memory ~<project>/<slug> (<why>)")
      (is (some? (:segment/dog-recall segment)))
      (is (<= (count (get-in segment [:segment/dog-recall :items])) 3)))))

(deftest analysis-seats-get-nothing
  (let [store (tmp-store)
        record-id "turn-dogTEST2"]
    (write-operator-record! store record-id "benefits have a time-limited nature")
    (prepare! {:request-fn (stub-request [@fixture-05cXMM])} store record-id)
    (doseq [seat ["象" "象-sonnet" "馬-sonnet" "驴-haiku"]]
      (let [result (render-with-dog {:agent-id seat :session-id "sess-test"
                                     :turn-record-id record-id
                                     :render-at "2026-11-11T18:30:00Z"})]
        (is (empty? (filter #(= :memory (:segment/id %)) (:segments result)))
            (str seat " gets no dog recall"))))))

(deftest slow-m40-yields-not-ready-inside-the-render-budget
  (let [store (tmp-store)
        record-id "turn-dogTEST3"
        _ (write-operator-record! store record-id "benefits have a time-limited nature")
        ;; a slow m40: the stub sleeps well past the provider budget
        slow (fn [_] (Thread/sleep 2000) (:response @fixture-05cXMM))]
    (binding [dog/*request-fn* slow
              prompt-line/*turn-record-store* store]
      (let [started (System/currentTimeMillis)
            ;; cold cache: the provider kicks the slow compute onto the
            ;; background thread and reports not-ready without waiting
            result (render-with-dog {:agent-id "kimi-1" :session-id "sess-test"
                                     :turn-record-id record-id
                                     :render-at "2026-11-11T18:30:00Z"})
            elapsed (- (System/currentTimeMillis) started)
            segment (first (:segments result))]
        (is (= :memory (:segment/id segment)))
        (is (= "memory: not ready" (memory-fact segment)))
        (is (= "not-ready" (get-in segment [:segment/dog-recall :status])))
        (is (< elapsed prompt-line/whole-render-budget-ms)
            "the line renders inside the whole-render budget")
        ;; the slow recall lands in the background; the NEXT render shows it
        (is (loop [n 0]
              (cond (dog/cached-recall record-id) true
                    (< 100 n) false
                    :else (do (Thread/sleep 50) (recur (inc n)))))
            "background compute completes")
        (let [segment2 (first (:segments (render-with-dog {:agent-id "kimi-1"
                                                           :session-id "sess-test"
                                                           :turn-record-id record-id
                                                           :render-at "2026-11-11T18:31:00Z"})))]
          (is (seq (get-in segment2 [:segment/dog-recall :items]))
              "once ready, the items render"))))))

(deftest render-records-dog-recall-on-the-turn-record
  (let [store (tmp-store)
        record-id "turn-dogTEST4"]
    (write-operator-record! store record-id "benefits have a time-limited nature")
    (prepare! {:request-fn (stub-request [@fixture-05cXMM])} store record-id)
    (dog/register!)
    (binding [prompt-line/*turn-record-store* store]
      (prompt-line/render! {:agent-id "kimi-1" :session-id "sess-test"
                            :turn-record-id record-id}))
    (let [dog-recall (:dog_recall (turn-store/read-record store record-id))]
      (is (map? dog-recall) "dog_recall is never silently missing")
      (is (seq (:items dog-recall)))
      (is (number? (:query-ms dog-recall)))
      (is (some? (:at dog-recall))))))

(deftest render-records-typed-absence-when-m40-fails
  (let [store (tmp-store)
        record-id "turn-dogTEST5"]
    (write-operator-record! store record-id "some operator text about giraffes")
    (prepare! {:request-fn (fn [_] (throw (ex-info "store down" {})))} store record-id)
    (dog/register!)
    (binding [prompt-line/*turn-record-store* store]
      (prompt-line/render! {:agent-id "kimi-1" :session-id "sess-test"
                            :turn-record-id record-id}))
    (let [dog-recall (:dog_recall (turn-store/read-record store record-id))]
      (is (map? dog-recall) "failure still records dog_recall")
      (is (= "failed" (:status dog-recall))))))

(deftest off-switch-disables-the-hook
  (let [off (io/file (.toFile (Files/createTempDirectory "dog-off" (into-array FileAttribute [])))
                     "dog-off")]
    (.createNewFile off)
    (binding [dog/*off-switch-path* (str off)]
      (let [store (tmp-store)
            record-id "turn-dogTEST6"]
        (write-operator-record! store record-id "benefits have a time-limited nature")
        (prepare! {:request-fn (stub-request [@fixture-05cXMM])} store record-id)
        (is (nil? (dog/provider {:agent-id "kimi-1" :session-id "sess-test"
                                 :turn-record-id record-id}))
            "provider silent while the off-switch file exists")))))


;; -------------------------------------------- second pass (M-repl-circus):
;; 驴-seeded queries, dated directives, de-duplication, readable projects.
;; Fixtures turn-vsC2tD.* are real minus40 answers captured 2026-11-11
;; (read-only) for the queries the new recall builds for that turn, and
;; the turn's real .patterns.json.

(def fixture-vsC2tD-kw (delay (fixture "turn-vsC2tD.kw.json")))
(def fixture-vsC2tD-lu (delay (fixture "turn-vsC2tD.lu.json")))

(defn- routing-request
  "A :request-fn that answers the 驴-seeded query from LU-FIX and any
   other query from KW-FIX, recording the URLs it saw in order."
  [lu-fix kw-fix]
  (let [seen (atom [])]
    [seen
     (fn [url]
       (swap! seen conj url)
       (if (re-find #"bare-continue-signal" url)
         (:response lu-fix)
         (:response kw-fix)))]))

(deftest lu-seeded-query-runs-first-and-tags-sources
  (let [[seen request-fn] (routing-request @fixture-vsC2tD-lu @fixture-vsC2tD-kw)
        result (dog/recall "OK please send pattern-index packet 3"
                           {:request-fn request-fn
                            :lu-terms (seq (#'dog/content-terms
                                            "bare-continue-signal Bare Continue Signal"))})]
    (is (re-find #"bare-continue-signal" (first @seen))
        "the first (and seeding) query is built from 驴's terms")
    (is (nil? (:status result)))
    (is (seq (:items result)))
    (let [sources (mapv :source (:items result))]
      (is (= "lu" (first sources)) "驴-seeded items lead")
      (is (some #{"keywords"} sources)
          "keywords fill the items the 驴 query did not find"))
    (is (= "mixed" (:query-source result)))
    (is (= (count (:items result)) (count (distinct (map :memory (:items result)))))
        "de-duplicated by memory")))

(deftest keywords-are-the-fallback-when-no-patterns
  (let [[seen request-fn] (routing-request @fixture-vsC2tD-lu @fixture-vsC2tD-kw)
        result (dog/recall "OK please send pattern-index packet 3"
                           {:request-fn request-fn})]
    (is (nil? (:status result)))
    (is (seq (:items result)))
    (is (= #{"keywords"} (set (map :source (:items result)))))
    (is (= "keywords" (:query-source result)))
    (is (not (re-find #"bare-continue-signal" (first @seen))))))

(deftest dated-directives-are-dropped
  ;; The kw fixture really contains -home-joe-code-voxterm/stop-dispatches-2026-10-23,
  ;; the lapsed order the first pass recalled as a current fact. It ranks
  ;; 6th of 6 memories in the fixture, so widen max-items: the filter, not
  ;; the cut, must be what keeps it out.
  (is (re-find #"stop-dispatches-2026-10-23"
               (slurp (io/resource "fixtures/dog/turn-vsC2tD.kw.json")))
      "the fixture really contains a dated directive")
  (with-redefs [dog/max-items 10]
    (let [[_ request-fn] (routing-request @fixture-vsC2tD-lu @fixture-vsC2tD-kw)
          result (dog/recall "OK please send pattern-index packet 3"
                             {:request-fn request-fn})]
      (is (<= 4 (count (:items result))) "enough items that the dated one would show")
      (is (not-any? #(re-find #"stop-dispatches" (str (:memory %))) (:items result))
          "a dated directive is dropped, not offered as a current fact"))))

(deftest dedupe-across-the-two-queries
  ;; One memory, found by BOTH queries: it appears once, from 驴.
  (let [request-fn (fn [_url] (:response @fixture-05cXMM))
        result (dog/recall "benefits have a time-limited nature"
                           {:request-fn request-fn
                            :lu-terms ["benefits" "time-limited"]})]
    (is (= 1 (count (:items result))))
    (is (= "lu" (:source (first (:items result)))))))

(deftest readable-project-names
  (is (= "voxterm" (dog/readable-project "-home-joe-code-voxterm")))
  (is (= "zai-6" (dog/readable-project "zai-6")) "a plain project stays itself")
  (let [[_ request-fn] (routing-request @fixture-vsC2tD-lu @fixture-vsC2tD-kw)
        result (dog/recall "OK please send pattern-index packet 3"
                           {:request-fn request-fn})
        item (first (:items result))]
    (is (re-matches #"[^/]+/[^/]+" (:display item)) "display is readable/slug")
    (is (not (str/starts-with? (:display item) "-")))
    (is (re-matches #"[^/]+/[^/]+" (:memory item))
        "the raw handle stays, resolvable through m40_find in one call")))

(deftest lu-terms-from-a-real-patterns-file
  (let [store (tmp-store)
        record-id "turn-luTEST1"]
    (write-operator-record! store record-id "OK please send pattern-index packet 3")
    (spit (turn-store/pattern-candidates-path store record-id)
          (slurp (io/resource "fixtures/dog/turn-vsC2tD.patterns.json")))
    (let [terms (dog/lu-terms store record-id)]
      (is (seq terms))
      (is (some #{"bare-continue-signal"} terms))
      (is (not-any? #{"operator"} terms) "the family prefix is not a term")))
  (let [store (tmp-store)]
    (is (nil? (dog/lu-terms store "turn-luTEST2"))
        "no patterns file and no record: nil, so the caller falls back to keywords")))
