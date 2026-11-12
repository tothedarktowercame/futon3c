(ns futon3c.agency.dog-provider-test
  "狗 pre-hook tests. minus40 is stubbed by RECORDED responses: the three
   fixtures in test/fixtures/dog/ are real text-search answers from the
   minus40 store (:7093) for three real operator turns of session
   ed69fb38, captured 2026-11-11."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
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

;; The synthetic record ids below ending in "-s" hash to arm "shown"
;; (packet B); the arm itself is tested at the end of this file.

(defn- prepare!
  "Synchronously prepare the cache for RECORD-ID from STORE's record.
   Stamps the recall as prepare-async! does (packet C strata)."
  [opts store record-id]
  (let [record (turn-store/read-record store record-id)]
    (#'dog/publish! record-id
                    (dog/stamp-recall (or (:memory-projects-dir opts)
                                          @#'dog/memory-projects-dir)
                                      (dog/recall (:source_text record) opts)
                                      record))))

(defn- memory-fact
  "The Prompt-line fact for a :memory segment — the same pure function
   transport.http/memory-prompt-fact delegates to."
  [segment]
  (dog/prompt-fact segment))

(defn- render-with-dog
  "Render with only the dog provider registered (the each-fixture cleared
   the registry)."
  [ctx]
  (dog/register!)
  (prompt-line/render ctx (prompt-line/providers)))

(deftest provider-renders-recalled-memories
  (let [store (tmp-store)
        record-id "turn-dogTEST1-s"]
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
        record-id "turn-dogTEST3-s"
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
        record-id "turn-dogTEST4-s"]
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

;; -------------------------------------------- merge pass (M-repl-circus,
;; claude-13 11-11): both queries always run; slots alternate 驴, keyword,
;; 驴, skipping duplicates; one source empty → the other fills.

(defn- fake-hit
  "A minimal text-search hit whose body is a claude-code memory."
  [project slug hook score]
  {:score score
   :entry {:evidence/body {:source "claude-code" :project project
                           :name slug :hook hook}}})

(defn- fake-response [hits]
  {:results (vec hits)})

(defn- split-request
  "Answer the 驴 query (contains LU-MARK) from lu hits, anything else
   from kw hits."
  [lu-mark lu-hits kw-hits]
  (fn [url]
    (if (and lu-mark (re-find (re-pattern lu-mark) url))
      (fake-response lu-hits)
      (fake-response kw-hits))))

(deftest merge-alternates-lu-and-keyword-slots
  ;; 驴 has two unseen hits, keywords two: the slots must interleave
  ;; 驴, keyword, 驴 — the regression this guards is 驴 filling every
  ;; slot first (UkLbCu) or a 驴 item displacing a keyword hit (vsC2tD).
  (let [request-fn (split-request "luterm"
                                  [(fake-hit "proj-a" "lu-one" "first lu memory hook" -1.0)
                                   (fake-hit "proj-a" "lu-two" "second lu memory hook" -2.0)]
                                  [(fake-hit "proj-b" "kw-one" "first kw memory hook" -1.5)
                                   (fake-hit "proj-b" "kw-two" "second kw memory hook" -2.5)])
        result (dog/recall "operator text with keywords"
                           {:request-fn request-fn :lu-terms ["luterm"]})]
    (is (nil? (:status result)))
    (is (= ["lu" "keywords" "lu"] (mapv :source (:items result)))
        "slots alternate 驴, keyword, 驴")
    (is (= ["proj-a/lu-one" "proj-b/kw-one" "proj-a/lu-two"]
           (mapv :memory (:items result))))
    (is (= "mixed" (:query-source result)))))

(deftest merge-fills-when-one-source-is-empty
  (let [request-fn (split-request "luterm"
                                  []
                                  [(fake-hit "proj-b" "kw-one" "first kw memory hook" -1.0)
                                   (fake-hit "proj-b" "kw-two" "second kw memory hook" -2.0)
                                   (fake-hit "proj-b" "kw-three" "third kw memory hook" -3.0)])
        result (dog/recall "operator text with keywords"
                           {:request-fn request-fn :lu-terms ["luterm"]})]
    (is (= ["keywords" "keywords" "keywords"] (mapv :source (:items result)))
        "驴 empty → keywords fill all slots")
    (is (= "keywords" (:query-source result))))
  (let [request-fn (split-request "luterm"
                                  [(fake-hit "proj-a" "lu-one" "first lu memory hook" -1.0)
                                   (fake-hit "proj-a" "lu-two" "second lu memory hook" -2.0)
                                   (fake-hit "proj-a" "lu-three" "third lu memory hook" -3.0)]
                                  [])
        result (dog/recall "operator text with keywords"
                           {:request-fn request-fn :lu-terms ["luterm"]})]
    (is (= ["lu" "lu" "lu"] (mapv :source (:items result)))
        "keywords empty → 驴 fills all slots")
    (is (= "lu" (:query-source result)))))

(deftest merge-skips-duplicates-across-sources
  ;; The same memory in both queries: 驴's copy takes the first slot, the
  ;; keyword copy is skipped, the next keyword item fills the second slot.
  (let [shared (fake-hit "proj-x" "shared" "found by both queries" -1.0)
        request-fn (split-request "luterm"
                                  [shared
                                   (fake-hit "proj-a" "lu-two" "second lu memory hook" -2.0)]
                                  [(fake-hit "proj-x" "shared" "found by both queries" -0.5)
                                   (fake-hit "proj-b" "kw-one" "first kw memory hook" -1.5)])
        result (dog/recall "operator text with keywords"
                           {:request-fn request-fn :lu-terms ["luterm"]})]
    (is (= ["proj-x/shared" "proj-b/kw-one" "proj-a/lu-two"]
           (mapv :memory (:items result)))
        "a duplicate never takes a second slot")
    (is (= "lu" (:source (first (:items result)))))))

(deftest prompt-fact-omits-the-empty-label
  ;; Joe's E-futon-questions turn (turn-RgkbTi, 2026-11-11) ended its
  ;; Prompt line in a bare `memory`. The rule: a turn with no memory
  ;; items leaves NO fact on the line (the absence is still recorded as
  ;; :dog_recall on the turn record), and a segment with items but no
  ;; header rebuilds the label from the items — never a bare `memory`.
  (is (nil? (dog/prompt-fact {:segment/id :memory
                              :segment/dog-recall {:status "none" :items []}}))
      "status none: omitted")
  (is (nil? (dog/prompt-fact {:segment/id :memory
                              :segment/dog-recall {:status "failed" :items []}}))
      "status failed: omitted")
  (is (nil? (dog/prompt-fact {:segment/id :memory
                              :segment/dog-recall {:items []}}))
      "no items and no status: omitted")
  (is (nil? (dog/prompt-fact {:segment/id :memory}))
      "no payload at all: omitted — the bare `memory` case")
  (is (= "memory: not ready"
         (dog/prompt-fact {:segment/id :memory
                           :segment/dog-recall {:status "not-ready" :items []}})))
  (is (= "memory ~voxterm/some-memory (the hook)"
         (dog/prompt-fact {:segment/id :memory
                           :segment/header "~voxterm/some-memory (the hook)"
                           :segment/dog-recall {:items [{:memory "x/y"}]}}))
      "a hit renders its header")
  (is (= "memory ~voxterm/some-memory"
         (dog/prompt-fact {:segment/id :memory
                           :segment/dog-recall
                           {:items [{:memory "-home-joe-code-voxterm/some-memory"
                                     :display "voxterm/some-memory"}]}}))
      "items without a header rebuild the label, never bare")
  (let [fact (dog/prompt-fact {:segment/id :memory
                               :segment/dog-recall {:status "none" :items []}})]
    (is (not (and (string? fact) (re-find #"^memory\s*$" fact)))
        "no rendering is ever a bare `memory`")))

;; -------------------------------------------- post-hook part 1: the use
;; check (M-repl-circus, claude-13 11-11). Fixtures turn-*.record.json are
;; real operator records of session ed69fb38 captured 2026-11-11: the
;; record's dog_recall and reply_text verbatim, plus a snapshot of the
;; recalled memories' bodies from ~/.claude/projects/<project>/memory/.

(def fixture-vsC2tD-record (delay (fixture "turn-vsC2tD.record.json")))
(def fixture-63aK24-record (delay (fixture "turn-63aK24.record.json")))

(defn- bodies-map
  "The fixture's :bodies with string handle keys (cheshire keywordizes
   JSON object keys; use-check's MEMORY-BODIES is keyed by the :memory
   handle string)."
  [fix]
  (into {} (map (fn [[k v]] [(if (namespace k)
                               (str (namespace k) "/" (name k))
                               (name k))
                             v]))
        (:bodies fix)))

(deftest use-check-on-the-first-live-turn
  ;; turn-vsC2tD ("OK please send pattern-index packet 3"): the reply
  ;; names all three recalled memories verbatim and says "I did put a
  ;; requisition line on the packet".
  (let [fix @fixture-vsC2tD-record
        result (dog/use-check fix (bodies-map fix))]
    (is (= "ok" (:status result)))
    (is (= ["-home-joe-code-voxterm/kimi-unique-requisition"
            "-home-joe-code/batch-packets-invite-corner-cutting"
            "-home-joe-code/kimi-bells-need-requisition"]
           (:surfaced-memory-ids result))
        "surfaced ids, in recall order — memory_contract/use-receipt names")
    (is (= (:surfaced-memory-ids result) (:used-memory-ids result))
        "all three named ⇒ used; used ⊆ surfaced")
    (doseq [{:keys [used kind evidence]} (:items result)]
      (is (true? used))
      (is (= "named" kind))
      (is (re-find #"reply names" evidence)))))

(deftest use-check-unused-item
  ;; turn-63aK24: plop-2026-tracker-upkeep was recalled but the reply
  ;; (resolving two outstanding items, checkpoint refresh) neither names
  ;; it nor shares a single distinctive bigram with its body.
  (let [fix @fixture-63aK24-record
        result (dog/use-check fix (bodies-map fix))]
    (is (= "ok" (:status result)))
    (is (= 1 (count (:items result))))
    (is (= {:memory "-home-joe-code-voxterm/plop-2026-tracker-upkeep"
            :used false :kind "unused"
            :evidence "no shared distinctive bigrams; not named"}
           (first (:items result))))
    (is (empty? (:used-memory-ids result)))))

(def fixture-WHNTlI-record (delay (fixture "turn-WHNTlI.record.json")))

(deftest use-check-echoed-without-naming
  ;; The REAL turn-vsC2tD reply with every mention of the recalled
  ;; slugs removed: it still restates kimi-unique-requisition (3 shared
  ;; bigrams, among them "kimi seat"), so the echo half catches it.
  (let [fix @fixture-vsC2tD-record
        handles (map :memory (get-in fix [:dog_recall :items]))
        stripped (reduce (fn [t h] (-> t (str/replace h "")
                                       (str/replace (peek (str/split h #"/")) "")))
                         (:reply_text fix) handles)
        record {:dog_recall (:dog_recall fix) :reply_text stripped}
        result (dog/use-check record (bodies-map fix))
        by-memory (into {} (map (juxt :memory identity)) (:items result))]
    (is (= "echoed" (get-in by-memory ["-home-joe-code-voxterm/kimi-unique-requisition" :kind])))
    (is (re-find #"\"kimi seat\""
                 (get-in by-memory ["-home-joe-code-voxterm/kimi-unique-requisition" :evidence])))))

(deftest use-check-one-generic-bigram-is-not-an-echo
  ;; turn-WHNTlI (claude-5, 2026-11-11 21:52), the first live turn after
  ;; d415a2093 loaded: the reply is about a music player in Joe's mode
  ;; line and shares only "mode line" with kinesis-vdrive-editing. At
  ;; threshold 1 it was recorded as echoed (used); it was not used.
  (let [fix @fixture-WHNTlI-record
        result (dog/use-check {:dog_recall (:dog_recall fix) :reply_text (:reply_text fix)}
                              (bodies-map fix))
        item (some #(when (= "-home-joe/kinesis-vdrive-editing" (:memory %)) %) (:items result))]
    (is (= "unused" (:kind item)))
    (is (re-find #"one shared bigram .*mode line" (:evidence item)))
    (is (empty? (:used-memory-ids result)))))

(deftest use-check-typed-absences
  (is (= "no-recall" (:status (dog/use-check {:reply_text "some reply"} {})))
      "no dog_recall items")
  (is (= "no-recall" (:status (dog/use-check {:dog_recall {:status "none" :items []}
                                             :reply_text "some reply"} {})))
      "a dog_recall typed absence is still no recall to check")
  (is (= "no-reply" (:status (dog/use-check {:dog_recall {:items [{:memory "a/b"}]}} {})))
      "no reply_text yet")
  (let [fix @fixture-vsC2tD-record
        result (dog/use-check (assoc fix :reply_text nil) (bodies-map fix))]
    (is (= "no-reply" (:status result)))
    (is (= 3 (count (:surfaced-memory-ids result)))
        "surfaced ids ride the no-reply absence")))

(deftest use-check-body-unavailable
  (let [result (dog/use-check {:dog_recall {:items [{:memory "proj/missing-slug"}]}
                               :reply_text "a real reply with distinctive words"}
                              {})]
    (is (= "unused" (:kind (first (:items result)))))
    (is (= "memory body unavailable; not named in reply"
           (:evidence (first (:items result)))))))

(deftest settled-event-writes-dog-use-on-the-record
  ;; The :settled turn event (which follows attach-happened!'s
  ;; :reply_text write) drives the post-hook: dog_use lands on the
  ;; operator record.
  (let [store (tmp-store)
        record-id "turn-useTEST1"
        fix @fixture-vsC2tD-record]
    (write-operator-record! store record-id "OK please send pattern-index packet 3")
    (turn-store/update-record! store record-id
                               #(assoc % :dog_recall (:dog_recall fix)
                                       :reply_text (:reply_text fix)))
    (with-redefs [dog/memory-body (fn ([handle] (get (bodies-map fix) handle))
                                    ([_dir handle] (get (bodies-map fix) handle)))]
      (dog/handle-event {:store store} {:event :settled :id record-id})
      (is (loop [n 0]
            (cond (:dog_use (turn-store/read-record store record-id)) true
                  (< 100 n) false
                  :else (do (Thread/sleep 50) (recur (inc n)))))
          "dog_use written off-path")
      (let [dog-use (:dog_use (turn-store/read-record store record-id))]
        (is (= "ok" (:status dog-use)))
        (is (= 3 (count (:used-memory-ids dog-use))))))
    ;; a record with no dog_recall is skipped entirely
    (let [record-id2 "turn-useTEST2"]
      (write-operator-record! store record-id2 "an ordinary turn")
      (dog/handle-event {:store store} {:event :settled :id record-id2})
      (Thread/sleep 300)
      (is (not (contains? (turn-store/read-record store record-id2) :dog_use))
          "no dog_recall → no dog_use"))))

;; -------------------------------------------- post-hook part 2a: bury
;; selection (M-repl-circus, claude-13 11-11). Fixtures turn-*.bury.json
;; are real operator records of session ed69fb38 captured 2026-11-11
;; (turn_id, created_at, source_text, reply_text, acts verbatim).

(def fixture-UkLbCu-bury (delay (fixture "turn-UkLbCu.bury.json")))
(def fixture-vDBr4L-bury (delay (fixture "turn-vDBr4L.bury.json")))
(def fixture-6MMa3B-bury (delay (fixture "turn-6MMa3B.bury.json")))
(def fixture-PeSYc2-bury (delay (fixture "turn-PeSYc2.bury.json")))

(deftest bury-selects-rulings-not-status
  ;; turn-UkLbCu: Joe's :approve authorises chained/parallel dispatches
  ;; (a direction worth burying); his :report ("I minted codex-7 as a
  ;; helper") is a status note, not buried; the agent's own acts never
  ;; qualify.
  (let [candidates (dog/bury-candidates @fixture-UkLbCu-bury)]
    (is (= 1 (count candidates)))
    (let [{:keys [slug description body basis]} (first candidates)]
      (is (re-find #"(?i)chained" body) "the authorisation, verbatim")
      (is (re-find #"^status: buried by 狗 2026-11-11 from turn-UkLbCu, not yet used" body)
          "the body opens with the status line")
      (is (re-find #"^joe-2026-11-11-" slug) "who + date slug")
      (is (>= 79 (count description)) "description fits a memory hook")
      (is (= {:record "turn-UkLbCu" :act-index 0
              :act-id "claude-13-turn-323-f-s1-0" :kind "approve"}
             basis)
          "basis cites the record id and the act index"))
    (is (not-any? #(re-find #"(?i)minted codex-7" (str (:body %))) candidates)
        "status reports are not buried")))

(deftest bury-skips-questions
  ;; turn-6MMa3B: the :ask-action act "Isn't this a little bit like beam
  ;; search?" is a question, not a direction; the :propose ("maybe
  ;; there's a pager-like mechanism") is a suggestion, not a ruling.
  (is (= [] (dog/bury-candidates @fixture-6MMa3B-bury))))

(deftest bury-catches-the-example-ruling
  ;; turn-PeSYc2 (cited in claude-13's requisition): "proposal recurs is
  ;; enough for me to accept it" is an :accept ruling; the same turn's
  ;; :constrain keeps the GCC entries *proposed*.
  (let [candidates (dog/bury-candidates @fixture-PeSYc2-bury)
        bodies (map :body candidates)]
    (is (some #(re-find #"proposal recurs" %) bodies) "the accept ruling")
    (is (some #(re-find #"retain those as \*proposed\*" %) bodies) "the constrain ruling")
    (is (not-any? #(re-find #"go ahead" %) bodies)
        "the bare 🈸 go-ahead is assent, not content (2b: 2 stripped terms < 5)")))

(deftest bury-retraction-after-joes-disagree
  ;; turn-vDBr4L: Joe's 🈚 disagree ("done for now but not done-done")
  ;; and the reply's 🈹 self-correction — both buried.
  (let [candidates (dog/bury-candidates @fixture-vDBr4L-bury)
        by-kind (group-by #(get-in % [:basis :kind]) candidates)]
    (is (= 1 (count (get by-kind "disagree"))) "Joe's disagree ruling")
    (is (= 1 (count (get by-kind "retract"))) "the reply's 🈹 retraction")
    (let [retraction (first (get by-kind "retract"))]
      (is (re-find #"🈹" (:body retraction)) "verbatim 🈹 paragraph")
      (is (= "turn-vDBr4L" (get-in retraction [:basis :record]))))))

(deftest bury-suppresses-what-minus40-already-returns
  ;; :known-fn is the same text-search the recall uses: a candidate
  ;; minus40 already covers is not buried again.
  (let [candidates (dog/bury-candidates @fixture-UkLbCu-bury
                                        {:known-fn (fn [_] ["-home-joe-code/existing-memory"])})]
    (is (= [] candidates) "already-known ruling suppressed"))
  (let [candidates (dog/bury-candidates @fixture-UkLbCu-bury
                                        {:known-fn (fn [_] nil)})]
    (is (= 1 (count candidates)) "unknown ruling still buried")))

;; -------------------------------------------- post-hook part 2b: the bury
;; write (M-repl-circus, claude-13 11-11). Rule changes from review:
;; content-tested approvals, 2-bigram dedup, real writes to a temp dir.

(def fixture-vsC2tD-bury (delay (fixture "turn-vsC2tD.bury.json")))
(def fixture-63aK24-bury (delay (fixture "turn-63aK24.bury.json")))
(def fixture-RgkbTi-bury (delay (fixture "turn-RgkbTi.bury.json")))

(deftest approvals-need-content-beyond-assent
  ;; The 14-row table's praise / bare go-aheads are no longer buried:
  ;; vsC2tD "OK please send pattern-index packet 3" (3 stripped terms),
  ;; 63aK24's praise (3), RgkbTi's praise (7 terms but no directive
  ;; marker) — while RgkbTi's :redirect ruling still is.
  (is (= [] (dog/bury-candidates @fixture-vsC2tD-bury))
      "bare go-ahead: not buried")
  (is (= [] (dog/bury-candidates @fixture-63aK24-bury))
      "praise and a question: nothing buried")
  (let [candidates (dog/bury-candidates @fixture-RgkbTi-bury)]
    (is (= 1 (count candidates)))
    (is (= "redirect" (get-in (first candidates) [:basis :kind]))
        "the excursion ruling (redirect) is buried; the praise (approve) is not")
    (is (re-find #"E-futon-questions" (:body (first candidates)))))
  ;; and the two must-bury approvals from the table still pass
  (is (= 1 (count (dog/bury-candidates @fixture-UkLbCu-bury)))
      "UkLbCu's dispatch authorisation survives (6 terms, marker continue)")
  (is (some #(re-find #"proposal recurs" (:body %))
            (dog/bury-candidates @fixture-PeSYc2-bury))
      "PeSYc2's acceptance ruling survives (7 terms, markers enough/accept/should)"))

(deftest known-memories-needs-two-shared-bigrams
  ;; 2b dedup: one shared distinctive bigram is not enough (the 2a
  ;; probe suppressed the vDBr4L retraction on the generic "question
  ;; mode"); echo-min-shared is 2.
  (let [hit {:score -1.0
             :entry {:evidence/body {:source "claude-code" :project "proj"
                                     :name "existing" :hook "an existing memory hook"}}}
        request-fn (fn [_url] {:results [hit]})
        one-shared "progressive widening differs entirely from alpha-beta pruning folklore"
        two-shared "progressive widening keeps the search informed"]
    (with-redefs [dog/memory-body
                  (fn ([_] "progressive widening keeps every move reachable eventually and stays narrow")
                    ([_ _] "progressive widening keeps every move reachable eventually and stays narrow"))]
      (is (= [] (dog/known-memories request-fn "http://127.0.0.1:7093" one-shared))
          "a one-bigram overlap does not suppress")
      (is (= ["proj/existing"]
             (dog/known-memories request-fn "http://127.0.0.1:7093" two-shared))
          "two shared bigrams suppress"))))

(deftest bury-store-docs-shape-matches-a-real-stored-document
  ;; Pin against a REAL stored document: fixture
  ;; store-evidence-kimi-unique-requisition.json is the :7093 entry for
  ;; cc:-home-joe-code-voxterm/kimi-unique-requisition, fetched by GET
  ;; text-search 2026-11-11. bury-store-docs re-implements
  ;; ingest_hook.py's evidence_doc/version_doc (the entry points that
  ;; write them also write hook state under ~/code/data/minus40, which
  ;; this packet may not touch).
  (let [real (fixture "store-evidence-kimi-unique-requisition.json")
        candidate {:slug "joe-2026-11-11-test-slug" :description "a test candidate"
                   :body "status: buried by 狗 2026-11-11 from turn-TEST, not yet used\n\nquoted"}
        text (dog/bury-file-text candidate)
        [evidence version] (dog/bury-store-docs candidate text 1794430000000)]
    (is (= (set (keys real)) (set (keys evidence))) "document-level keys")
    (is (= :memory (:evidence/type evidence)))
    (is (= :assert (:evidence/claim-type evidence)))
    (is (= "minus40-import" (:evidence/author evidence)) "ingest_hook's author")
    (is (= {:ref/type :memory :ref/id "cc:狗/joe-2026-11-11-test-slug"}
           (:evidence/subject evidence)))
    (is (= [:memory :memory/assert :source/claude-code :memory/version]
           (:evidence/tags evidence)))
    (let [real-body-keys (set (keys (:evidence/body real)))
          my-body (:evidence/body evidence)]
      (is (every? #(contains? my-body %) real-body-keys)
          "every body key of the real stored document is present"))
    (is (= "claude-code" (get-in evidence [:evidence/body :source]))
        "what the recall's memory-item filter matches")
    (is (= "buried" (get-in evidence [:evidence/body :kind]))
        "distinguishable as buried by 狗, not yet used")
    ;; the version hyperedge (ingest_hook's version_doc)
    (is (= :memory/version (:hx/type version)))
    (is (= ["cc:狗/joe-2026-11-11-test-slug" (:evidence/id evidence)]
           (:hx/endpoints version)))
    (is (re-find #"^2026-11-11T" (:hx/valid-time version)))))

(deftest bury-one-writes-file-and-ingests-never-overwrites
  (let [dir (io/file (.toFile (Files/createTempDirectory "dog-bury" (into-array FileAttribute [])))
                     "狗" "memory")
        ingested (atom [])
        candidate {:slug "joe-2026-11-11-happy-continue-remaining-packets-chained-parallel"
                   :description "Joe 2026-11-11 (approve, turn-UkLbCu): chained or parallel"
                   :body "status: buried by 狗 2026-11-11 from turn-UkLbCu, not yet used\n\n\"…chained or parallel dispatches\""}
        opts {:bury-dir (str dir)
              :ingest-fn (fn [cand text ms] (swap! ingested conj [cand text ms]) [200 200])}
        first-result (dog/bury-one! opts candidate)]
    (is (= "buried" (:status first-result)))
    (is (= 1 (count @ingested)) "the store docs post at once, with the file")
    (let [text (slurp (io/file dir (str (:slug candidate) ".md")))]
      (is (re-find #"(?m)^  type: buried$" text) "frontmatter type: buried")
      (is (re-find #"(?m)^status: buried by 狗 2026-11-11 from turn-UkLbCu, not yet used$" text))
      (is (not (re-find #"MEMORY" text))))
    (is (not (.exists (io/file dir "MEMORY.md"))) "no MEMORY.md in the 狗 directory")
    ;; never overwrite
    (spit (io/file dir (str (:slug candidate) ".md")) "sentinel")
    (let [second-result (dog/bury-one! opts candidate)]
      (is (= "exists" (:status second-result)))
      (is (= "sentinel" (slurp (io/file dir (str (:slug candidate) ".md"))))
          "an existing slug is skipped, file untouched")
      (is (= 1 (count @ingested)) "no second ingest"))))

(deftest settled-event-writes-dog-buried
  ;; End to end at :settled (temp bury dir, recorded ingest, no real
  ;; store): UkLbCu's authorisation is buried and recorded as
  ;; :dog_buried on the record.
  (let [store (tmp-store)
        record-id "turn-buryTEST1"
        dir (str (Files/createTempDirectory "dog-bury" (into-array FileAttribute [])))
        ingested (atom [])
        fix @fixture-UkLbCu-bury]
    (write-operator-record! store record-id (:source_text fix))
    (turn-store/update-record! store record-id
                               #(assoc % :acts (vec (:acts fix))
                                       :reply_text (:reply_text fix)
                                       :dog_recall {:status "none" :items []}))
    (dog/handle-event {:store store
                       :bury-dir dir
                       :ingest-fn (fn [_cand _text _ms] (swap! ingested conj true) [200 200])
                       :request-fn (fn [_url] {:results []})}
                      {:event :settled :id record-id})
    (is (loop [n 0]
          (cond (:dog_buried (turn-store/read-record store record-id)) true
                (< 100 n) false
                :else (do (Thread/sleep 50) (recur (inc n)))))
        "dog_buried written off-path")
    (let [buried (:dog_buried (turn-store/read-record store record-id))]
      (is (= "ok" (:status buried)))
      (is (= 1 (count (:items buried))))
      (is (= "buried" (:status (first (:items buried)))))
      (is (.isFile (io/file (get-in buried [:items 0 :path])))))
    (is (= 1 (count @ingested)))))

(deftest bury-off-switch-stops-only-burying
  (let [off (io/file (.toFile (Files/createTempDirectory "dog-bury-off" (into-array FileAttribute [])))
                     "dog-bury-off")]
    (.createNewFile off)
    (binding [dog/*bury-off-switch-path* (str off)]
      (let [store (tmp-store)
            record-id "turn-buryTEST2"
            fix @fixture-UkLbCu-bury]
        (write-operator-record! store record-id (:source_text fix))
        (turn-store/update-record! store record-id #(assoc % :acts (vec (:acts fix))))
        (dog/handle-event {:store store :request-fn (fn [_url] {:results []})}
                          {:event :settled :id record-id})
        (is (loop [n 0]
              (cond (:dog_buried (turn-store/read-record store record-id)) true
                    (< 100 n) false
                    :else (do (Thread/sleep 50) (recur (inc n))))))
        (is (= "off" (:status (:dog_buried (turn-store/read-record store record-id))))
            "burying stops; the record says so")))))

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

(deftest bury-quotes-the-full-act-not-its-truncated-text
  ;; turn-RgkbTi: the act's :text is cut at 120 chars ("... b/c it
  ;; onl…"); the buried quote is the act's full span of :source_text.
  (let [rec @fixture-RgkbTi-bury
        c (some #(when (= "redirect" (get-in % [:basis :kind])) %)
                (dog/bury-candidates rec))]
    (is (some? c))
    (is (str/includes? (:body c) "b/c it only came up in passing just now.\""))
    (is (not (str/includes? (:body c) "onl…")))))

;; -------------------------------------------- E-dog-helps packet B: the arm
;; §2 "Design: recalled but withheld": the arm is a pure function of the
;; record id; a withheld turn's recall is recorded with items and :arm
;; "withheld" but the Prompt line carries no memory fact; use and bury run
;; as on a shown turn.

(deftest arm-known-ids
  ;; Arms computed 2026-11-11 over 20,358 real record ids under
  ;; ~/.emacs-graph/session-turn-analysis: 6,096 withheld = 29.9% (the 30%
  ;; design rate is a property report, not a test).
  (is (= "shown" (dog/arm "turn-05cXMM")))
  (is (= "withheld" (dog/arm "turn-1jgPTg")))
  (is (= "shown" (dog/arm "turn-RgkbTi")))
  (is (= (dog/arm "turn-05cXMM") (dog/arm "turn-05cXMM")) "reproducible"))

(deftest arm-off-switch-shows-everything
  (let [off (io/file (.toFile (Files/createTempDirectory "dog-arm-off"
                                                          (into-array FileAttribute [])))
                     "dog-arm-off")]
    (.createNewFile off)
    (binding [dog/*arm-off-switch-path* (str off)]
      (is (= "shown-arm-off" (dog/arm-for-turn "turn-1jgPTg"))
          "a hash-withheld id is shown, marked so the analysis can drop it"))))

(deftest withheld-turn-records-items-and-renders-nothing
  (let [store (tmp-store)
        record-id "turn-1jgPTg"] ; hash-withheld (see arm-known-ids)
    (write-operator-record! store record-id "benefits have a time-limited nature")
    (prepare! {:request-fn (stub-request [@fixture-05cXMM])} store record-id)
    (let [result (render-with-dog {:agent-id "kimi-1" :session-id "sess-test"
                                   :turn-record-id record-id
                                   :render-at "2026-11-11T18:30:00Z"})
          segment (first (:segments result))
          recall (:segment/dog-recall segment)]
      (is (seq (:items recall)) "the recall has items as on a shown turn")
      (is (= "withheld" (:arm recall)) "dog_recall says the arm")
      (is (nil? (:segment/header segment)) "no header, so nothing renders")
      (is (nil? (memory-fact segment))
          "no memory fact on the Prompt line — the seat is not told")))
  ;; and the recorded dog_recall on the 象 turn record carries the arm
  (let [store (tmp-store)
        record-id "turn-1jgPTg"]
    (write-operator-record! store record-id "benefits have a time-limited nature")
    (prepare! {:request-fn (stub-request [@fixture-05cXMM])} store record-id)
    (dog/register!)
    (binding [prompt-line/*turn-record-store* store]
      (prompt-line/render! {:agent-id "kimi-1" :session-id "sess-test"
                            :turn-record-id record-id}))
    (let [dog-recall (:dog_recall (turn-store/read-record store record-id))]
      (is (map? dog-recall))
      (is (seq (:items dog-recall)))
      (is (= "withheld" (:arm dog-recall))))))

(deftest use-check-runs-on-a-withheld-turn
  ;; The real turn-vsC2tD fixture with its dog_recall marked withheld:
  ;; the use check is arm-independent by construction.
  (let [fix @fixture-vsC2tD-record
        record (assoc-in fix [:dog_recall :arm] "withheld")
        result (dog/use-check record (bodies-map fix))]
    (is (= "ok" (:status result)))
    (is (= 3 (count (:used-memory-ids result)))
        "same use verdict as the shown fixture")))

;; -------------------------------------------- E-dog-helps packet D:
;; bury-split keeps what minus40 already knows as :said-again events, and
;; dog_buried records them at :settled (empty when none), arm-independent.

(deftest bury-split-keeps-the-suppression
  ;; turn-UkLbCu's approve ruling, matched by an existing memory: kept
  ;; empty, and the suppression is recorded with the act, Joe's words,
  ;; and the matching memory ids + shared bigrams.
  (let [{:keys [kept suppressed]}
        (dog/bury-split @fixture-UkLbCu-bury
                        {:known-detail-fn
                         (fn [_] [{:memory "-home-joe-code/existing-memory"
                                   :shared [["dispatch" "authorisation"]
                                            ["chained" "parallel"]]}])})]
    (is (= [] kept) "already-known ruling is not buried again")
    (is (= 1 (count suppressed)))
    (let [entry (first suppressed)]
      (is (= "claude-13-turn-323-f-s1-0" (:act entry)) "the act id")
      (is (= "approve" (:kind entry)))
      (is (re-find #"(?i)chained" (:text entry)) "Joe's words, full span")
      (is (= ["-home-joe-code/existing-memory"] (:memory-ids entry)))
      (is (= #{"chained parallel" "dispatch authorisation"}
             (set (map #(str/join " " %) (:shared entry))))))))

(deftest said-again-respects-the-two-bigram-threshold
  ;; The 2a near-miss (vDBr4L's retraction suppressed on the generic
  ;; "question mode"): through the production known-memories-detailed
  ;; path, a one-bigram overlap does NOT suppress, so no said-again.
  (let [hit {:score -1.0
             :entry {:evidence/body {:source "claude-code" :project "proj"
                                     :name "existing" :hook "question mode"}}}
        request-fn (fn [_url] {:results [hit]})]
    (with-redefs [dog/memory-body
                  (fn ([_] "question mode folklore, otherwise unrelated")
                    ([_ _] "question mode folklore, otherwise unrelated"))]
      (let [detailed (dog/known-memories-detailed request-fn "http://127.0.0.1:7093"
                                                  "question mode")
            {:keys [suppressed]} (dog/bury-split @fixture-vDBr4L-bury
                                                 {:known-detail-fn
                                                  (fn [text]
                                                    (dog/known-memories-detailed
                                                     request-fn "http://127.0.0.1:7093" text))})]
        (is (= [] detailed) "one shared bigram is below echo-min-shared")
        (is (= [] suppressed) "the near-miss is not recorded as said-again")
        (is (some #(= "retract" (get-in % [:basis :kind]))
                  (dog/bury-candidates @fixture-vDBr4L-bury))
            "the retraction still buries")))))

(deftest settled-event-records-said-again-and-writes-no-file
  ;; End to end at :settled: UkLbCu's ruling matches an existing memory
  ;; (two shared distinctive bigrams), so nothing is buried or ingested
  ;; and dog_buried carries the said-again entry instead.
  (let [store (tmp-store)
        record-id "turn-buryTEST3"
        dir (str (Files/createTempDirectory "dog-bury" (into-array FileAttribute [])))
        ingested (atom [])
        fix @fixture-UkLbCu-bury
        hit {:score -1.0
             :entry {:evidence/body {:source "claude-code" :project "proj"
                                     :name "existing" :hook "an existing memory"}}}
        request-fn (fn [_url] {:results [hit]})]
    (write-operator-record! store record-id (:source_text fix))
    (turn-store/update-record! store record-id
                               #(assoc % :acts (vec (:acts fix))
                                       :reply_text (:reply_text fix)
                                       :dog_recall {:status "none" :items []}))
    (with-redefs [dog/memory-body
                  ;; two distinctive bigrams of the ruling's text
                  ;; (\"chained parallel\", \"parallel dispatches\")
                  (fn ([_] "chained parallel dispatches, held already")
                    ([_ _] "chained parallel dispatches, held already"))]
      (dog/handle-event {:store store
                         :bury-dir dir
                         :ingest-fn (fn [_cand _text _ms] (swap! ingested conj true) [200 200])
                         :request-fn request-fn}
                        {:event :settled :id record-id})
      ;; the post-hook is async; wait inside the with-redefs scope
      (is (loop [n 0]
            (cond (:dog_buried (turn-store/read-record store record-id)) true
                  (< 100 n) false
                  :else (do (Thread/sleep 50) (recur (inc n)))))))
    (let [buried (:dog_buried (turn-store/read-record store record-id))]
      (is (= "none" (:status buried)) "nothing left to bury")
      (is (= [] (:items buried)))
      (is (= 1 (count (:said-again buried))) "the suppression is recorded")
      (let [entry (first (:said-again buried))]
        (is (= "claude-13-turn-323-f-s1-0" (:act entry)))
        (is (= ["proj/existing"] (:memory-ids entry)))
        (is (seq (:shared entry)))))
    (is (empty? @ingested) "no store write for a suppressed candidate")
    (is (not (.exists (io/file dir "狗"))) "no memory file written")))

(deftest said-again-empty-on-a-plain-turn
  ;; Every settled turn carries :said-again, empty when nothing matched.
  (let [store (tmp-store)
        record-id "turn-buryTEST4"
        dir (str (Files/createTempDirectory "dog-bury" (into-array FileAttribute [])))
        fix @fixture-UkLbCu-bury]
    (write-operator-record! store record-id (:source_text fix))
    (turn-store/update-record! store record-id
                               #(assoc % :acts (vec (:acts fix))
                                       :reply_text (:reply_text fix)
                                       :dog_recall {:status "none" :items []}))
    (dog/handle-event {:store store
                       :bury-dir dir
                       :ingest-fn (fn [_c _t _m] [200 200])
                       :request-fn (fn [_url] {:results []})}
                      {:event :settled :id record-id})
    (is (loop [n 0]
          (cond (:dog_buried (turn-store/read-record store record-id)) true
                (< 100 n) false
                :else (do (Thread/sleep 50) (recur (inc n))))))
    (let [buried (:dog_buried (turn-store/read-record store record-id))]
      (is (= "ok" (:status buried)))
      (is (= [] (:said-again buried))))))

;; -------------------------------------------- E-dog-helps packet C: strata
;; Each recalled item is stamped at recall time with :project, :type,
;; :in-seat-index, :age-days.  Fixtures here are INVENTED frontmatter-only
;; stubs written into a temp dir (public-repo rule: no memory file body is
;; ever copied); the turn records are the real dog fixtures.

(defn- tmp-projects-with-memory
  "A temp projects root: proj/memory/stubbed.md (frontmatter-only stub)
   and proj-seat/<session>.jsonl + MEMORY.md mentioning stubbed.md when
   LISTED? — returns the root path."
  [session listed?]
  (let [root (.toFile (Files/createTempDirectory "dog-projects" (into-array FileAttribute [])))
        memdir (io/file root "proj" "memory")
        seatdir (io/file root "proj-seat")]
    (.mkdirs memdir)
    (.mkdirs seatdir)
    (spit (io/file memdir "stubbed.md")
          (str "---\nname: stubbed\ndescription: \"an invented stub memory for packet C tests\"\n"
               "metadata:\n  node_type: memory\n  type: feedback\n  modified: 2026-10-04T14:46:35.701Z\n---\n\nstub body.\n"))
    (spit (io/file seatdir (str session ".jsonl")) "{}")
    (spit (io/file seatdir "MEMORY.md")
          (if listed? "- [an invented stub](memory/stubbed.md)\n" "- [something else](memory/other.md)\n"))
    (str root)))

(deftest stamp-item-strata
  (let [root (tmp-projects-with-memory "sess-claude" true)
        item {:memory "proj/stubbed" :display "proj/stubbed" :score 1.0 :why "w"}
        record {:agent_id "claude-13" :session_id "sess-claude"}
        stamped (dog/stamp-item root item record)]
    (is (= "proj" (:project stamped)))
    (is (= "feedback" (:type stamped)) "frontmatter metadata.type")
    (is (true? (:in-seat-index stamped)) "the seat's MEMORY.md lists the slug")
    (is (number? (:age-days stamped)))
    (let [expected (quot (- (System/currentTimeMillis)
                            (.toEpochMilli (java.time.Instant/parse "2026-10-04T14:46:35.701Z")))
                         86400000)]
      (is (= expected (:age-days stamped))
          "frontmatter modified, not file mtime, drives the age"))))

(deftest stamp-item-in-seat-index-variants
  (let [item {:memory "proj/stubbed"}]
    (testing "MEMORY.md without the slug → false"
      (let [root (tmp-projects-with-memory "sess-a" false)]
        (is (false? (:in-seat-index (dog/stamp-item root item {:agent_id "claude-13" :session_id "sess-a"}))))))
    (testing "no transcript for the session → \"unknown\""
      (let [root (tmp-projects-with-memory "sess-a" true)]
        (is (= "unknown" (:in-seat-index (dog/stamp-item root item {:agent_id "claude-13" :session_id "sess-nope"}))))))
    (testing "non-Claude seat → \"n/a\""
      (let [root (tmp-projects-with-memory "sess-a" true)]
        (is (= "n/a" (:in-seat-index (dog/stamp-item root item {:agent_id "kimi-2" :session_id "sess-a"}))))))))

(deftest stamp-item-no-file
  (let [root (tmp-projects-with-memory "sess-a" true)
        stamped (dog/stamp-item root {:memory "proj/missing"} {:agent_id "claude-13" :session_id "sess-a"})]
    (is (= "unknown" (:type stamped)) "no file → unknown type")
    (is (nil? (:age-days stamped)))))

(deftest recall-items-carry-strata
  ;; The real recall path (fixture turn-05cXMM's recorded m40 answer):
  ;; every item in the cached recall is stamped.
  (let [store (tmp-store)
        record-id "turn-dogTEST1-s"
        root (tmp-projects-with-memory "sess-test" false)]
    (write-operator-record! store record-id "benefits have a time-limited nature")
    (prepare! {:request-fn (stub-request [@fixture-05cXMM])
               :memory-projects-dir root} store record-id)
    (let [items (:items (dog/cached-recall record-id))]
      (is (seq items))
      (doseq [item items]
        (is (contains? item :project))
        (is (contains? item :type))
        (is (contains? item :in-seat-index))
        (is (contains? item :age-days))
        ;; none of the fixture's real handles exist under the temp root
        (is (= "unknown" (:type item)))))))
