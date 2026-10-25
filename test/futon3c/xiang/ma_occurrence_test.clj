(ns futon3c.xiang.ma-occurrence-test
  "馬's per-pattern occurrence verdict: window selection, the verbatim-quote
  and window checks that void a bad map, the status counts, and the typed
  no-reader absence. Synthetic text only; the pattern parts come from a
  temp library so no real pattern file is read. Run:
  clojure -M:test -n futon3c.xiang.ma-occurrence-test"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.xiang.ma-occurrence :as mo]
            [futon3c.xiang.turn-service :as turn-service]
            [futon3c.xiang.turn-store :as ts]))

(defn- temp-store []
  (ts/store (str (java.nio.file.Files/createTempDirectory
                  "ma-occurrence-" (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- temp-library []
  (let [dir (java.nio.file.Files/createTempDirectory
             "ma-occurrence-lib-" (make-array java.nio.file.attribute.FileAttribute 0))
        f (java.io.File. (str (.toFile dir)) "ifr-pattern.flexiarg")]
    (spit (.getPath f)
          "  + IF: the operator commits to one intent before delegating\n  + HOWEVER: the reply splits the work across several sessions\n\n  + BECAUSE: the stack records the intent at the head of the chain\n\n  + THEN: every later turn reads the intent before it acts\n")
    (.getPath (.getParentFile f))))

(def pid "test-family/ifr-pattern")

(defn- record!
  "Write (or merge onto) a crafted record at deterministic ID; returns the store."
  [store id over]
  (let [record (merge {:origin "operator" :agent_id "claude-3"
                       :session_id "sess-1" :turn_id id
                       :created_at "2026-10-24T00:00:00Z"
                       :source_text "operator text of the default record"
                       :reply_text "reply text of the default record"}
                      over)]
    (if (ts/read-record store id)
      (ts/update-record! store id #(merge % record))
      (ts/write-record-id! store id record)))
  store)

(defn- base-records [store]
  ;; six operator turns, a system (session-end) record and an other-session
  ;; record interleaved, so window selection has something to exclude.
  (doseq [[id at] [["turn-old1" "2026-10-24T00:00:00Z"]
                   ["turn-old2" "2026-10-24T00:01:00Z"]
                   ["turn-old3" "2026-10-24T00:02:00Z"]
                   ["turn-old4" "2026-10-24T00:03:00Z"]
                   ["turn-new" "2026-10-24T00:04:00Z"]]]
    (record! store id {:created_at at
                       :source_text (str "operator words of " id " that can be quoted at length")
                       :reply_text (str "reply words of " id " that can be quoted at length")}))
  (record! store "turn-system" {:origin "system" :created_at "2026-10-24T00:03:30Z"})
  (record! store "turn-other" {:session_id "sess-2" :created_at "2026-10-24T00:03:45Z"})
  (record! store "turn-bell" {:origin "agent" :created_at "2026-10-24T00:03:50Z"})
  store)

(defn- q [id who] ;; a 5+ word verbatim quote of record ID's text
  (str who " words of " id " that can be quoted at length"))

(defn- map-of [element id who quote]
  {:element element :turn id :quote quote :who who})

;; ------------------------------------------------------------------ window

(deftest window-selects-earlier-operator-turns-of-the-session-only
  (let [store (base-records (temp-store))]
    (is (= ["turn-old3" "turn-old4" "turn-new"]
           (mapv :id (mo/chain-window store "turn-new" 3)))
        "n=3: the record plus its two immediately earlier operator turns")
    (is (= ["turn-new"] (mapv :id (mo/chain-window store "turn-new" 1)))
        "n=1: the record alone")
    (is (not (some #{"turn-system" "turn-other" "turn-bell"}
                   (mapv :id (mo/chain-window store "turn-new" 9))))
        "system, bell and other-session records are excluded")
    (is (= ["turn-old1" "turn-old2" "turn-old3" "turn-old4" "turn-new"]
           (mapv :id (mo/chain-window store "turn-new" 9)))
        "n=9: all five operator turns of the session, ending at the record")))

;; ------------------------------------------------------------- the checks

(defn- ask-reply [rows] (json/generate-string rows))

(defn- run-with [store id ask]
  (mo/verdict! {:store store :library (temp-library) :ask ask :seat "馬-sonnet"} id))

(defn- verdicts [store id]
  (:pattern_verdicts (ts/read-record store id)))

(deftest a-wrong-quote-is-voided-with-its-reason
  (let [store (base-records (temp-store))]
    (record! store "turn-new"
             {:created_at "2026-10-24T00:04:00Z"
              :source_text (q "turn-new" "operator")
              :reply_text (q "turn-new" "reply")
              :proposed_patterns [{:patterns [pid] :at "x" :job_id "j"}]})
    (run-with store "turn-new"
              (constantly
               (ask-reply
                [{:pattern pid
                  :turn {:I1 (map-of "intent" "turn-new" "operator"
                                     (str "these words appear nowhere in the record at all"))}
                  :chain {:I1 (map-of "intent" "turn-new" "operator" (q "turn-new" "operator"))
                          :I2 (map-of "split" "turn-new" "reply" (q "turn-new" "reply"))
                          :G (map-of "head" "turn-old4" "operator" (q "turn-old4" "operator"))
                          :B (map-of "read" "turn-old4" "reply" (q "turn-old4" "reply"))}}])))
    (let [p (first (:patterns (verdicts store "turn-new")))]
      (is (= "quote-not-found" (get-in p [:turn :I1 :why]))
          "the planted wrong quote is voided, named")
      (is (seq (get-in p [:turn :I1 :attempt :quote]))
          "the failed attempt is kept, not dropped")
      (is (= 0 (:turn_mapped p)))
      (is (= 4 (:chain_mapped p)))
      (is (= "occurrence" (:status p))))))

(deftest a-quote-from-a-record-outside-the-window-is-voided
  (let [store (base-records (temp-store))]
    ;; window n=1: only turn-new is allowed; turn-old4 is verbatim-quotable
    ;; but outside the window.
    (record! store "turn-new"
             {:created_at "2026-10-24T00:04:00Z"
              :source_text (q "turn-new" "operator")
              :reply_text (q "turn-new" "reply")
              :proposed_patterns [{:patterns [pid] :at "x" :job_id "j"}]})
    (mo/verdict! {:store store :library (temp-library) :window-n 1
              :ask (constantly
                    (ask-reply
                     [{:pattern pid
                       :turn {:I1 (map-of "intent" "turn-new" "operator" (q "turn-new" "operator"))}
                       :chain {:I1 (map-of "intent" "turn-new" "operator" (q "turn-new" "operator"))
                               :I2 (map-of "split" "turn-old4" "operator" (q "turn-old4" "operator"))}}]))
             }
             "turn-new")
    (let [p (first (:patterns (verdicts store "turn-new")))]
      (is (= "record-outside-window" (get-in p [:chain :I2 :why]))
          "the outside-window map is voided, named")
      (is (= 1 (:chain_mapped p)))
      (is (= "partial" (:status p)))
      (is (= ["turn-new"] (:window (verdicts store "turn-new")))))))

(deftest status-counts-follow-the-chain-window
  (let [store (base-records (temp-store))]
    (record! store "turn-new"
             {:created_at "2026-10-24T00:04:00Z"
              :source_text (q "turn-new" "operator")
              :reply_text (q "turn-new" "reply")
              :proposed_patterns [{:patterns [pid "~test-family/second" "~test-family/second"]
                                   :at "x" :job_id "j"}]})
    (run-with store "turn-new"
              (constantly
               (ask-reply
                [{:pattern pid
                  :turn {:I1 (map-of "intent" "turn-new" "operator" (q "turn-new" "operator"))
                         :B (map-of "read" "turn-new" "reply" (q "turn-new" "reply"))}
                  :chain {:I1 (map-of "intent" "turn-new" "operator" (q "turn-new" "operator"))
                          :I2 (map-of "split" "turn-new" "reply" (q "turn-new" "reply"))
                          :G (map-of "head" "turn-old4" "operator" (q "turn-old4" "operator"))
                          :B (map-of "read" "turn-old4" "reply" (q "turn-old4" "reply"))}}
                 {:pattern "test-family/second"
                  :turn {}
                  :chain {:G (map-of "head" "turn-old3" "operator" (q "turn-old3" "operator"))}}])))
    (let [[p1 p2] (:patterns (verdicts store "turn-new"))]
      (is (= 2 (:turn_mapped p1)))
      (is (= 4 (:chain_mapped p1)))
      (is (= "occurrence" (:status p1)))
      (is (= 0 (:turn_mapped p2)))
      (is (= 1 (:chain_mapped p2)))
      (is (= "partial" (:status p2)))
      (is (str/starts-with? (get-in p2 [:turn :I1 :why]) "map-missing")
          "a missing window map voids with map-missing, not dropped"))))

(deftest no-reader-is-typed-not-defaulted
  (let [store (base-records (temp-store))]
    (record! store "turn-new"
             {:created_at "2026-10-24T00:04:00Z"
              :source_text (q "turn-new" "operator")
              :reply_text (q "turn-new" "reply")
              :proposed_patterns [{:patterns [pid] :at "x" :job_id "j"}]})
    ;; :ask nil (seat not wired) and :ask answering nil (failed whistle):
    ;; both leave a typed record, never a success.
    (mo/verdict! {:store store :library (temp-library)} "turn-new")
    (let [v (verdicts store "turn-new")]
      (is (map? v) "the record is still written")
      (is (= "none" (:reader v)))
      (is (= "no reader" (get-in v [:patterns 0 :chain :I1 :why])))
      (is (= 0 (get-in v [:patterns 0 :chain_mapped])))
      (is (= "void" (get-in v [:patterns 0 :status])))
      (is (= ["turn-old1" "turn-old2" "turn-old3" "turn-old4" "turn-new"] (:window v))
          "the stored window is the chain, oldest first"))
    (record! store "turn-new"
             {:proposed_patterns [{:patterns [pid] :at "x" :job_id "j"}]})
    (mo/verdict! {:store store :library (temp-library)
                  :ask (fn [_] (throw (ex-info "whistle failed" {})))}
                 "turn-new")
    (let [v (verdicts store "turn-new")]
      (is (= "none" (:reader v)) "a failed whistle is also a typed no-reader"))))

(deftest a-record-without-proposals-or-not-operator-is-left-alone
  (let [store (temp-store)]
    (record! store "turn-plain" {})
    (is (nil? (run-with store "turn-plain" (constantly "[]"))))
    (record! store "turn-agent" {:origin "agent"
                                 :proposed_patterns [{:patterns [pid]}]})
    (is (nil? (run-with store "turn-agent" (constantly "[]"))))
    (is (nil? (verdicts store "turn-agent")))))

;; -------------------------------------------------------------- the listener

(defn- wait-for
  "Poll PRED up to ~5 s (the listener runs on the namespace's own thread)."
  [pred]
  (let [deadline (+ (System/nanoTime) (* 5 (long 1e9)))]
    (loop []
      (or (pred)
          (when (< (System/nanoTime) deadline)
            (Thread/sleep 20)
            (recur))))))

(deftest an-aligned-event-writes-verdicts-a-ported-event-alone-does-not
  (let [store (base-records (temp-store))
        asks (atom 0)
        opts {:store store :library (temp-library)
              :ask (fn [_]
                     (swap! asks inc)
                     (ask-reply [{:pattern pid :turn {} :chain {}}]))}]
    (record! store "turn-new"
             {:proposed_patterns [{:patterns [pid] :at "x" :job_id "j"}]})
    (mo/handle-event opts {:event :ported :id "turn-new"})
    (is (nil? (verdicts store "turn-new"))
        "a :ported event alone writes no verdict")
    (mo/handle-event opts {:event :aligned :id "turn-new"})
    (let [v (verdicts store "turn-new")]
      (is (map? v) "the :aligned event writes :pattern_verdicts")
      (is (= 1 (count (:patterns v)))))
    ;; idempotence: a repeated :aligned (the alignment reading can raise it
    ;; more than once per record) must not double-write or re-ask.
    (let [before (verdicts store "turn-new")]
      (mo/handle-event opts {:event :aligned :id "turn-new"})
      (is (= 1 @asks) "the same pattern set is not read twice")
      (is (= before (verdicts store "turn-new"))
          "the stored verdict is left alone, not overwritten")))
  ;; a non-operator record is left alone even on :aligned
  (let [store (temp-store)
        opts {:store store :library (temp-library) :ask (constantly "[]")}]
    (record! store "turn-bell2" {:origin "agent"
                                 :proposed_patterns [{:patterns [pid]}]})
    (mo/handle-event opts {:event :aligned :id "turn-bell2"})
    (is (nil? (verdicts store "turn-bell2")))))

(deftest deletion-check-removing-the-aligned-filter-makes-ported-write
  ;; The guard under test is handle-event's (= :aligned event) filter.
  ;; Replace handle-event with a copy that has that one check deleted: a
  ;; :ported event now writes, so the :ported-alone assertion above would
  ;; fail without the filter (demonstrated against the real store and ask).
  (let [store (base-records (temp-store))
        opts {:store store :library (temp-library)
              :ask (constantly (ask-reply [{:pattern pid :turn {} :chain {}}]))}]
    (record! store "turn-del" {:proposed_patterns [{:patterns [pid] :at "x" :job_id "j"}]})
    (with-redefs [mo/handle-event
                  (fn [opts {:keys [id]}]
                    ;; the (= :aligned event) check, deleted
                    (let [store (if (fn? (:store opts)) ((:store opts)) (:store opts))]
                      (when (and id store)
                        (mo/verdict! (assoc opts :store store) id))))]
      (mo/handle-event opts {:event :ported :id "turn-del"})
      (is (map? (verdicts store "turn-del"))
          "with the filter deleted a :ported event DOES write — so the
          :ported-alone test above fails in that world; the filter is what
          holds it"))))

(deftest start-registers-a-keyed-listener-that-runs-on-its-own-thread
  (let [store (base-records (temp-store))
        registered (atom nil)]
    (record! store "turn-live" {:proposed_patterns [{:patterns [pid] :at "x" :job_id "j"}]})
    (with-redefs [turn-service/add-turn-listener!
                  (fn [_key f] (reset! registered f) nil)]
      (mo/start! {:store store :library (temp-library)
                  :ask (constantly (ask-reply [{:pattern pid :turn {} :chain {}}]))}))
    (let [f @registered]
      (is (fn? f) "start! registers a listener")
      (f {:event :aligned :id "turn-live"})
      (is (wait-for #(map? (verdicts store "turn-live")))
          "the registered listener's :aligned event reaches the store"))))

(deftest patterns-of-reads-the-ranked-list-when-a-render-has-one
  (is (= [{:id "a/top" :sources ["embedding"] :shown true}
          {:id "b/also" :sources ["embedding"] :shown true}
          {:id "c/also" :sources ["embedding"] :shown true}]
         (mo/patterns-of
          {:proposed_patterns
           [{:patterns ["~a/top"]
             :ranked [{:id "a/top" :score 0.5 :rank 1}
                      {:id "b/also" :score 0.4 :rank 2}
                      {:id "c/also" :score 0.3 :rank 3}]}
            {:patterns ["~d/old"]}
            {:patterns ["~a/top"] :ranked [{:id "a/top" :rank 1}]}]}))
      "ranked ids in rank order, older renders by :patterns, distinct; the
      embedding source is capped at its top 3 (packet 2b), so d/old drops"))

(deftest patterns-of-unions-lu-ranked-after-the-embedding-ids
  (is (= [{:id "e/one" :sources ["embedding"]}
          {:id "l/one" :sources ["驴"]}
          {:id "l/two" :sources ["驴"]}]
         (mo/patterns-of
          {:proposed_patterns [{:patterns ["~e/one"]}]
           :lu_ranked {:source "驴"
                       :ranked [{:id "l/one" :rank 1} {:id "l/two" :rank 2}]}}))
      "the lu_ranked ids follow the proposed ids, deduplicated")
  (is (= [{:id "x/both" :sources ["embedding" "驴"]}]
         (mo/patterns-of
          {:proposed_patterns [{:patterns ["~x/both"]}]
           :lu_ranked {:source "embedding-fallback"
                       :ranked [{:id "x/both" :rank 1}]}}))
      "an id proposed by both carries both sources, whatever lu's source"))

(deftest patterns-of-unions-all-sources-with-shown-and-cap
  (let [result (mo/patterns-of
                {:proposed_patterns
                 [{:ranked [{:id "emb/one" :rank 1} {:id "emb/two" :rank 2}
                            {:id "emb/three" :rank 3} {:id "emb/four" :rank 4}]}]
                 :embedding_ranked [{:id "emb/three" :rank 1} {:id "emb/nine" :rank 2}]
                 :m40_ranked {:source "驴/m40"
                              :ranked [{:id "m40/one" :rank 1} {:id "emb/one" :rank 2}
                                       {:id "m40/two" :rank 3} {:id "m40/three" :rank 4}]}
                 :lu_ranked {:source "驴"
                             :ranked [{:id "m40/one" :rank 1} {:id "lu/one" :rank 2}
                                      {:id "lu/two" :rank 3} {:id "lu/three" :rank 4}]}})]
    ;; embedding top 3 (emb/nine and emb/four capped out), 驴/m40's new ids
    ;; (emb/one merges, m40/three capped out), 驴's new ids (lu/three capped
    ;; out): 7 ids, never more than 9.
    (is (= ["emb/one" "emb/two" "emb/three" "m40/one" "m40/two" "lu/one" "lu/two"]
           (mapv :id result)))
    (is (= ["embedding" "驴/m40"]
           (:sources (some #(when (= "emb/one" (:id %)) %) result))))
    (is (= ["驴/m40" "驴"]
           (:sources (some #(when (= "m40/one" (:id %)) %) result))))
    (is (true? (:shown (some #(when (= "emb/one" (:id %)) %) result)))
        "ids of the shown segment carry :shown true")
    (is (not (:shown (some #(when (= "lu/one" (:id %)) %) result)))
        "unshown ids do not")))

(deftest verdict-rows-score-both-sets-and-carry-sources
  (let [store (base-records (temp-store))]
    (record! store "turn-new"
             {:created_at "2026-10-24T00:04:00Z"
              :source_text (q "turn-new" "operator")
              :reply_text (q "turn-new" "reply")
              :proposed_patterns [{:patterns [pid] :at "x" :job_id "j"}]
              :lu_ranked {:version 1 :source "驴" :ranked [{:id "test-family/lu-only" :rank 1}]}})
    (run-with store "turn-new"
              (constantly
               (ask-reply [{:pattern pid :turn {} :chain {}}
                           {:pattern "test-family/lu-only" :turn {} :chain {}}])))
    (let [rows (:patterns (verdicts store "turn-new"))]
      (is (= 2 (count rows)) "both the embedding's and 驴's patterns are read in one whistle")
      (is (= ["embedding"] (:sources (first rows))))
      (is (= ["驴"] (:sources (second rows))))
      (is (= "test-family/lu-only" (:pattern (second rows))))
      (is (false? (:shown (first rows)))
          "no render carried :ranked, so nothing was marked shown"))))
