(ns futon3c.xiang.ma-occurrence-test
  "馬's per-pattern occurrence verdict: window selection, the verbatim-quote
  and window checks that void a bad map, the status counts, and the typed
  no-reader absence. Synthetic text only; the pattern parts come from a
  temp library so no real pattern file is read. Run:
  clojure -M:test -n futon3c.xiang.ma-occurrence-test"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.arxana.pattern-occurrences :as occurrences]
            [futon3c.arxana.pattern-proposals :as proposals]
            [futon3c.arxana.store-http :as store-http]
            [futon3c.arxana.turn-mention-writer :as mentions]
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

(defn- fixture-proposals [_ record]
  {:ids (mapv #(str/replace-first (str (:id %)) #"^~" "")
              (get-in record [:lu_ranked :ranked]))})

(defn- run-with [store id ask]
  (mo/verdict! {:store store :library (temp-library) :ask ask :seat "馬-sonnet"
                :proposal-reader fixture-proposals} id))

(defn- verdicts [store id]
  (:pattern_verdicts (ts/read-record store id)))

(def strand-verdict
  {:version 1 :at "fixture-at" :reader "fixture"
   :window [] :model_outcome {:failure_kind nil :batches []}
   :patterns [{:pattern pid :status "void"}]})

(defn- store-strand-verdict! [store id]
  (with-redefs-fn {#'futon3c.xiang.ma-occurrence/verdict-map
                   (fn [& _] strand-verdict)}
    #(mo/verdict! {:store store :proposal-reader fixture-proposals} id)))

(deftest ma-verdicts-form-a-session-local-durable-strand
  (let [store (temp-store)]
    (record! store "turn-first" {:created_at "2026-10-24T00:00:00Z"
                                  :proposed_patterns [{:patterns [pid]}]})
    (store-strand-verdict! store "turn-first")
    (is (= {:status "session-start"}
           (:connection (verdicts store "turn-first"))))

    (record! store "turn-second" {:created_at "2026-10-24T00:01:00Z"
                                   :proposed_patterns [{:patterns [pid]}]})
    (store-strand-verdict! store "turn-second")
    (is (= {:status "linked" :turn/id "turn-first"
            :sha256 (mo/verdict-digest (verdicts store "turn-first"))}
           (:connection (verdicts store "turn-second"))))

    (record! store "turn-other-session" {:session_id "sess-2"
                                          :created_at "2026-10-24T00:00:30Z"
                                          :proposed_patterns [{:patterns [pid]}]})
    (store-strand-verdict! store "turn-other-session")
    (is (= {:status "session-start"}
           (:connection (verdicts store "turn-other-session")))
        "interleaved sessions never cross-link")))

(deftest out-of-order-verdict-is-pending-then-repaired-without-overwriting-links
  (let [store (temp-store)]
    (record! store "turn-prev" {:created_at "2026-10-24T00:00:00Z"
                                 :proposed_patterns [{:patterns [pid]}]})
    (record! store "turn-next" {:created_at "2026-10-24T00:01:00Z"
                                 :proposed_patterns [{:patterns [pid]}]})
    (store-strand-verdict! store "turn-next")
    (is (= {:status "prev-pending" :turn/id "turn-prev"}
           (:connection (verdicts store "turn-next"))))
    (store-strand-verdict! store "turn-prev")
    (let [linked (:connection (verdicts store "turn-next"))]
      (is (= "linked" (:status linked)))
      (is (= "turn-prev" (:turn/id linked)))
      (is (= (mo/verdict-digest (verdicts store "turn-prev")) (:sha256 linked)))
      (ts/update-record! store "turn-prev"
                         #(assoc-in % [:pattern_verdicts :reader] "late replacement"))
      (mo/repair-connection! store "turn-next")
      (is (= linked (:connection (verdicts store "turn-next")))
          "a late repair cannot overwrite an established link"))))

(deftest a-placeholder-or-retryable-predecessor-is-pending-not-linked
  ;; claude-13 review of CL-63: N-1's lu-pending placeholder and a retryable
  ;; model failure are rewritten later, so N must not link to their digest.
  (let [store (temp-store)]
    (record! store "turn-prev" {:created_at "2026-10-24T00:00:00Z"
                                 :proposed_patterns [{:patterns [pid]}]})
    (record! store "turn-next" {:created_at "2026-10-24T00:01:00Z"
                                 :proposed_patterns [{:patterns [pid]}]})
    (doseq [placeholder [{:version 1 :state "lu-proposals-pending" :patterns []}
                         {:version 1 :model_attempts 1 :patterns []
                          :model_outcome {:failure_kind "model/timeout"}}]]
      (ts/update-record! store "turn-prev" #(assoc % :pattern_verdicts placeholder))
      (is (= {:status :prev-pending :turn/id "turn-prev"}
             (mo/connection-for store "turn-next"))))
    (ts/update-record! store "turn-prev"
                       #(assoc-in % [:pattern_verdicts :model_attempts] 3))
    (is (= :linked (:status (mo/connection-for store "turn-next")))
        "an exhausted retry is final and linkable")))

(deftest no-reply-predecessor-is-typed-absent-and-never-skipped
  (let [store (temp-store)]
    (record! store "turn-no-reply" {:created_at "2026-10-24T00:00:00Z"
                                     :reply_text nil})
    (record! store "turn-after-absence" {:created_at "2026-10-24T00:01:00Z"
                                          :proposed_patterns [{:patterns [pid]}]})
    (store-strand-verdict! store "turn-after-absence")
    (is (= {:status "prev-absent" :turn/id "turn-no-reply" :reason "no-reply"}
           (:connection (verdicts store "turn-after-absence"))))))

(deftest absent-gcc-proposals-record-a-retryable-pending-outcome
  (let [store (temp-store)]
    (record! store "turn-pending" {:lu_ranked {:ranked [{:id "must/not/fallback" :rank 1}]}})
    (let [out (mo/verdict! {:store store
                            :proposal-reader (fn [& _] {:pending :lu-proposals-pending})}
                           "turn-pending")]
      (is (= "lu-proposals-pending" (:state out)))
      (is (= "lu proposals pending" (:why out)))
      (is (= "lu-proposals-pending" (:state (verdicts store "turn-pending"))))))
  (let [store (temp-store)
        real {:version 1 :reader "馬-sonnet" :patterns [{:pattern "x/y" :verdict "occurs"}]}]
    (record! store "turn-read" {:pattern_verdicts real})
    (mo/verdict! {:store store
                  :proposal-reader (fn [& _] {:pending :lu-proposals-pending})}
                 "turn-read")
    (is (= "馬-sonnet" (:reader (verdicts store "turn-read")))
        "pending never overwrites a stored verdict")))

(deftest patterns-of-never-reads-the-legacy-lu-ranked-field
  (let [forbidden (reify clojure.lang.ILookup
                    (valAt [_ _] (throw (AssertionError. "legacy lu_ranked fallback")))
                    (valAt [_ _ _] (throw (AssertionError. "legacy lu_ranked fallback"))))]
    (is (= [] (mo/patterns-of {:lu_ranked forbidden :lu_proposal_ids []})))))

(deftest pattern-verdict-event-waits-for-gcc-write
  (let [store (temp-store) queued (atom nil) order (atom [])
        verdict {:version 1 :at "2026-11-16T20:00:00Z" :reader "馬-sonnet"
                 :window ["turn-order"]
                 :patterns [{:pattern pid :status "void" :sources []}]}]
    (record! store "turn-order" {})
    (with-redefs-fn {#'futon3c.xiang.ma-occurrence/verdict-map (fn [& _] verdict)}
      #(do
         (mo/verdict! {:store store
                       :proposal-reader (fn [& _] {:ids [pid]})
                       :occurrence-writer! (fn [& _] (swap! order conj :write) {:written 1})
                       :occurrence-async! (fn [f] (reset! queued f))
                       :notify! (fn [_] (swap! order conj :notify))}
                      "turn-order")
         (is (empty? @order) "the dependent event is not raised before the queued write")
         (@queued)
         (is (= [:write :notify] @order))
         (is (= {:written 1} (:pattern_occurrences (ts/read-record store "turn-order"))))))))

(deftest fresh-listener-verdict-writes-occurrence-through-real-writer
  (let [store (temp-store)
        appended (atom nil)
        quote "operator words of turn-gcc that can be quoted at length"]
    (record! store "turn-gcc"
             {:source_text quote
              :reply_text "reply words of turn-gcc that can be quoted at length"
              :proposed_patterns [{:patterns [pid] :at "x" :job_id "j"}]})
    (with-redefs [mentions/field-evidence
                  (fn [& _] {:reference {:id "external-reference/turn-gcc"}})
                  proposals/pattern-reference
                  (fn [_ _] {:id "external-reference/pattern-gcc"
                             :revision/digest "fixture-digest"})
                  store-http/append-batch!
                  (fn [_ message-id rows]
                    (reset! appended {:message-id message-id :rows rows})
                    {:ok {}})]
      (mo/handle-event
       {:store store
        :library (temp-library)
        :ask (constantly
              (ask-reply
               [{:pattern pid
                 :turn {"I1" (map-of "one intent" "turn-gcc" "operator" quote)}
                 :chain {}}]))
        :proposal-reader fixture-proposals
        :occurrence-writer! occurrences/write!
        :occurrence-config {:graph-store :fixture}
        :occurrence-async! (fn [f] (f))}
       {:event :aligned :id "turn-gcc"})
      (is (= {:written 1 :skipped []}
             (:pattern_occurrences (ts/read-record store "turn-gcc"))))
      (is (seq (:rows @appended)) "the real writer submitted its invented GCC batch"))))

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
    (mo/verdict! {:store store :library (temp-library) :window-n 1 :proposal-reader fixture-proposals
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
    ;; :ask nil (seat not wired) leaves a typed record, never a success.
    (mo/verdict! {:store store :library (temp-library) :proposal-reader fixture-proposals} "turn-new")
    (let [v (verdicts store "turn-new")]
      (is (map? v) "the record is still written")
      (is (= "none" (:reader v)))
      (is (= "model/no-reader" (get-in v [:model_outcome :failure_kind])))
      (is (= 1 (count (get-in v [:model_outcome :batches]))))
      (is (= "no reader" (get-in v [:patterns 0 :chain :I1 :why])))
      (is (= 0 (get-in v [:patterns 0 :chain_mapped])))
      (is (= "void" (get-in v [:patterns 0 :status])))
      (is (= ["turn-old2" "turn-old3" "turn-old4" "turn-new"] (:window v))
          "the stored window is the chain, oldest first"))
    ))

(deftest typed-model-failure-is-kept-on-verdict-and-voids
  (let [store (base-records (temp-store))]
    (record! store "turn-new"
             {:proposed_patterns [{:patterns [pid] :at "x" :job_id "j"}]})
    (mo/verdict! {:store store :library (temp-library) :proposal-reader fixture-proposals
                  :ask (constantly {:failure_kind "model/quota"
                                    :http_status 429 :error "invented quota"})}
                 "turn-new")
    (let [v (verdicts store "turn-new")]
      (is (= {:failure_kind "model/quota" :http_status 429 :error "invented quota"}
             (select-keys (:model_outcome v) [:failure_kind :http_status :error])))
      (is (= "model/quota" (get-in v [:patterns 0 :chain :I1 :why])))
      (is (= "void" (get-in v [:patterns 0 :status]))))))

(deftest thrown-ask-is-model-error
  (let [store (base-records (temp-store))]
    (record! store "turn-throw" {:created_at "2026-10-24T00:05:00Z"
                                 :proposed_patterns [{:patterns [pid]}]})
    (mo/verdict! {:store store :library (temp-library) :proposal-reader fixture-proposals
                  :ask (fn [_] (throw (ex-info "invented whistle failure" {})))}
                 "turn-throw")
    (is (= {:failure_kind "model/error" :http_status nil :error "invented whistle failure"}
           (select-keys (:model_outcome (verdicts store "turn-throw"))
                        [:failure_kind :http_status :error])))))

(deftest unexpected-listener-exception-stores-a-retry-receipt
  (let [store (temp-store)]
    (record! store "turn-listener-failure"
             {:proposed_patterns [{:patterns [pid]}]})
    (is (nil? (mo/verdict! {:store store
                            :proposal-reader
                            (fn [& _] (throw (ex-info "invented listener failure" {})))}
                           "turn-listener-failure")))
    (let [receipt (:ma_listener_failure
                   (ts/read-record store "turn-listener-failure"))]
      (is (= "invented listener failure" (:error receipt)))
      (is (= "clojure.lang.ExceptionInfo" (:class receipt)))
      (is (= 1 (:attempts receipt)))
      (is (string? (:at receipt))))
    (mo/verdict! {:store store
                  :proposal-reader
                  (fn [& _] (throw (ex-info "invented listener failure again" {})))}
                 "turn-listener-failure")
    (is (= 2 (get-in (ts/read-record store "turn-listener-failure")
                     [:ma_listener_failure :attempts])))))

(deftest another-aligned-turn-retries-listener-failure-and-clears-receipt
  (let [store (temp-store)
        success {:version 1 :reader "馬-test" :model_outcome {:failure_kind nil}
                 :patterns [{:pattern pid :status "void"}]}
        opts {:store store :proposal-reader (fn [& _] {:ids [pid]})}]
    (doseq [id ["turn-listener-failure" "turn-later"]]
      (record! store id {:proposed_patterns [{:patterns [pid]}]}))
    (mo/verdict! {:store store
                  :proposal-reader
                  (fn [& _] (throw (ex-info "invented listener failure" {})))}
                 "turn-listener-failure")
    (with-redefs-fn {#'futon3c.xiang.ma-occurrence/verdict-map
                     (fn [& _] success)}
      #(mo/handle-event opts {:event :aligned :id "turn-later"}))
    (let [record (ts/read-record store "turn-listener-failure")]
      (is (= success (dissoc (:pattern_verdicts record) :model_attempts :connection)))
      (is (= 1 (get-in record [:pattern_verdicts :model_attempts])))
      (is (nil? (:ma_listener_failure record))))))

(deftest listener-failure-at-attempt-cap-is-not-retried
  (let [store (temp-store)
        called (atom [])]
    (record! store "turn-capped"
             {:ma_listener_failure {:at "2026-11-17T00:00:00Z"
                                    :error "invented" :class "example.Error"
                                    :attempts 3}})
    (record! store "turn-later" {})
    (with-redefs [mo/verdict! (fn [_ id] (swap! called conj id))]
      (mo/handle-event {:store store} {:event :aligned :id "turn-later"}))
    (is (= ["turn-later"] @called))))

(deftest listener-receipt-write-failure-is-contained
  (let [store (temp-store)]
    (record! store "turn-receipt-refused"
             {:proposed_patterns [{:patterns [pid]}]})
    (with-redefs [ts/update-record!
                  (fn [& _] (throw (ex-info "invented receipt refusal" {})))]
      (is (nil? (mo/verdict! {:store store
                              :proposal-reader
                              (fn [& _] (throw (ex-info "invented listener failure" {})))}
                             "turn-receipt-refused"))))))

(deftest nil-and-unparsed-ask-results-are-distinct
  (doseq [[id ask kind] [["turn-nil" (constantly nil) "model/error"]
                         ["turn-unparsed" (constantly "not a JSON array")
                          "model/unparsed-reply"]]]
    (let [store (base-records (temp-store))]
      (record! store id {:created_at "2026-10-24T00:05:00Z"
                         :proposed_patterns [{:patterns [pid]}]})
      (mo/verdict! {:store store :library (temp-library)
                    :proposal-reader fixture-proposals :ask ask} id)
      (is (= kind (get-in (verdicts store id) [:model_outcome :failure_kind]))))))

(deftest truncated-array-has-its-own-outcome
  ;; Planted check: changing reply-result to classify this as ordinary
  ;; model/unparsed-reply makes this fail.
  (let [store (base-records (temp-store))]
    (record! store "turn-truncated"
             {:created_at "2026-10-24T00:05:00Z"
              :proposed_patterns [{:patterns [pid]}]})
    (mo/verdict! {:store store :library (temp-library)
                  :proposal-reader fixture-proposals
                  :ask (constantly "[{\"pattern\":\"unfinished\"")}
                 "turn-truncated")
    (is (= "model/truncated-reply"
           (get-in (verdicts store "turn-truncated")
                   [:model_outcome :failure_kind])))))

(deftest nine-patterns-are-three-independent-batches
  ;; Planted check: restoring a batch size of 9 reduces calls to one and fails.
  (let [store (base-records (temp-store))
        ids (mapv #(str "p" %) (range 9))
        calls (atom [])
        ask (fn [{:keys [prompt]}]
              (let [batch (mapv second (re-seq #"(?m)^\[(p\d+)\]$" prompt))]
                (swap! calls conj batch)
                (ask-reply (mapv (fn [id]
                                   {:pattern id
                                    :turn {:I1 {:element nil :why "invented void"}}
                                    :chain {:I1 {:element nil :why "invented void"}}})
                                 batch))))]
    (record! store "turn-nine"
             {:created_at "2026-10-24T00:05:00Z"
              :proposed_patterns [{:patterns (subvec ids 0 3)}]
              :m40_ranked {:ranked (mapv #(hash-map :id %) (subvec ids 3 6))}
              :lu_ranked {:ranked (mapv #(hash-map :id %) (subvec ids 6 9))}})
    (with-redefs [mo/read-pattern (fn [_ id] {:id id :sha256 "fixture" :parts {}})]
      (mo/verdict! {:store store :proposal-reader fixture-proposals :ask ask}
                   "turn-nine"))
    (is (= [3 3 3] (mapv count @calls)))
    (is (= ids (mapv :pattern (:patterns (verdicts store "turn-nine")))))
    (is (= 3 (count (get-in (verdicts store "turn-nine")
                            [:model_outcome :batches]))))))

(deftest one-failed-batch-voids-only-that-batch
  (let [store (base-records (temp-store))
        ids (mapv #(str "p" %) (range 7))
        call (atom 0)
        ask (fn [{:keys [prompt]}]
              (let [n (swap! call inc)
                    batch (mapv second (re-seq #"(?m)^\[(p\d+)\]$" prompt))]
                (if (= n 2)
                  {:failure_kind "model/budget" :error "invented budget"}
                  (ask-reply (mapv (fn [id]
                                     {:pattern id :turn {} :chain {}})
                                   batch)))))]
    (record! store "turn-partial-batch"
             {:created_at "2026-10-24T00:05:00Z"
              :proposed_patterns [{:patterns (subvec ids 0 3)}]
              :m40_ranked {:ranked (mapv #(hash-map :id %) (subvec ids 3 6))}
              :lu_ranked {:ranked (mapv #(hash-map :id %) (subvec ids 6 7))}})
    (with-redefs [mo/read-pattern (fn [_ id] {:id id :sha256 "fixture" :parts {}})]
      (mo/verdict! {:store store :proposal-reader fixture-proposals :ask ask}
                   "turn-partial-batch"))
    (let [v (verdicts store "turn-partial-batch")
          by-id (into {} (map (juxt :pattern identity)) (:patterns v))]
      (is (= 3 @call))
      (is (= "model/budget" (get-in v [:model_outcome :failure_kind])))
      (is (= ["model/budget" "model/budget" "model/budget"]
             (mapv #(get-in by-id [% :chain :I1 :why]) (subvec ids 3 6))))
      (is (= "map-missing" (get-in by-id ["p0" :chain :I1 :why])))
      (is (= "map-missing" (get-in by-id ["p6" :chain :I1 :why]))))))

(deftest retryable-quota-is-replaced-by-success-on-the-next-aligned-event
  (let [store (base-records (temp-store))
        calls (atom 0)
        ask (fn [_]
              (if (= 1 (swap! calls inc))
                {:failure_kind "model/quota" :http_status 429 :error "invented"}
                (ask-reply [{:pattern pid :turn {} :chain {}}])))
        opts {:store store :library (temp-library)
              :proposal-reader fixture-proposals :ask ask}]
    (record! store "turn-new" {:proposed_patterns [{:patterns [pid]}]})
    (mo/handle-event opts {:event :aligned :id "turn-new"})
    (is (= "model/quota" (get-in (verdicts store "turn-new")
                                  [:model_outcome :failure_kind])))
    (mo/handle-event opts {:event :aligned :id "turn-new"})
    (let [v (verdicts store "turn-new")]
      (is (= 2 @calls) "the next aligned event retries the typed failure")
      (is (nil? (get-in v [:model_outcome :failure_kind])))
      (is (= 2 (:model_attempts v)))
      (is (not (:model_retry_exhausted v))))))

(deftest another-turns-aligned-event-retries-a-waiting-failure
  ;; A turn raises :aligned about once, so the retry must come from a
  ;; LATER turn's event, not the failed turn's own.
  (let [store (base-records (temp-store))
        quota? (atom true)
        asked (atom [])
        ask (fn [{:keys [prompt]}]
              (swap! asked conj prompt)
              (if @quota?
                {:failure_kind "model/quota" :http_status 429 :error "invented"}
                (ask-reply [{:pattern pid :turn {} :chain {}}])))
        opts {:store store :library (temp-library)
              :proposal-reader fixture-proposals :ask ask}]
    (record! store "turn-new" {:created_at "2026-10-24T00:05:00Z"
                               :proposed_patterns [{:patterns [pid]}]})
    (mo/handle-event opts {:event :aligned :id "turn-new"})
    (is (= "model/quota" (get-in (verdicts store "turn-new") [:model_outcome :failure_kind])))
    (reset! quota? false)
    (record! store "turn-later" {:created_at "2026-10-24T00:06:00Z"
                                 :proposed_patterns [{:patterns [pid]}]})
    (mo/handle-event opts {:event :aligned :id "turn-later"})
    (let [v (verdicts store "turn-new")]
      (is (nil? (get-in v [:model_outcome :failure_kind]))
          "the later turn's event re-read the earlier quota failure")
      (is (= 2 (:model_attempts v))))
    (let [n (count @asked)]
      (mo/handle-event opts {:event :aligned :id "turn-later"})
      (is (= n (count @asked)) "nothing left waiting: no further asks"))))

(deftest nonretryable-failure-and-success-remain-final
  (doseq [[id first-result kind]
          [["turn-http" {:failure_kind "model/http" :http_status 500 :error "invented"}
            "model/http"]
           ["turn-success" (ask-reply [{:pattern pid :turn {} :chain {}}]) nil]]]
    (let [store (base-records (temp-store))
          calls (atom 0)
          ask (fn [_] (swap! calls inc) first-result)
          opts {:store store :library (temp-library)
                :proposal-reader fixture-proposals :ask ask}]
      (record! store id {:created_at "2026-10-24T00:05:00Z"
                         :proposed_patterns [{:patterns [pid]}]})
      (mo/verdict! opts id)
      (mo/verdict! opts id)
      (is (= 1 @calls) (str id " is final after its first read"))
      (is (= kind (get-in (verdicts store id) [:model_outcome :failure_kind])))
      (is (= 1 (:model_attempts (verdicts store id)))))))

(deftest third-retryable-failure-is-exhausted-and-fourth-call-is-idempotent
  (let [store (base-records (temp-store))
        calls (atom 0)
        ask (fn [_] (swap! calls inc)
              {:failure_kind "model/quota" :http_status 429 :error "invented"})
        opts {:store store :library (temp-library)
              :proposal-reader fixture-proposals :ask ask}]
    (record! store "turn-new" {:proposed_patterns [{:patterns [pid]}]})
    (dotimes [_ 4] (mo/verdict! opts "turn-new"))
    (let [v (verdicts store "turn-new")]
      (is (= 3 @calls))
      (is (= 3 (:model_attempts v)))
      (is (true? (:model_retry_exhausted v)))
      (is (= "model/quota" (get-in v [:model_outcome :failure_kind]))))))

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
        opts {:store store :library (temp-library) :proposal-reader fixture-proposals
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
        opts {:store store :library (temp-library) :ask (constantly "[]")
              :proposal-reader fixture-proposals}]
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
                        (mo/verdict! (assoc opts :store store :proposal-reader fixture-proposals) id))))]
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
           :lu_proposal_ids ["l/one" "l/two"]}))
      "the lu_ranked ids follow the proposed ids, deduplicated")
  (is (= [{:id "x/both" :sources ["embedding" "驴"]}]
         (mo/patterns-of
          {:proposed_patterns [{:patterns ["~x/both"]}]
           :lu_proposal_ids ["x/both"]}))
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
                 :lu_proposal_ids ["m40/one" "lu/one" "lu/two" "lu/three"]})]
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
