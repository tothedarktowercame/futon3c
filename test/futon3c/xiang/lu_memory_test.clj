(ns futon3c.xiang.lu-memory-test
  "驴's Minus40 write half: situation evidence + :lu/occurrence hyperedge
  pairs, idempotence, the off-switch. The store transport is a recording
  stub; no real futon1b is touched. Run:
  clojure -M:test -n futon3c.xiang.lu-memory-test"
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.xiang.lu-memory :as lum]
            [futon3c.xiang.turn-store :as ts]))

(defn- temp-store []
  (ts/store (str (java.nio.file.Files/createTempDirectory
                  "lu-memory-" (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- record!
  [store id over]
  (let [record (merge {:origin "operator" :agent_id "claude-3"
                       :session_id "sess-1" :turn_id id
                       :created_at "2026-10-25T00:00:00Z"
                       :source_text "the whole operator source text of the turn"}
                      over)]
    (if (ts/read-record store id)
      (ts/update-record! store id #(merge % record))
      (ts/write-record-id! store id record)))
  store)

(def verdict
  {:version 1 :at "2026-10-25T00:01:00Z" :reader "馬-sonnet"
   :patterns [{:pattern "f/one" :sha256 "abc123" :status "occurrence"
               :turn_mapped 4 :chain_mapped 4 :sources ["embedding" "驴"]}
              {:pattern "f/void" :sha256 "def456" :status "void"
               :turn_mapped 0 :chain_mapped 0 :sources ["embedding"]}]})

(defn- stub
  "A recording store transport: GET hyperedge-by-id answers FOUND?, POST
  evidence 201, POST hyperedge 200."
  [& {:keys [found?] :or {found? false}}]
  (let [calls (atom [])]
    {:request (fn [{:keys [method path body]}]
                (swap! calls conj {:method method :path path :body body})
                (cond
                  (and (= "GET" method) (str/starts-with? path "/api/alpha/hyperedge/"))
                  {:status (if (and found? (str/includes? path "lu.occurrence")) 200 404)}
                  (and (= "POST" method) (= "/api/alpha/evidence" path))
                  {:status 201 :body {:ok true :evidence/id (:evidence/id body)}}
                  (and (= "POST" method) (= "/api/alpha/hyperedge" path))
                  {:status 200 :body {:hx/id (:hx/id body)}}
                  :else {:status 400 :body {:error "unexpected"}}))
     :calls calls}))

(deftest one-verdict-row-writes-one-evidence-and-one-hyperedge
  (let [store (temp-store)]
    (record! store "turn-lm1" {:pattern_verdicts verdict
                               :lu_ranked {:source "驴"
                                           :ranked [{:id "f/one" :rank 1
                                                     :quote "quote from lu for f/one"}]}})
    (let [{:keys [request calls]} (stub)
          tally (lum/write-record! {:store store :request request} "turn-lm1")]
      (is (= {:rows 2 :written 2 :unchanged 0} (dissoc tally :failed :written-rows))
          "both rows (occurrence and void) are written")
      (is (empty? (:failed tally)))
      (let [evidence-posts (filter #(and (= "POST" (:method %))
                                         (= "/api/alpha/evidence" (:path %)))
                                   @calls)
            hx-posts (filter #(and (= "POST" (:method %))
                                   (= "/api/alpha/hyperedge" (:path %)))
                             @calls)]
        (is (= 2 (count evidence-posts)))
        (is (= 2 (count hx-posts)))
        (let [ev (first evidence-posts)]
          (is (= "quote from lu for f/one"
                 (get-in (:body ev) [:evidence/body :text]))
              "the text is 驴's IF quote when there is one")
          (is (= "lu-quote" (get-in (:body ev) [:evidence/body :text-source])))
          (is (= "turn-lm1" (get-in (:body ev) [:evidence/body :record])))
          (is (= "sess-1" (:evidence/session-id (:body ev))))
          (is (= "claude-3" (get-in (:body ev) [:evidence/body :agent]))))
        (let [ev2 (second evidence-posts)]
          (is (= "the whole operator source text of the turn"
                 (get-in (:body ev2) [:evidence/body :text]))
              "a pattern 驴 did not quote gets the whole source_text"))
        (let [hx (some #(when (= "f/one" (second (get-in (:body %) [:hx/endpoints]))) %)
                       hx-posts)
              props (get-in (:body hx) [:hx/props])]
          (is (map? hx) "f/one's hyperedge is written")
          (is (= "occurrence" (:status props)))
          (is (= 4 (:turn_mapped props)))
          (is (= 4 (:chain_mapped props)))
          (is (= ["embedding" "驴"] (:sources props)))
          (is (= "2026-10-25T00:01:00Z" (:at props)))
          (is (= "abc123" (:sha256 props)))
          (is (= "2026-10-25T00:00:00Z" (:hx/valid-time (:body hx)))
              "valid-time is the record's created_at")
          (is (= :lu/occurrence (get-in (:body hx) [:hx/type]))))
        (let [hx (some #(when (= "f/void" (second (get-in (:body %) [:hx/endpoints]))) %)
                       hx-posts)]
          (is (= "void" (get-in (:body hx) [:hx/props :status]))
              "void rows are written with status void"))))))

(deftest a-re-run-writes-nothing
  (let [store (temp-store)]
    (record! store "turn-lm2" {:pattern_verdicts verdict})
    (let [{:keys [request]} (stub)
          first (lum/write-record! {:store store :request request} "turn-lm2")]
      (is (= 2 (:written first)))
      ;; second run: the stub now reports the hyperedges as already stored
      (let [{:keys [request calls]} (stub :found? true)
            second (lum/write-record! {:store store :request request} "turn-lm2")]
        (is (= {:rows 2 :written 0 :unchanged 2} (dissoc second :failed :written-rows)))
        (is (empty? (:failed second)))
        (is (= 2 (count @calls))
            "only the two existence GETs happen; no POSTs")
        (is (every? #(= "GET" (:method %)) @calls))))))

(deftest with-the-off-switch-present-nothing-is-written
  (let [store (temp-store)]
    (record! store "turn-lm3" {:pattern_verdicts verdict})
    (let [off (str (java.nio.file.Files/createTempDirectory
                    "lu-off-" (make-array java.nio.file.attribute.FileAttribute 0)))
          off-file (java.io.File. (str off) "lu-off")]
      (spit off-file "")
      (let [{:keys [request calls]} (stub)]
        (lum/handle-event {:store store :request request
                           :off-switch (.getPath off-file)}
                          {:event :pattern-verdicts :id "turn-lm3"})
        (is (empty? @calls) "with the off-switch file present, nothing is written")
        (lum/handle-event {:store store :request request
                           :off-switch (str off "/absent")}
                          {:event :pattern-verdicts :id "turn-lm3"})
        (is (= 6 (count @calls))
            "without the off-switch the same event writes (2 GETs + 2+2 POSTs)")))))

(deftest a-non-verdict-event-or-record-is-ignored
  (let [store (temp-store)]
    (record! store "turn-lm4" {:pattern_verdicts verdict})
    (let [s (stub)]
      (lum/handle-event {:store store :request (:request s)}
                        {:event :recorded :id "turn-lm4"})
      (is (empty? @(:calls s)) "only :pattern-verdicts events write"))
    (let [{:keys [request]} (stub)]
      (lum/write-record! {:store store :request request} "turn-none")
      (is (= {:rows 0 :written 0 :unchanged 0 :failed [] :written-rows []}
             (lum/write-record! {:store store :request request} "turn-none"))
          "a record without verdicts writes nothing"))))
(deftest situations-of-reads-back-by-endpoint
  (let [calls (atom [])
        request (fn [{:keys [method path]}]
                  (swap! calls conj path)
                  (is (= "GET" method))
                  {:status 200
                   :body {:hyperedges
                          [{:hx/endpoints ["e-lu-x" "f/one"]
                            :hx/props {:status "occurrence" :turn_mapped 4
                                       :chain_mapped 4 :sources ["embedding" "驴"]
                                       :at "2026-10-25T00:01:00Z"}}]}})]
    (is (= [{:situation "e-lu-x" :status "occurrence" :turn_mapped 4
             :chain_mapped 4 :sources ["embedding" "驴"] :at "2026-10-25T00:01:00Z"}]
           (lum/situations-of {:request request} "f/one")))
    (is (re-find #"type=lu%2Foccurrence&end=f%2Fone" (first @calls))
        "the query is by type and end, like the card provider's")))

(deftest situations-of-returns-nil-when-the-query-fails
  (is (nil? (lum/situations-of {:request (fn [_] {:status 500 :body {}})}
                               "f/one"))))

;; --------------------------------------------- the fast read half (packet 2a)

(def planted-neighbours
  "Four planted situations: p/occ lifted by an occurrence neighbour and
  dragged by a void one; p/part carried by a partial; p/void-only pushed
  under zero."
  [{:id "s1" :cos 0.60 :record "r1" :pattern "p/occ" :status "occurrence"}
   {:id "s2" :cos 0.50 :record "r2" :pattern "p/occ" :status "void"}
   {:id "s3" :cos 0.45 :record "r3" :pattern "p/part" :status "partial"}
   {:id "s4" :cos 0.40 :record "r4" :pattern "p/void-only" :status "void"}])

(deftest the-scoring-rule-on-four-planted-situations
  (let [ranked (lum/score-patterns planted-neighbours)]
    ;; p/occ: 0.60*1.0 + 0.50*(-0.5) = 0.35 > 0 (the occurrence lifts it
    ;; past the void); p/part: 0.45*0.5 = 0.225; p/void-only: -0.20 drops out.
    (is (= ["p/occ" "p/part"] (mapv :id ranked))
        "the void-only pattern is pushed below zero and dropped")
    (is (= [0.35 0.225] (mapv :score ranked)))
    (is (= [1 2] (mapv :rank ranked)))
    (is (= [["r1" "r2"] ["r3"]] (mapv :evidence ranked)))))

(deftest an-occurrence-neighbour-alone-lifts-a-pattern
  (is (= [{:id "p/a" :score 0.6 :rank 1 :evidence ["r1"]}]
         (lum/score-patterns
          [{:id "s1" :cos 0.60 :record "r1" :pattern "p/a" :status "occurrence"}]))))

(deftest a-void-heavier-than-its-occurrence-drops-the-pattern
  (is (empty? (lum/score-patterns
               [{:id "s1" :cos 0.30 :record "r1" :pattern "p/a" :status "occurrence"}
                {:id "s2" :cos 0.70 :record "r2" :pattern "p/a" :status "void"}]))))

(deftest the-threshold-fallback-and-no-positive-fallback
  (let [replies (atom [{:ok true :neighbours (subvec planted-neighbours 0 2)}
                       {:ok true :neighbours
                        [{:id "s1" :cos 0.6 :record "r1" :pattern "p/v" :status "void"}
                         {:id "s2" :cos 0.5 :record "r2" :pattern "p/w" :status "void"}
                         {:id "s3" :cos 0.4 :record "r3" :pattern "p/x" :status "void"}]}
                       {:ok true :neighbours planted-neighbours}])]
    (with-redefs [lum/read-resident-op!
                  (fn [_op _opts]
                    (let [r (first @replies)]
                      (swap! replies rest)
                      r))]
      (let [fewer (lum/read-near "text" {})]
        (is (nil? (:source fewer)))
        (is (= "fewer-than-3-neighbours" (:reason fewer)))
        (is (= 2 (:neighbours fewer))))
      (let [none (lum/read-near "text" {})]
        (is (nil? (:source none)))
        (is (= "no-positive-pattern" (:reason none)))
        (is (= 3 (:neighbours none))))
      (let [hit (lum/read-near "text" {})]
        (is (= "驴/m40" (:source hit)))
        (is (= ["p/occ" "p/part"] (mapv :id (:ranked hit))))
        (is (= 4 (:neighbours hit)))
        (is (number? (:ms hit)))))))

(deftest the-same-record-exclusion-is-sent-to-the-resident
  (let [ops (atom [])]
    (with-redefs [lum/read-resident-op!
                  (fn [op _opts] (swap! ops conj op) {:ok true :neighbours []})]
      (lum/read-near "some operator text" {:record-id "turn-self"})
      (is (= ["turn-self"] (get-in (first @ops) [:exclude-records]))
          "the query's own record is excluded so a turn never retrieves itself")
      (lum/read-near "some operator text" {})
      (is (= [] (get-in (second @ops) [:exclude-records]))))))

(deftest a-resident-error-is-a-typed-fallback-never-a-throw
  (with-redefs [lum/read-resident-op! (fn [_ _] (throw (ex-info "down" {})))]
    (let [out (lum/read-near "text" {})]
      (is (nil? (:source out)))
      (is (re-find #"resident-error" (:reason out))))))

(deftest an-append-adds-exactly-one-row-through-the-resident
  (let [ops (atom [])]
    (with-redefs [lum/read-resident-op!
                  (fn [op _opts]
                    (swap! ops conj op)
                    (if (= 1 (count @ops))
                      {:ok true :situations 130}
                      {:ok true :unchanged true}))]
      (is (true? (lum/append-situation! "e-lu-new" "new text" {:pattern "p/x"}))
          "the first append is a write")
      (is (false? (lum/append-situation! "e-lu-new" "new text" {:pattern "p/x"}))
          "the second is the resident's unchanged")
      (is (= "add" (:op (first @ops))))
      (is (= "e-lu-new" (:id (first @ops))))
      (is (= "new text" (:text (first @ops))))
      ;; a failure is nil, logged, never thrown
      (with-redefs [lum/read-resident-op! (fn [_ _] (throw (ex-info "down" {})))]
        (is (nil? (lum/append-situation! "e-lu-2" "t" {:pattern "p"})))))))
