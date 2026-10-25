(ns futon3c.xiang.lu-rerank-test
  "驴 reranker: pool construction (embedding leg + graph leg, earlier records
  only), the verbatim-quote ground, and the embedding fallback. Synthetic text
  only; the embedding search and the 驴 seat are stubbed. Run:
  clojure -M:test -n futon3c.xiang.lu-rerank-test"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.xiang.lu-rerank :as lu]
            [futon3c.xiang.turn-service]
            [futon3c.xiang.turn-store :as ts]))

(defn- temp-store []
  (ts/store (str (java.nio.file.Files/createTempDirectory
                  "lu-rerank-" (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- record!
  [store id over]
  (let [record (merge {:origin "operator" :agent_id "claude-3"
                       :session_id "sess-1" :turn_id id
                       :created_at "2026-10-24T00:00:00Z"
                       :source_text (str "operator words of " id
                                         " that a verbatim quote can be taken from")
                       :reply_text "reply words"}
                      over)]
    (if (ts/read-record store id)
      (ts/update-record! store id #(merge % record))
      (ts/write-record-id! store id record)))
  store)

(def verdict
  "A stored :pattern_verdicts map with one occurrence and one partial."
  {:version 1 :at "x" :window [] :reader "馬-sonnet"
   :patterns [{:pattern "~occ/found-occurrence" :status "occurrence"}
              {:pattern "~occ/found-partial" :status "partial"}
              {:pattern "~occ/not-seeded" :status "void"}]})

(def graph
  {:edges [{:a "occ/found-occurrence" :b "graph/why-nbr" :kind "why"}
           {:a "occ/found-occurrence" :b "graph/cocited-nbr" :kind "co-cited"}
           {:a "occ/found-partial" :b "graph/why-nbr" :kind "co-cited"}
           {:a "occ/not-seeded" :b "graph/void-nbr" :kind "why"}]})

(defn- stub-search [results]
  (fn [& _] (vec results)))

(deftest pool-combines-embedding-and-graph-legs-earlier-records-only
  (let [store (temp-store)]
    ;; an earlier record of the session carrying verdicts
    (record! store "turn-early" {:created_at "2026-10-24T00:01:00Z"
                                 :pattern_verdicts verdict})
    ;; a LATER record's verdicts must not seed (b)
    (record! store "turn-late-other" {:created_at "2026-10-24T00:03:00Z"
                                      :pattern_verdicts
                                      (assoc-in verdict [:patterns 0 :pattern]
                                                "~later/should-not-seed")})
    ;; a same-moment other-session record must not seed either
    (record! store "turn-other-session" {:session_id "sess-2"
                                         :created_at "2026-10-24T00:00:30Z"
                                         :pattern_verdicts verdict})
    (record! store "turn-n" {:created_at "2026-10-24T00:02:00Z"})
    (with-redefs [lu/embedding-top (stub-search ["emb/one" "emb/two"])
                  lu/load-graph (constantly graph)]
      (let [pool (lu/candidate-pool store "turn-n")]
        (is (= ["emb/one" "emb/two" "graph/why-nbr" "graph/cocited-nbr"] pool)
            "embedding leg first, then graph neighbours of the earlier
            occurrence/partial seeds only, ranked by best edge kind")
        (is (not (some #(str/starts-with? % "later/") pool))
            "a later record's verdicts do not seed the graph leg")
        (is (not (some #{"graph/void-nbr"} pool))
            "a void pattern's neighbours are not seeded")))))

(deftest a-pick-with-a-non-verbatim-quote-is-dropped-and-next-taken
  (let [store (temp-store)]
    (record! store "turn-q" {:source_text
                             "the operator asked for a verbatim quote ground here"})
    (with-redefs [lu/embedding-top (stub-search ["q/a" "q/b" "q/c"])
                  lu/load-graph (constantly nil)]
      (let [ask (fn [_] (json/generate-string
                         [{:pattern "q/a" :quote "these words appear nowhere at all"}
                          {:pattern "q/b" :quote "a verbatim quote ground here"}
                          {:pattern "q/c" :quote "asked for a verbatim quote ground here"}]))]
        (is (= [{:id "q/b" :rank 1 :quote "a verbatim quote ground here"}
                {:id "q/c" :rank 2 :quote "asked for a verbatim quote ground here"}]
               (:ranked (lu/rerank {:ask ask} store "turn-q")))
            "the non-verbatim pick is dropped and the next one taken")))))

(deftest timeout-or-thrown-ask-gives-embedding-fallback-with-a-reason
  (let [store (temp-store)]
    (record! store "turn-f" {:source_text "some operator text"})
    (with-redefs [lu/embedding-top (stub-search ["f/one" "f/two" "f/three"])
                  lu/load-graph (constantly nil)]
      (let [timeout (lu/rerank {:ask (fn [_] nil)} store "turn-f")]
        (is (= "embedding-fallback" (:source timeout)))
        (is (= "timeout-or-empty-reply" (:reason timeout)))
        (is (= ["f/one" "f/two" "f/three"] (mapv :id (:ranked timeout)))))
      (let [thrown (lu/rerank {:ask (fn [_] (throw (ex-info "seat down" {})))}
                              store "turn-f")]
        (is (= "embedding-fallback" (:source thrown)))
        (is (re-find #"ask-error" (str (:reason thrown))))
        (is (= 3 (count (:ranked thrown)))))
      (let [zero (lu/rerank {:ask (fn [_] "[]")} store "turn-f")]
        (is (= "embedding-fallback" (:source zero)))
        (is (= "zero-surviving-picks" (:reason zero)))))))

(deftest rerank-never-throws
  (let [store (temp-store)]
    (record! store "turn-never" {:source_text "text"})
    (with-redefs [lu/embedding-top (fn [& _] (throw (ex-info "search down" {})))
                  lu/load-graph (constantly nil)]
      (is (map? (lu/rerank {:ask (fn [_] "garbage")} store "turn-never"))))))

;; ------------------------------------------- Packet B: the record-time trigger

(defn- wait-for
  "Poll PRED up to ~5 s (the trigger runs on the namespace's own thread)."
  [pred]
  (let [deadline (+ (System/nanoTime) (* 5 (long 1e9)))]
    (loop []
      (or (pred)
          (when (< (System/nanoTime) deadline)
            (Thread/sleep 20)
            (recur))))))

(defn- stub-ask [picks]
  (fn [_] (json/generate-string
           (mapv (fn [[pid quote]] {:pattern pid :quote quote}) picks))))

(deftest a-recorded-event-writes-lu-ranked-onto-the-record
  (let [store (temp-store)
        registered (atom nil)]
    (record! store "turn-trig"
             {:source_text "the operator asked for a rerank of patterns here"})
    (with-redefs [lu/embedding-top (stub-search ["trig/emb" "trig/emb2" "trig/emb3"])
                  lu/load-graph (constantly nil)
                  futon3c.xiang.turn-service/add-turn-listener!
                  (fn [_key f] (reset! registered f) nil)]
      (lu/start! {:store store
                  :ask (stub-ask [["trig/emb" "a rerank of patterns here"]])})
      (let [f @registered]
        (is (fn? f) "start! registers a turn listener")
        (f {:event :recorded :id "turn-trig"})
        (is (wait-for #(contains? (ts/read-record store "turn-trig") :lu_ranked))
            "the :recorded event leads to :lu_ranked on the record")
        (let [lu* (:lu_ranked (ts/read-record store "turn-trig"))]
          (is (= 1 (:version lu*)))
          (is (= "驴" (:source lu*)))
          (is (= [{:id "trig/emb" :rank 1 :quote "a rerank of patterns here"}]
                 (:ranked lu*)))
          (is (number? (:ms lu*)))
          (is (string? (:at lu*))))
        ;; idempotent: a second :recorded event does not re-run the whistle
        (let [first-at (:at (:lu_ranked (ts/read-record store "turn-trig")))]
          (f {:event :recorded :id "turn-trig"})
          (Thread/sleep 300)
          (is (= first-at (:at (:lu_ranked (ts/read-record store "turn-trig"))))
              "the second :recorded event leaves the first :lu_ranked alone"))))))

(deftest the-off-switch-file-stops-the-trigger
  (let [store (temp-store)
        off (str (java.nio.file.Files/createTempDirectory
                  "lu-off-" (make-array java.nio.file.attribute.FileAttribute 0)))
        off-file (java.io.File. (str off) "lu-off")]
    (spit off-file "")
    (record! store "turn-off" {:source_text "some operator text"})
    (with-redefs [lu/embedding-top (stub-search ["off/one" "off/two" "off/three"])
                  lu/load-graph (constantly nil)]
      (lu/handle-event {:store store :off-switch (.getPath off-file)
                        :ask (stub-ask [["off/one" "some operator text"]])}
                       {:event :recorded :id "turn-off"})
      (is (not (contains? (ts/read-record store "turn-off") :lu_ranked))
          "with the off-switch file present, nothing is written")
      ;; and the same event runs when the switch is a non-existent path
      (lu/handle-event {:store store :off-switch (str off "/absent")
                        :ask (stub-ask [["off/one" "some operator text"]])}
                       {:event :recorded :id "turn-off"})
      (is (wait-for #(contains? (ts/read-record store "turn-off") :lu_ranked))
          "without the off-switch, the same trigger writes"))))

(deftest the-trigger-skips-records-with-no-source-text
  (let [store (temp-store)]
    (record! store "turn-blank" {:source_text ""})
    (with-redefs [lu/embedding-top (stub-search ["b/one" "b/two" "b/three"])
                  lu/load-graph (constantly nil)]
      (lu/handle-event {:store store
                        :ask (stub-ask [["b/one" "nothing quotable"]])}
                       {:event :recorded :id "turn-blank"})
      (is (not (contains? (ts/read-record store "turn-blank") :lu_ranked))
          "no :source_text, no rerank"))))

(deftest a-repeated-pick-cannot-take-two-slots
  (let [store (temp-store)]
    (record! store "turn-dup" {:source_text
                               "duplicate picks must not take two slots here"})
    (with-redefs [lu/embedding-top (stub-search ["d/one" "d/two" "d/three"])
                  lu/load-graph (constantly nil)]
      (let [ask (fn [_] (json/generate-string
                         [{:pattern "d/one" :quote "must not take two slots here"}
                          {:pattern "d/one" :quote "must not take two slots here"}
                          {:pattern "d/two" :quote "picks must not take two slots"}]))]
        (is (= [{:id "d/one" :rank 1 :quote "must not take two slots here"}
                {:id "d/two" :rank 2 :quote "picks must not take two slots"}]
               (:ranked (lu/rerank {:ask ask} store "turn-dup")))
            "the second d/one is skipped and d/two takes the next slot")))))
