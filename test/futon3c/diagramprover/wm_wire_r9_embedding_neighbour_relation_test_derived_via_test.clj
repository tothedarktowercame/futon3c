(ns futon3c.diagramprover.wm-wire-r9-embedding-neighbour-relation-test-derived-via-test
  "Wire [:r9-embedding-neighbour :r9-relation-test :derived-via]: the
  embedding derivation receipt reaching the box's own test,
  futon2/test/futon2/aif/relation_derivation_test.clj, whose read is
  (:derived-via c) on the classification it asserts (m-autoclock-in-live
  selects :kind :neighbour :cosine :runner-up off it).

  The seventh flight's run record carries the writer's end (the recorded
  :derived-via {:kind :embedding-neighbour ...}) but a test box records
  nothing, so the wire is WITNESSED-HERMETICALLY: embedding-neighbour
  (writer) is called on the real pinned inputs with the rowed map built as
  classify-target builds it, classify-target (the carrier to the reader)
  is called with the reader's own live context, and the reader's read
  (:derived-via c, less the key classify-target adds) is observed."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.focus-receipt :as fr]
            [futon3c.diagramprover.wm-wire :as w]))

(def ^:private at-or-before?
  @(ns-resolve 'futon2.aif.focus-receipt 'at-or-before?))

;; the reader's own setup (relation_derivation_test.clj:18-26)
(def inputs (fr/read-inputs))

(def discovery
  (let [ret (last (sort (map :valid-through (:windows inputs))))
        est (fr/discover inputs ret nil)]
    (fr/discover inputs "2026-09-26T02:00:00Z" {:focus (:focus est) :as-of ret})))

(def as-of "2026-09-26T02:00:00Z")

(defn observe
  "embedding-neighbour (writer) on M-autoclock-in, then classify-target
  through the same derivation, and the reader's read (:derived-via c) as
  relation-derivation-test performs it. TAMPER edits the classification
  before the read (the bad cases). The reader's value is the receipt less
  :stated-relation, the key classify-target assocs as it reads
  (focus_receipt.clj:283-284)."
  ([] (observe identity))
  ([tamper]
   (let [rowed (into {} (keep (fn [r] (when (at-or-before? (:effective-from r) as-of)
                                        [(:target r) r])))
                     (reverse (:relations inputs)))
         e (fr/embedding-neighbour inputs "M-autoclock-in" rowed)
         c (fr/classify-target inputs discovery as-of "M-autoclock-in" {:code-root "/home/joe/code"})
         c' (tamper c)
         dv (:derived-via c')]
     {:writer (:derived-via e)
      :reader (cond (w/typed-absence? dv) dv
                    (map? dv) (dissoc dv :stated-relation)
                    :else {:absent :field-not-carried})
      :stated-relation (:stated-relation (:derived-via c))})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "carries the writer's end: [:decision :selection-certificate :focus-receipt :candidates 0 :derived-via] {:kind :embedding-neighbour :neighbour \"M-aif4iad\" :cosine 0.3777004044318593 ...}; the reader is a test and records nothing"}]))

(def wire
  {:wire [:r9-embedding-neighbour :r9-relation-test :derived-via]
   :kind :witnessed-hermetically
   :test `the-embedding-receipt-reaches-the-relation-test
   :check check
   :live-records-read live-records-read})

(deftest the-embedding-receipt-reaches-the-relation-test
  (let [o (check)]
    (is (= {:kind :embedding-neighbour :neighbour "M-aif4iad" :cosine 0.3777004044318593
            :runner-up ["M-futonzero-generative" 0.11165555990299686]}
           (select-keys (:writer o) [:kind :neighbour :cosine :runner-up]))
        "the writer's receipt, the reader's own live pin")
    (is (= {:absent :no-stated-path-to-a-classified-target} (:stated-relation o)))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(assoc % :derived-via {:absent :embedding-pin-mismatch}))]
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-receipt-at-the-reader-fails-the-wire
  (let [o (observe #(assoc-in % [:derived-via :cosine] 0.0))]
    (is (some? (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-record-carries-the-writers-end
  (let [{:keys [path sha256]} (first live-records-read)]
    (is (= sha256 (w/sha256-file path)))
    (is (= :embedding-neighbour
           (get-in (w/read-record path)
                   [:decision :selection-certificate :focus-receipt :candidates 0 :derived-via :kind])))))
