(ns futon3c.diagramprover.wm-wire-r9-embedding-neighbour-classify-target-derived-via-test
  "Wire [:r9-embedding-neighbour :r9-classify-target :derived-via]: the
  embedding derivation receipt reaching classify-target, which returns it
  as its own :derived-via (focus_receipt.clj:279-286).

  VERIFIED on the seventh flight's run record: the recorded
  classification's :derived-via is {:kind :embedding-neighbour :neighbour
  \"M-aif4iad\" :cosine 0.3777004044318593 ...} carrying
  :stated-relation {:absent :no-stated-path-to-a-classified-target} — the
  key classify-target assocs onto the receipt as it reads it
  (focus_receipt.clj:283-284), so the record shows the receipt after the
  reader's touch. The writer's end is the same map less that key; the
  reader's end is the recorded :derived-via with the reader's addition
  projected away. The live pin agrees with the reader box's own live test
  (relation-derivation-test/m-autoclock-in-live, same neighbour, same
  cosine, same pin shas)."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]))

(def record-path
  (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"))

(def record-sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa")

(def classification-path [:decision :selection-certificate :focus-receipt :candidates 0])

(defn observe
  "Both ends off the pinned record: {:writer the embedding receipt as
  embedding-neighbour wrote it (the recorded :derived-via less the reader's
  :stated-relation addition), :reader the same receipt as classify-target
  received it}. TAMPER edits the record in memory before the read (the bad
  cases)."
  ([] (observe identity))
  ([tamper]
   (let [r (w/read-record record-path)
         dv (get-in r (conj classification-path :derived-via))
         dv' (get-in (tamper r) (conj classification-path :derived-via))]
     {:writer (dissoc dv :stated-relation)
      :reader (if (w/typed-absence? dv') dv' (dissoc dv' :stated-relation))
      :stated-relation (:stated-relation dv)
      :raw dv})))

(defn check [] (observe))

(def wire
  {:wire [:r9-embedding-neighbour :r9-classify-target :derived-via]
   :kind :verified
   :test `the-embedding-receipt-reaches-classify-target
   :check check
   :record {:path record-path :sha256 record-sha256}})

(deftest the-embedding-receipt-reaches-classify-target
  (is (= record-sha256 (w/sha256-file record-path)) "the pinned record before it is read")
  (let [o (check)]
    (is (= {:kind :embedding-neighbour :neighbour "M-aif4iad" :cosine 0.3777004044318593
            :runner-up ["M-futonzero-generative" 0.11165555990299686]}
           (select-keys (:raw o) [:kind :neighbour :cosine :runner-up]))
        "the writer's receipt, agreeing with relation-derivation-test's live pin")
    (is (= {:absent :no-stated-path-to-a-classified-target} (:stated-relation o))
        "the reader's own addition, proving the receipt passed through classify-target")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(assoc-in % (conj classification-path :derived-via) {:absent :embedding-not-pinned}))]
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-receipt-at-the-reader-fails-the-wire
  (let [o (observe #(assoc-in % (conj classification-path :derived-via :cosine) 0.0))]
    (is (some? (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))
