(ns futon3c.wm.meta-latest-test
  "GET /api/alpha/wm/meta-latest's disk half: the newest retained receipt
  across the attempt trees, mtime-ordered, typed absence when none.  The
  receipt shape (browser-receipt) is futon2's; here it is stubbed by testing
  response's source block and the raw receipt projection only through
  latest-retained-receipt.  Run: clojure -M:test -n futon3c.wm.meta-latest-test"
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.wm.meta-latest :as ml]))

(defn- temp-root []
  (str (java.nio.file.Files/createTempDirectory
        "meta-latest-" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- attempt!
  "Write one attempt tree under ROOT with the given mtime offset and
  checkpoint text; returns the 002-selection.edn path."
  [root cohort tree attempt text mtime-ms]
  (let [dir (java.io.File. root (str cohort "/" tree "/" attempt))
        f (java.io.File. dir "002-selection.edn")]
    (.mkdirs dir)
    (spit f text)
    (.setLastModified f (long mtime-ms))
    (.getAbsolutePath f)))

(def receipt-text-old
  "{:event/schema-version 1
    :payload {:judgment {:selected-cascade :C1
                         :outer-task-selection {:schema :wm/outer-task-selection-v1
                                                :policy {:kind :field}
                                                :support [{:id \"old/one\" :kind :mission :source \"s\"}]
                                                :excluded [{:id \"old/x\" :kind :ticket :source \"s\" :ineligible-reason :not-live}]
                                                :chosen {:id \"old/one\"}
                                                :action {:target \"old/one\"}}}}}")

(def receipt-text-new
  "A file whose FIRST :outer-task-selection mention is a path key (not a
  map), the real receipt comes later — the scanner must try occurrences."
  "{:event/schema-version 1
    :payload {:pin {:at [:outer-task-selection :policy :kind]}
              :judgment {:selected-cascade :C2
                         :outer-task-selection {:schema :wm/outer-task-selection-v1
                                                :policy {:kind :field}
                                                :support [{:id \"new/one\" :kind :mission :source \"s\"}]
                                                :excluded []
                                                :chosen {:id \"new/one\"}
                                                :action {:target \"new/one\"}}}}}")

(def absent-text
  "A typed absence is not a receipt."
  "{:payload {:judgment {:outer-task-selection {:schema :wm/outer-task-selection-v1
                                                :status :absent
                                                :reason :receipt-not-retained}}}}")

(deftest the-newest-retained-receipt-wins-with-its-source
  (let [root (temp-root)
        _ (attempt! root "wm-full-loop-machinery-9" "wm-contract-machinery-9-v1"
                    "attempt-001" receipt-text-old 1000000)
        _ (attempt! root "wm-full-loop-machinery-9" "wm-contract-machinery-9-v1"
                    "attempt-002" absent-text 2000000)
        _ (attempt! root "wm-full-loop-machinery-9" "wm-contract-machinery-9-v1"
                    "attempt-003" receipt-text-new 3000000)
        found (ml/latest-retained-receipt [root])]
    (is (nil? (:absent found)))
    (is (= "new/one" (get-in found [:receipt :chosen :id]))
        "the newest mtime wins, past the typed absence and the path-key mention")
    (is (= "attempt-003" (get-in found [:source :attempt])))
    (is (.endsWith (get-in found [:source :path]) "attempt-003/002-selection.edn"))
    (is (= "wm-contract-machinery-9-v1" (get-in found [:source :tree])))))

(deftest no-receipt-anywhere-is-a-typed-absence
  (let [root (temp-root)
        _ (attempt! root "wm-full-loop-machinery-1" "wm-contract-machinery-1-v1"
                    "attempt-001" absent-text 1000000)
        found (ml/latest-retained-receipt [root])]
    (is (= ::ml/no-retained-receipt (:absent found)))
    (is (= :no-retained-receipt (:reason found)))
    (is (= 1 (:scanned found))))
  (let [root (temp-root)]
    ;; empty root directory: zero files scanned, still typed
    (is (= ::ml/no-retained-receipt
           (:absent (ml/latest-retained-receipt [root]))))))

(deftest response-shape-matches-meta-preview-and-404s-typed
  (let [root (temp-root)
        _ (attempt! root "wm-full-loop-machinery-2" "wm-contract-machinery-2-v1"
                    "attempt-001" receipt-text-old 1000000)
        [status body] (ml/response [root])]
    (is (= 200 status))
    (is (:ok body))
    (is (get-in body [:receipt :chosen]))
    (is (= "attempt-001" (get-in body [:source :attempt]))))
  (let [root (temp-root)
        [status body] (ml/response [root])]
    (is (= 404 status))
    (is (false? (:ok body)))
    (is (= :no-retained-receipt (:error body)))))

(deftest g-frontier-lists-every-scored-candidate-by-rank
  (let [receipt {:policy {:canonical-g-frontier
                          {:status :produced :horizon-choice {:horizon 10}
                           :entries [{:target-id "B" :rank 2 :G -1.0 :cascade-digest "d2" :belief :dropped}
                                     {:target-id "A" :rank 1 :G -2.0 :cascade-digest "d1"}
                                     {:target-id "C" :status :refused :reason :canonical-action-unavailable}]}}}
        f (ml/g-frontier receipt)]
    (is (= ["A" "B" "C"] (mapv :target-id (:entries f))) "ranked first, unranked last")
    (is (= 10 (:horizon f)))
    (is (nil? (:belief (second (:entries f)))) "only the summary keys are kept")
    (is (= :canonical-action-unavailable (:reason (last (:entries f))))))
  (is (nil? (ml/g-frontier {:policy {}})) "no frontier, no summary"))

;; Live pin: wm-full-loop-machinery-150 attempt-002 002-selection.edn
;; (:selection-as-of "2026-10-24T20:40:24.272212785Z"). q4 rows by index and
;; the frontier's scored entries, values verbatim.
(def sil "M-self-improvement-loop")
(def sil-digest "f79e6e5b560469f5f1a5edb69cda2e990a8f33fd6aa6228c13913e06fb031b4f")
(def sg "M-stack-geometry")
(def sg-digest "c601dc5697a2d5d65c5c8440f08b3905cee1030b9a62dfa03e2ad5e7ab202c27")
(def wmw "M-war-machine-wiring")
(def wmw-digest "3a5228e7942579f3e0095ee4a902b1b08a0a9d0bf7a11c966c52bddbbb4e4475")
(def rb "M-recommendation-bindings")
(def rb-digest "13c0068ddbc12266370e59d82812d9b10472a38831e7e61f0e9aef6a9e690e11")

(defn- pinned-q4-row [target digest]
  {:target-id target :policy-id (str target "/pinned") :cascade-digest digest
   :canonical-cascade {:digest digest} :posterior {:pinned true} :terms {:pinned true}})

(defn- pinned-receipt [& {:as overrides}]
  (merge {:policy {:canonical-g-frontier
                   {:status :produced :horizon-choice {:horizon 10}
                    :entries [{:target-id sil :status :scored :rank 2 :G -3.944
                               :policy-id (str sil "/p2") :cascade-digest sil-digest}
                              {:target-id sg :status :scored :rank 3 :G -2.004
                               :policy-id (str sg "/p3") :cascade-digest sg-digest}
                              {:target-id wmw :status :scored :rank 1 :G -3.956
                               :policy-id (str wmw "/p1") :cascade-digest wmw-digest}
                              {:target-id rb :status :scored :rank 1 :G -3.956
                               :policy-id (str rb "/p1") :cascade-digest rb-digest}]}}
          :outer-policy-recomputation
          {:q4 [(pinned-q4-row sil sil-digest)
                (pinned-q4-row sg sg-digest)
                (pinned-q4-row wmw wmw-digest)
                (pinned-q4-row rb rb-digest)]}}
         overrides))

(deftest g-frontier-locates-each-entrys-cascade-in-q4
  (let [receipt (pinned-receipt)
        f (ml/g-frontier receipt)
        entries (:entries f)
        by-target (into {} (map (juxt :target-id identity)) entries)]
    (testing "each entry carries the q4 index path of its cascade, pinned"
      (is (= [rb wmw sil sg] (mapv :target-id entries)))
      (is (= [:outer-policy-recomputation :q4 2] (:cascade-at (by-target wmw))))
      (is (= [:outer-policy-recomputation :q4 3] (:cascade-at (by-target rb))))
      (is (= [:outer-policy-recomputation :q4 0] (:cascade-at (by-target sil))))
      (is (= [:outer-policy-recomputation :q4 1] (:cascade-at (by-target sg))))
      (doseq [e entries]
        (is (= (:cascade-digest e)
               (:cascade-digest (get-in receipt (:cascade-at e)))))))
    (testing "summary keys and order are otherwise unchanged"
      (is (= [{:target-id rb :policy-id (str rb "/p1") :status :scored :rank 1
               :G -3.956 :cascade-digest rb-digest}
              {:target-id wmw :policy-id (str wmw "/p1") :status :scored :rank 1
               :G -3.956 :cascade-digest wmw-digest}
              {:target-id sil :policy-id (str sil "/p2") :status :scored :rank 2
               :G -3.944 :cascade-digest sil-digest}
              {:target-id sg :policy-id (str sg "/p3") :status :scored :rank 3
               :G -2.004 :cascade-digest sg-digest}]
             (mapv #(dissoc % :cascade-at) entries))))
    (testing "a digest with no q4 row is a typed absence"
      (let [orphan (assoc-in receipt [:policy :canonical-g-frontier :entries]
                             (conj (get-in receipt [:policy :canonical-g-frontier :entries])
                                   {:target-id "M-orphan" :status :scored :rank 4
                                    :G -1.0 :policy-id "M-orphan/p"
                                    :cascade-digest "0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f"}))
            orphan-entry (some #(when (= "M-orphan" (:target-id %)) %)
                               (:entries (ml/g-frontier orphan)))]
        (is (= {:absent :cascade-not-retained} (:cascade-at orphan-entry))))
      (let [no-recompute (dissoc receipt :outer-policy-recomputation)
            absent-entries (:entries (ml/g-frontier no-recompute))]
        (is (= 4 (count absent-entries)))
        (is (every? #(= {:absent :cascade-not-retained} (:cascade-at %))
                    absent-entries)))
      (let [one-char-off (assoc-in receipt
                                   [:outer-policy-recomputation :q4 2 :cascade-digest]
                                   (str (subs wmw-digest 0 63) "0"))
            entries' (:entries (ml/g-frontier one-char-off))
            wmw' (some #(when (= wmw (:target-id %)) %) entries')]
        (is (not= wmw-digest
                  (get-in one-char-off [:outer-policy-recomputation :q4 2 :cascade-digest])))
        (is (= {:absent :cascade-not-retained} (:cascade-at wmw')))))))

(deftest strings-and-char-literals-inside-a-receipt-do-not-break-balancing
  (let [text "{:payload {:judgment {:outer-task-selection {:schema :wm/outer-task-selection-v1
   :note \"braces }{ inside \\\" a string\"
   :c \\}
   :chosen {:id \"ok/one\"}}}}"
        v (ml/receipt-in text)]
    (is (map? v))
    (is (= "ok/one" (get-in v [:chosen :id])))))
