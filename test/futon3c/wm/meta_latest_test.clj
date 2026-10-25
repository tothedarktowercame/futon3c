(ns futon3c.wm.meta-latest-test
  "GET /api/alpha/wm/meta-latest's disk half: the newest retained receipt
  across the attempt trees, mtime-ordered, typed absence when none.  The
  receipt shape (browser-receipt) is futon2's; here it is stubbed by testing
  response's source block and the raw receipt projection only through
  latest-retained-receipt.  Run: clojure -M:test -n futon3c.wm.meta-latest-test"
  (:require [clojure.test :refer [deftest is]]
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

(deftest strings-and-char-literals-inside-a-receipt-do-not-break-balancing
  (let [text "{:payload {:judgment {:outer-task-selection {:schema :wm/outer-task-selection-v1
   :note \"braces }{ inside \\\" a string\"
   :c \\}
   :chosen {:id \"ok/one\"}}}}"
        v (ml/receipt-in text)]
    (is (map? v))
    (is (= "ok/one" (get-in v [:chosen :id])))))
