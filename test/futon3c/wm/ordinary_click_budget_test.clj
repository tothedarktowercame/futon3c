(ns futon3c.wm.ordinary-click-budget-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.transport.http :as http]
            [futon3c.wm.ordinary-click-budget :as budget]
            [futon3c.wm.runner-service :as service]
            #_{:clj-kondo/ignore [:unused-namespace]}
            [futon3c.agency.registry :as reg]
            [futon3c.wm.r10-click-adapter :as r10]
            #_{:clj-kondo/ignore [:unused-namespace]}
            [futon3c.wm.machinery-execution-cohort :as cohort]
            #_{:clj-kondo/ignore [:unused-namespace]}
            [futon2.aif.hermetic-repair-fixture :as hermetic-fixture]))

(defn files [root]
  (count (filter #(.isFile %) (file-seq (io/file root)))))

(defn rows []
  (if (.exists (io/file budget/*ledger-path*))
    (mapv #(json/parse-string % true) (remove str/blank? (str/split-lines (slurp budget/*ledger-path*))))
    []))

;; The allocation is DATA (budget/allocated), renewed whenever Joe grants
;; more; what this test pins is the behaviour at the boundary -- every click
;; up to the allocation issues and is charged before the worker runs, and the
;; one after it refuses with 409. Writing the allocation as a literal here
;; made the test fail on renewal-6 (7 clicks) for no defect at all.
(deftest issued-up-to-the-allocation-then-refused
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                      "ordinary-click-test" (make-array java.nio.file.attribute.FileAttribute 0)))
        production ["/home/joe/code/futon2/data/wm-repair-obligations"
                    "/home/joe/code/futon2/data/wm-tripwires/trips"
                    "/home/joe/code/futon2/data/wm-ordinary-clicks"]
        before (mapv files production)]
    ;; This is the exact live-allocation boundary. Keep it independent of
    ;; runner admission/provider fixtures: those are not budget semantics.
    (binding [budget/*ledger-path* (str root "/consumption.jsonl")]
      (dotimes [n budget/allocated]
        (let [entry (budget/consume! (str "boundary-" n) "now"
                                     (when (zero? n) "codex-34"))]
          (is (= (inc n) (count (rows))))
          (is (= (str "boundary-" n) (:click-id entry)))))
      (let [refusal (try
                      (budget/consume! "one-past-allocation" "now" nil)
                      nil
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (is (= :ordinary-click-budget-exhausted (:error refusal)))
        (is (= budget/authorization (:authorization refusal)))
        (is (= "Joe" (:renewal refusal)))
        (is (= budget/allocated (count (rows)))))
      (is (= "codex-34" (:caller (first (rows)))))
      (is (= "caller-unknown" (:caller (second (rows)))))
      (println "EXECUTION-RECEIPT"
               (pr-str {:issued budget/allocated
                        :one-past-allocation :ordinary-click-budget-exhausted
                        :ledger-rows (count (rows))})))
    (is (= before (mapv files production)))
    (println "PRODUCTION-STORE-COUNTS" (pr-str {:before before :after (mapv files production)}))))

(deftest r10-keeps-its-own-authority
  (let [h (http/make-handler {}) calls (atom 0)]
    (with-redefs [r10/commissioned-click! (fn [_] (swap! calls inc) {:click-id "r10"})
                  budget/consume! (fn [& _] (throw (ex-info "must not charge R10" {})))]
      (is (= 200 (:status (h {:request-method :post :uri "/api/alpha/wm/click"
                             :body "{\"r10-commissioned\":true}"}))))
      (is (= 1 @calls)))))

(deftest concurrent-issues-cannot-overspend
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                      "ordinary-concurrent" (make-array java.nio.file.attribute.FileAttribute 0)))
        contention-allocation 16]
    ;; Contention needs overlapping issuers, not one native thread per live
    ;; campaign credit. The preceding boundary test still exercises all 228
    ;; real credits sequentially and the one-past-allocation refusal.
    (with-redefs [budget/allocated contention-allocation]
      (binding [budget/*ledger-path* (str root "/consumption.jsonl")]
        (let [attempts (mapv (fn [n] (future (try (budget/consume! (str n) (str (java.time.Instant/now)) nil)
                                                :issued
                                                (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
                             (range (+ contention-allocation 7)))
              outcomes (frequencies (mapv deref attempts))]
          (is (= {:issued contention-allocation :ordinary-click-budget-exhausted 7} outcomes))
          (is (= contention-allocation (count (rows)))))))))

(deftest availability-is-source-pinned-and-does-not-consume
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "ordinary-availability" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (binding [budget/*ledger-path* (str root "/consumption.jsonl")]
      (let [empty-receipt (budget/availability)]
        (is (= {:allocated budget/allocated :consumed 0
                :available budget/allocated :unit :ordinary-click}
               (select-keys empty-receipt [:allocated :consumed :available :unit])))
        (is (= 64 (count (get-in empty-receipt [:ledger-source :sha256])))))
      (budget/consume! "click-one" "now" "test")
      (let [receipt (budget/availability)]
        (is (= 1 (:consumed receipt)))
        (is (= (dec budget/allocated) (:available receipt)))
        (is (= 1 (count (rows))))))))

(deftest refund-is-compensating-idempotent-and-click-specific
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "ordinary-refund" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (binding [budget/*ledger-path* (str root "/consumption.jsonl")]
      (budget/consume! "click-one" "issued" "test")
      (is (= 1 (:consumed (budget/availability))))
      (budget/refund! "click-one" "refunded" "joe" "debugger-proved wiring defect")
      (is (= 0 (:consumed (budget/availability))))
      (is (= budget/allocated (:available (budget/availability))))
      (is (= [nil "refund"] (mapv :event (rows))))
      (is (= :ordinary-click-already-refunded
             (try (budget/refund! "click-one" "again" "joe" "duplicate")
                  (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
      (is (= :ordinary-click-refund-unknown
             (try (budget/refund! "not-charged" "now" "joe" "unknown")
                  (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))))))

(deftest busy-click-does-not-consume
  (let [h (http/make-handler {}) calls (atom 0)]
    (reset! service/!status (assoc service/initial-status :running? true :click-id "in-flight"))
    (try
      (with-redefs [budget/consume! (fn [& _] (swap! calls inc))]
        (is (= 409 (:status (h {:request-method :post :uri "/api/alpha/wm/click" :body "{}"}))))
        (is (zero? @calls)))
      (finally (reset! service/!status service/initial-status)))))
