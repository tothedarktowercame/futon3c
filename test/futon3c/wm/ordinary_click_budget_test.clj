(ns futon3c.wm.ordinary-click-budget-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.transport.http :as http]
            [futon3c.wm.ordinary-click-budget :as budget]
            [futon3c.wm.runner-service :as service]
            [futon3c.agency.registry :as reg]
            [futon3c.wm.r10-click-adapter :as r10]
            [futon3c.wm.machinery-execution-cohort :as cohort]
            [futon2.aif.hermetic-repair-fixture :refer [with-hermetic-stores]]))

(defn files [root]
  (count (filter #(.isFile %) (file-seq (io/file root)))))

(defn rows []
  (if (.exists (io/file budget/*ledger-path*))
    (mapv #(json/parse-string % true) (remove str/blank? (str/split-lines (slurp budget/*ledger-path*))))
    []))

(deftest issued-before-failure-and-sixth-refused
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                      "ordinary-click-test" (make-array java.nio.file.attribute.FileAttribute 0)))
        production ["/home/joe/code/futon2/data/wm-repair-obligations"
                    "/home/joe/code/futon2/data/wm-tripwires/trips"
                    "/home/joe/code/futon2/data/wm-ordinary-clicks"]
        before (mapv files production)
        observed (atom [])
        h (http/make-handler {})]
    (with-hermetic-stores
      (fn []
        (binding [budget/*ledger-path* (str root "/consumption.jsonl")
                  service/*resolve-var*
                  (fn [sym]
                    (case sym
                      futon2.aif.full-loop-runner/config identity
                      futon2.aif.full-loop-runner/run-opportunity!
                      (fn [opts]
                        (swap! observed conj {:click-id (:click-id opts) :rows (rows)})
                        (throw (ex-info "intentional immediate runner failure" {:fixture true})))
                      nil))]
          (with-redefs [cohort/apply-binding identity
                        reg/mark-agent-idle! (fn [& _])
                        reg/clear-external-invoke! (fn [& _])]
            (reset! service/!status service/initial-status)
            (dotimes [n 5]
              (let [response (h {:request-method :post :uri "/api/alpha/wm/click"
                                 :body (json/generate-string
                                        (cond-> {:trigger "duree-click-on-demand"}
                                          (zero? n) (assoc :issuing-caller "codex-34")))})
                    body (json/parse-string (:body response) true)]
                (is (= 200 (:status response)))
                (service/await-click! (:click-id body))
                (is (= :service-failed (get-in @service/!status [:last-result :outcome])))
                (is (= (inc n) (count (rows))))))
            (let [response (h {:request-method :post :uri "/api/alpha/wm/click" :body "{}"})
                  body (json/parse-string (:body response) true)]
              (is (= 409 (:status response)))
              (is (= "ordinary-click-budget-exhausted" (:error body)))
              (is (= budget/authorization (get-in body [:details :authorization])))
              (is (= "Joe" (get-in body [:details :renewal])))
              (is (= 5 (count (rows))))
              (is (= 5 (count @observed))))
            (is (= [1 2 3 4 5] (mapv #(count (:rows %)) @observed)))
            (is (every? #(= (:click-id %) (:click-id (last (:rows %)))) @observed))
            (is (= "codex-34" (:caller (first (rows)))))
            (is (= "caller-unknown" (:caller (second (rows)))))
            (println "EXECUTION-RECEIPT" (pr-str {:issued 5 :failed-after-issue 5
                                                 :sixth-status 409 :worker-entry-counts [1 2 3 4 5]}))))))
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
                      "ordinary-concurrent" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (binding [budget/*ledger-path* (str root "/consumption.jsonl")]
      (let [attempts (mapv (fn [n] (future (try (budget/consume! (str n) (str (java.time.Instant/now)) nil)
                                              :issued
                                              (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
                           (range 12))
            outcomes (frequencies (mapv deref attempts))]
        (is (= {:issued 5 :ordinary-click-budget-exhausted 7} outcomes))
        (is (= 5 (count (rows))))))))

(deftest busy-click-does-not-consume
  (let [h (http/make-handler {}) calls (atom 0)]
    (reset! service/!status (assoc service/initial-status :running? true :click-id "in-flight"))
    (try
      (with-redefs [budget/consume! (fn [& _] (swap! calls inc))]
        (is (= 409 (:status (h {:request-method :post :uri "/api/alpha/wm/click" :body "{}"}))))
        (is (zero? @calls)))
      (finally (reset! service/!status service/initial-status)))))
