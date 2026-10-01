(ns futon3c.wm.cast-preflight-test
  "Failed runs never refund a grant, so the endpoint refuses known-bad casts
  and the runner admits the ration only after its authoritative readiness read.
  Incident: wm-click-ff7c0384 (2026-09-23) spent a grant on default
  repair-reviewer codex-24, absent from the roster; the run ended
  :agent-unavailable and the ledger kept the spend (claude-5's report)."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.transport.http :as http]
            [futon3c.wm.ordinary-click-budget :as budget]
            [futon3c.wm.runner-service :as service]
            [futon3c.agency.registry :as reg]
            [futon3c.wm.machinery-execution-cohort :as cohort]
            [futon2.aif.hermetic-repair-fixture :refer [with-hermetic-stores]]))

(defn- rows []
  (if (.exists (io/file budget/*ledger-path*))
    (mapv #(json/parse-string % true) (remove str/blank? (str/split-lines (slurp budget/*ledger-path*))))
    []))

(def ^:private cast-seats
  {:author "kimi-2" :reviewer "claude-5" :repair-reviewer "codex-24"})

(defn- roster-with
  "A roster map in the /api/alpha/agents :agents shape."
  [seat->record]
  (into {} (map (fn [[seat record]] [(keyword seat) record])) seat->record))

(def ^:private idle-seat {:status "idle" :invoke-ready? true})

(deftest missing-cast-is-resolved-from-the-live-roster
  (let [roster (roster-with {"claude-17" idle-seat
                             "claude-1" idle-seat
                             "kimi-2" idle-seat
                             "codex-8" idle-seat
                             "zai-4" {:status "restored" :invoke-ready? true}
                             "wm-author" idle-seat
                             "wm-reviewer" {:status "restored" :invoke-ready? true}})]
    (binding [service/*roster-fn* (fn [_] roster)
              service/*resolve-var* (fn [sym]
                                      (when (= sym 'futon2.aif.full-loop-runner/config)
                                        identity))]
      (with-redefs [cohort/apply-binding identity]
        (is (= {:author "wm-author"
                :reviewer "wm-reviewer"
                :repair-reviewer "wm-reviewer"}
               (select-keys (service/prepare-ordinary-click-opts {})
                            [:author :reviewer :repair-reviewer])))))))

(deftest explicit-cast-is-preserved-and-a-single-seat-cannot-self-review
  (binding [service/*roster-fn* (fn [_] (roster-with {"codex-8" idle-seat}))
            service/*resolve-var* (fn [sym]
                                    (when (= sym 'futon2.aif.full-loop-runner/config)
                                      identity))]
    (with-redefs [cohort/apply-binding identity]
      (is (= {:author "zai-4" :reviewer "codex-8" :repair-reviewer "codex-8"}
             (select-keys
              (service/prepare-ordinary-click-opts
               {:author "zai-4" :reviewer "codex-8"})
              [:author :reviewer :repair-reviewer])))
      (let [failure (try
                      (service/prepare-ordinary-click-opts {})
                      nil
                      (catch clojure.lang.ExceptionInfo throwable throwable))]
        (is (= :wm-click-cast-unavailable (:error (ex-data failure))))
        (is (= 409 (:status (ex-data failure))))))))

(defn- run-click-post
  "POST one ordinary click through the real handler with the given roster;
   returns [response parsed-body]. The runner stub fails immediately after
   entry, as the existing budget test's does."
  [h root roster body observed]
  (binding [budget/*ledger-path* (str root "/consumption.jsonl")
            service/*roster-fn* (fn [_agency-base] roster)
            service/*resolve-var*
            (fn [sym]
              (case sym
                futon2.aif.full-loop-runner/config identity
                futon2.aif.full-loop-runtime/run-opportunity!
                (fn [opts]
                  ((:readiness-admitted-fn opts))
                  (swap! observed conj {:click-id (:click-id opts) :rows (rows)})
                  (throw (ex-info "intentional immediate runner failure" {:fixture true})))
                nil))]
    (with-redefs [cohort/apply-binding identity
                  reg/mark-agent-idle! (fn [& _])
                  reg/clear-external-invoke! (fn [& _])]
      (reset! service/!status service/initial-status)
      (let [response (h {:request-method :post :uri "/api/alpha/wm/click"
                         :body (json/generate-string body)})]
        [response (json/parse-string (:body response) true)]))))

(defn- with-fixture [f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "cast-preflight-test" (make-array java.nio.file.attribute.FileAttribute 0)))
        production ["/home/joe/code/futon2/data/wm-repair-obligations"
                    "/home/joe/code/futon2/data/wm-tripwires/trips"
                    "/home/joe/code/futon2/data/wm-ordinary-clicks"]
        before (mapv (fn [p] (count (filter #(.isFile %) (file-seq (io/file p))))) production)
        h (http/make-handler {})]
    (with-hermetic-stores
      (fn [] (f h root)))
    (is (= before (mapv (fn [p] (count (filter #(.isFile %) (file-seq (io/file p))))) production))
        "the production stores are untouched")))

;; Case 1 -- the bad case is the point. The cast names a seat that is not on
;; the roster: the response must be a typed refusal naming it, and the
;; consumption ledger must have THE SAME NUMBER OF ROWS before and after. A
;; refusal that still consumes is the bug wearing a hat.
;;
;; BEFORE this change this request returned 200: the click was issued, the
;; ledger gained a row, and 45 seconds later the run record closed
;; :selection-not-reached via :agent-unavailable -- that is exactly the
;; incident run wm-click-ff7c0384 this test is named for.
(deftest absent-seat-refuses-and-consumes-nothing
  (with-fixture
   (fn [h root]
     (let [observed (atom [])
           roster (roster-with {"kimi-2" idle-seat "claude-5" idle-seat})
           before-count (count (binding [budget/*ledger-path* (str root "/consumption.jsonl")] (rows)))
           [response body] (run-click-post h root roster
                                           {:trigger "duree-click-on-demand"
                                            :author "kimi-2" :reviewer "claude-5"
                                            :repair-reviewer "codex-24"}
                                           observed)
           after-rows (binding [budget/*ledger-path* (str root "/consumption.jsonl")] (rows))]
       (is (= 409 (:status response)) (pr-str body))
       (is (= "wm-click-cast-not-invoke-ready" (:error body)))
       (is (= {:seat "codex-24" :reason "absent"}
              (get-in body [:details :unready :repair-reviewer])))
       (is (= before-count (count after-rows))
           "the ledger has the same number of rows before and after the refusal")
       (is (empty? @observed) "and the runner was never started")))))

;; Case 2 -- a fully present, idle cast still issues and consumes exactly
;; once: the existing behaviour must not move. "restored" counts as
;; available, the rule the script learned on 2026-09-19 (the runner wakes
;; restored seats itself; rejecting them blocked every click after a
;; restart).
(deftest present-idle-cast-still-issues-and-consumes-once
  (with-fixture
   (fn [h root]
     (let [observed (atom [])
           roster (roster-with {"kimi-2" idle-seat
                                "claude-5" {:status "restored" :invoke-ready? true}
                                "codex-24" idle-seat})
           [response body] (run-click-post h root roster
                                           (assoc cast-seats :trigger "duree-click-on-demand")
                                           observed)]
       (is (= 200 (:status response)) (pr-str body))
       (service/await-click! (:click-id body))
       (is (= 1 (count @observed)) "the runner started exactly once")
       (is (= 1 (count (binding [budget/*ledger-path* (str root "/consumption.jsonl")] (rows))))
           "and exactly one row was appended, before the runner entered")))))

;; Case 3 -- a registered but BUSY seat. The script WAITS; the endpoint has
;; no waiting semantics, so it refuses with :busy and consumes nothing, and
;; retrying later is the caller's job. Pinned as chosen behaviour.
(deftest busy-seat-refuses-without-consuming
  (with-fixture
   (fn [h root]
     (let [observed (atom [])
           roster (roster-with {"kimi-2" {:status "invoking" :invoke-ready? true}
                                "claude-5" idle-seat
                                "codex-24" idle-seat})
           [response body] (run-click-post h root roster
                                            (assoc cast-seats :trigger "duree-click-on-demand")
                                            observed)]
       (is (= 409 (:status response)) (pr-str body))
       (is (= {:seat "kimi-2" :reason "busy"}
              (get-in body [:details :unready :author])))
       (is (zero? (count (binding [budget/*ledger-path* (str root "/consumption.jsonl")] (rows)))))
       (is (empty? @observed))))))

(deftest automatically-selected-seat-becoming-busy-refuses-before-consumption
  (with-fixture
   (fn [h root]
     (let [idle-roster (roster-with {"wm-author" idle-seat
                                     "wm-reviewer" idle-seat
                                     ;; Interactive helpers are deliberately
                                     ;; irrelevant to automatic casting.
                                     "codex-11" idle-seat})
           busy-roster (assoc idle-roster :wm-author
                              {:status "invoking" :invoke-ready? true})
           reads (atom 0)
           roster-fn (fn [_]
                       (if (<= (swap! reads inc) 2) idle-roster busy-roster))
           runner-outcome (atom nil)]
       (binding [budget/*ledger-path* (str root "/consumption.jsonl")
                 service/*roster-fn* roster-fn
                 service/*resolve-var*
                 (fn [sym]
                   (case sym
                     futon2.aif.full-loop-runner/config identity
                     futon2.aif.full-loop-runtime/run-opportunity!
                     (fn [opts]
                       ;; Reproduce the runner's authoritative readiness read:
                       ;; wm-author was idle at endpoint preflight and is busy now.
                       (let [roster (roster-fn nil)
                             author (:author opts)]
                         (if (= "invoking" (get-in roster [(keyword author) :status]))
                           (do (reset! runner-outcome :agent-unavailable)
                               (throw (ex-info "Configured author is unavailable"
                                               {:failure-kind :agent-unavailable
                                                :failure-stage :agent-readiness
                                                :failure-detail :busy})))
                           (do ((:readiness-admitted-fn opts))
                               (reset! runner-outcome :admitted)))))
                     nil))]
         (with-redefs [cohort/apply-binding identity
                       reg/mark-agent-idle! (fn [& _])
                       reg/clear-external-invoke! (fn [& _])]
           (reset! service/!status service/initial-status)
           (let [response (h {:request-method :post :uri "/api/alpha/wm/click"
                              :body (json/generate-string
                                     {:trigger "duree-click-on-demand"})})
                 body (json/parse-string (:body response) true)]
             (is (= 409 (:status response)) (pr-str body))
             (is (= "wm-click-cast-not-admitted" (:error body)))
             (is (= :agent-unavailable @runner-outcome))
             (is (zero? (count (rows)))
                 "runner readiness refused before the ration callback"))))))))

;; Case 3b -- a seat that is present but NOT invoke-ready is a refusal too,
;; and an unreadable roster is a 503 (nothing else can be checked), never a
;; silent pass.
(deftest not-invoke-ready-seat-refuses-and-unreadable-roster-is-503
  (with-fixture
   (fn [h root]
     (let [observed (atom [])
           roster (roster-with {"kimi-2" idle-seat
                                "claude-5" {:status "idle" :invoke-ready? false}
                                "codex-24" idle-seat})
           [response body] (run-click-post h root roster
                                           (assoc cast-seats :trigger "duree-click-on-demand")
                                           observed)]
       (is (= 409 (:status response)))
       (is (= {:seat "claude-5" :reason "not-invoke-ready"}
              (get-in body [:details :unready :reviewer]))))
     (let [[response body] (binding [budget/*ledger-path* (str root "/consumption.jsonl")
                          service/*roster-fn* (fn [_] (throw (ex-info "connection refused" {})))
                          service/*resolve-var* (fn [_] nil)]
                  (with-redefs [cohort/apply-binding identity]
                    (let [response (h {:request-method :post :uri "/api/alpha/wm/click"
                                       :body (json/generate-string
                                              (assoc cast-seats :trigger "duree-click-on-demand"))})]
                             [response (json/parse-string (:body response) true)])))]
       (is (= 503 (:status response)) (pr-str body))
       (is (= "wm-click-roster-unavailable" (:error body)))
       (is (zero? (count (binding [budget/*ledger-path* (str root "/consumption.jsonl")] (rows)))))))))
