(ns futon3c.diagramprover.wm-wire-flight-click-flight-record-click-detail-test
  "Wire [:flight-click :flight-record-click :detail]: the server's reason for a click it
  did not start (http-click-fn's abstention :detail: the body's
  error/message/rejected/unready/details, WM-SPIKE-FIX-I A: typed, never
  nil) reaching the flight record's click
  entry — record-click keeps it under the entry's :abstention
  (select-keys [:kind :missing :declines :status :detail]).

  No live record carries both ends: the flights whose clicks were not
  started (d00574c8, abstention {:kind :click-not-started :missing
  :click}; 6cda5ee8, {:kind :run-record-missing :missing :run-record})
  predate WM-SPIKE-FIX-I A, so their entries carry no :detail; the eighth
  flight's click started and has no abstention (see live-records-read,
  each pinned). So the wire is WITNESSED-HERMETICALLY: http-click-fn (the
  writer's var) is driven with a stubbed post! answering 409 (the cast
  preflight's refusal), and its result is handed to record-click (the
  reader's var); the writer's value is the abstention's :detail as
  http-click-fn wrote it, the reader's the click entry's."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as fr]
            [futon3c.diagramprover.wm-wire :as w]))

(defn- refusal-body [seat]
  {:error "wm-click-cast-not-invoke-ready" :unready [{:seat seat :reason :not-invoke-ready}]})

(defn- click-result
  "http-click-fn's abstention for a click the server refused with STATUS
  and BODY."
  [status body]
  (let [cf (fr/http-click-fn {:today (constantly "2026-09-26")
                              :post! (constantly {:status status :body body})
                              :get-status! (fn [] (throw (ex-info "should not poll" {})))})]
    (cf {:flight {:flight/id "f" :target "M" :click 1}})))

(defn- entry
  "The click entry record-click writes for CLICK."
  [click]
  (first (:clicks (flight/record-click
                   (flight/start {:target "M" :chosen-because {:kind :requested}}
                                 {:kind :a-exits :repo "futon3c" :path "p" :read-text (fn [& _] "")}
                                 {:id "flight-wire"})
                   (merge click {:wants [] :before {} :after {}})))))

(defn observe [status body]
  (let [result (click-result status body)
        e (entry result)]
    {:writer (get-in result [:abstention :detail])
     :reader (get-in e [:abstention :detail])
     :entry-abstention (:abstention e)}))

(defn check [] (observe 409 (refusal-body "claude-6")))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-d00574c8.edn")
      :sha256 "68e531b462d535983ea5114a8da370daec943a3b4283b6b87831e63ecf9eadd8"
      :why "the reader's end absent: its click entry's abstention is {:kind :click-not-started :missing :click} with no :detail — written before WM-SPIKE-FIX-I A typed the refusal"}
     {:path (p "flight-6cda5ee8.edn")
      :sha256 "b26d4c3cc98009b1f7a828cd2355f6b01bb03feaeda8f03bf43a8a94f0292e34"
      :why "its click entry's abstention {:kind :run-record-missing :missing :run-record} is record-summary's, a different writer's; no :detail"}
     {:path (p "flight-ada87008/flight-ada87008.edn")
      :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"
      :why "the click started (200): no abstention, so no :detail at either end"}]))

(def wire
  {:wire [:flight-click :flight-record-click :detail]
   :kind :witnessed-hermetically
   :test `the-refused-clicks-detail-reaches-the-click-entry
   :check check
   :live-records-read live-records-read})

(deftest the-refused-clicks-detail-reaches-the-click-entry
  (let [o (check)]
    (is (= {:error "wm-click-cast-not-invoke-ready" :unready [{:seat "claude-6" :reason :not-invoke-ready}]}
           (:writer o))
        "the server's reason, as http-click-fn recorded it")
    (is (= :click-not-started (get-in o [:entry-abstention :kind])))
    (is (w/received? o))))

(deftest an-unanswered-clicks-typed-absent-detail-fails-the-wire
  ;; post! throws: http-click-fn types the detail {:absent :no-response :message ...}
  (let [cf (fr/http-click-fn {:today (constantly "2026-09-26")
                              :post! (fn [_] (throw (ex-info "connection refused" {})))
                              :get-status! (fn [] (throw (ex-info "should not poll" {})))})
        result (cf {:flight {:flight/id "f" :target "M" :click 1}})
        e (entry result)
        o {:writer (get-in result [:abstention :detail])
           :reader (get-in e [:abstention :detail])}]
    (is (= {:absent :no-response :message "connection refused"} (:writer o)))
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-detail-fails-the-wire
  (let [o (check)
        other (observe 409 (refusal-body "claude-13"))]
    (is (= {:error "wm-click-cast-not-invoke-ready" :unready [{:seat "claude-13" :reason :not-invoke-ready}]}
           (:reader other)))
    (is (not (w/received? (assoc o :reader (:reader other)))))))

(deftest the-live-records-carry-no-detail
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [entries (:clicks (:flight (w/read-record path)))]
      (is (every? #(nil? (get-in % [:abstention :detail])) entries) path)))
  (is (= {:kind :click-not-started :missing :click}
         (:abstention (first (:clicks (:flight (w/read-record (:path (first live-records-read))))))))))
