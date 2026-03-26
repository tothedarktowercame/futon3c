(ns futon3c.logic.arxana-bridge-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [futon3c.logic.arxana-bridge :as bridge]
            [futon3c.logic.invariant-runner :as runner]
            [futon1a.system :as sys])
  (:import (java.net URI)
           (java.net URLEncoder)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)
           (java.nio.file Files)))

(defn- temp-dir []
  (-> (Files/createTempDirectory "futon3c-arxana-bridge-"
                                 (make-array java.nio.file.attribute.FileAttribute 0))
      (.toFile)
      (.getAbsolutePath)))

(defn- http-client [] (HttpClient/newHttpClient))

(defn- http-get-json
  [client url]
  (let [req (-> (HttpRequest/newBuilder (URI/create url))
                (.GET)
                (.header "accept" "application/json")
                (.build))
        resp (.send client req (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode resp)
     :body (json/decode (.body resp) true)}))

(deftest obligation->hyperedge-builds-stable-invariant-shape
  (testing "structural-law obligations become invariant/violation hyperedges"
    (let [hx (bridge/obligation->hyperedge
              {:id "INV-mission-missing-blockers-001"
               :label "Repair mission cycle blocker reference: [\"C1\" \"O-x\" :blocker]"
               :summary "Mission cycle points at a blocker that is absent from the obligation ledger."
               :domain-id :mission
               :violation-key :missing-blockers
               :actionability :auto-fixable
               :family :existence
               :dispatchable? true
               :payload ["C1" "O-x" :blocker]})]
      (is (= "invariant/violation" (:hx/type hx)))
      (is (= ["inv:existence/missing-blockers"
              "mission-cycle:C1"
              "mission-obligation:O-x"]
             (:hx/endpoints hx)))
      (is (= "hx:invariant/violation:inv:existence/missing-blockers|mission-cycle:C1|mission-obligation:O-x"
             (:hx/id hx)))
      (is (= "missing-blockers" (get-in hx [:props :rule])))
      (is (= "mission" (get-in hx [:props :domain]))))))

(deftest aggregate->hyperedges-projects-all-obligations
  (testing "aggregate runner output projects into one hyperedge per obligation"
    (let [aggregate (runner/run-aggregate
                     {:mission true
                      :agency true}
                     [{:domain-id :mission
                       :input nil
                       :check (fn [_]
                                {:missing-blockers [["C1" "O-x" :blocker]]})}
                      {:domain-id :agency
                       :input nil
                       :check (fn [_]
                                {:entry-exit-asymmetry [[:mission :explore]]})}])
          hyperedges (bridge/aggregate->hyperedges aggregate)]
      (is (= 2 (count hyperedges)))
      (is (= #{"invariant/violation"} (set (map :hx/type hyperedges))))
      (is (some #(= "Review hop asymmetry from :mission to :explore"
                    (get-in % [:props :label]))
                hyperedges)))))

(deftest emit-aggregate-roundtrips-through-futon1a
  (testing "bridge emits invariant/violation hyperedges queryable by type"
    (let [dir (temp-dir)
          client (http-client)
          sys1 (sys/start! {:data-dir dir
                            :port 0
                            :allowed-penholders #{"tester"}})
          base (str "http://127.0.0.1:" (:http/port sys1))]
      (try
        (let [aggregate (runner/run-aggregate
                         {:mission true
                          :codex true}
                         [{:domain-id :mission
                           :input nil
                           :check (fn [_]
                                    {:missing-blockers [["C1" "O-x" :blocker]]})}
                          {:domain-id :codex
                           :input nil
                           :check (fn [_]
                                    {:orphan-announcements [["a-1" "missing-job"]]
                                     :running-session-mismatches [["job-1" "codex-1" "sid-job" "sid-reg"]]})}])
              emitted (bridge/emit-aggregate! aggregate {:futon1a-url base
                                                         :penholder "tester"
                                                         :client client})
              queried (http-get-json client (str base "/api/alpha/hyperedges?type=invariant/violation"))]
          (is (= 3 (:count emitted)))
          (is (= 3 (:ok-count emitted)))
          (is (= 200 (:status queried)))
          (is (= 3 (get-in queried [:body :count])))
          (is (= #{"invariant/violation"}
                 (set (map :hx/type (get-in queried [:body :hyperedges])))))
          (is (some #(= "running-session-mismatches" (get-in % [:hx/props :rule]))
                    (get-in queried [:body :hyperedges]))
              "stored hyperedges keep the rule in :hx/props on readback"))
        (finally
          ((:stop! sys1)))))))

(deftest reconcile-aggregate-marks-cleared-hyperedges-inactive
  (testing "previously emitted violations that disappear from the aggregate are deactivated rather than left live"
    (let [dir (temp-dir)
          client (http-client)
          sys1 (sys/start! {:data-dir dir
                            :port 0
                            :allowed-penholders #{"tester"}})
          base (str "http://127.0.0.1:" (:http/port sys1))]
      (try
        (let [aggregate-1 (runner/run-aggregate
                           {:mission true
                            :codex true}
                           [{:domain-id :mission
                             :input nil
                             :check (fn [_]
                                      {:missing-blockers [["C1" "O-x" :blocker]]})}
                            {:domain-id :codex
                             :input nil
                             :check (fn [_]
                                      {:orphan-announcements [["a-1" "missing-job"]]})}])
              emitted-1 (bridge/reconcile-aggregate!
                         aggregate-1
                         {:futon1a-url base
                          :penholder "tester"
                          :client client})
              aggregate-2 (runner/run-aggregate
                           {:codex true}
                           [{:domain-id :codex
                             :input nil
                             :check (fn [_]
                                      {:orphan-announcements [["a-1" "missing-job"]]})}])
              emitted-2 (bridge/reconcile-aggregate!
                         aggregate-2
                         {:previous-hyperedges (:current-hyperedges emitted-1)
                          :futon1a-url base
                          :penholder "tester"
                          :client client})
              mission-hx-id "hx:invariant/violation:inv:existence/missing-blockers|mission-cycle:C1|mission-obligation:O-x"
              mission-hx (http-get-json client (str base "/api/alpha/hyperedge/"
                                                   (URLEncoder/encode mission-hx-id "UTF-8")))
              queried (http-get-json client (str base "/api/alpha/hyperedges?type=invariant/violation"))]
          (is (= 2 (:current-count emitted-1)))
          (is (= 1 (:current-count emitted-2)))
          (is (= 1 (:cleared-count emitted-2)))
          (is (= 200 (:status mission-hx)))
          (is (= false (get-in mission-hx [:body :hx/props :active])))
          (is (= "cleared" (get-in mission-hx [:body :hx/props :state])))
          (is (string? (get-in mission-hx [:body :hx/props :cleared-at])))
          (is (= 2 (get-in queried [:body :count]))
              "query still returns both docs; inactive filtering is a browser concern"))
        (finally
          ((:stop! sys1)))))))
