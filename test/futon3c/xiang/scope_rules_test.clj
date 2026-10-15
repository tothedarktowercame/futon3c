(ns futon3c.xiang.scope-rules-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [futon3c.transport.http :as http]
            [futon3c.xiang.scope-rules :as scopes]))

(def fixture
  (edn/read-string (slurp (io/resource "futon3c/xiang/turn_acts_fixtures/scope-session.edn"))))
(def acts (vec (mapcat :acts fixture)))

(defn- response-body [response]
  (json/parse-string (:body response) true))

(deftest within-edges-use-the-labelled-r3-opener-and-r1-fallback
  (let [edges (scopes/within-edges acts)
        isolated [{:id "isolated-f-s1-0" :kind :explore :author "operator"
                   :turn "isolated" :at "2026-09-28T23:02:00Z"}
                  {:id "isolated-f-s2-0" :kind :explore :author "operator"
                   :turn "isolated" :at "2026-09-28T23:02:00Z"}]]
    (is (some #(= {:act "claude-18-turn-87-f-s2-0"
                   :within "claude-18-turn-87-f-s1-0"
                   :rule "R3" :opener-kind :propose} %)
              edges))
    (is (not-any? #(= "claude-18-turn-87-f-s1-0" (:act %)) edges))
    (is (some #(and (= "fixture-turn-2-f-s1-0" (:act %))
                    (= "R3" (:rule %))) edges))
    (is (= [{:act "isolated-f-s2-0" :within "isolated-f-s1-0"
             :rule "R1" :opener-kind :explore}]
           (scopes/within-edges isolated)))))

(deftest scope-for-turn-is-this-turn-plus-one-previous-reply
  (let [edges (scopes/scope-for-turn acts "fixture-turn-3")]
    (is (every? #{"fixture-turn-3-f-s1-0" "fixture-turn-2-r-0"
                 "fixture-turn-2-r-1"}
                (map :act edges)))
    (is (= #{"fixture-turn-3-f-s1-0" "fixture-turn-2-r-0" "fixture-turn-2-r-1"}
           (set (map :id (scopes/acts-for-turn acts "fixture-turn-3")))))))

(deftest scopes-route-returns-offset-acts-and-404s-unknown-turn
  (let [service {:config {:scope-acts {"fixture-session" acts}}}
        route (ns-resolve 'futon3c.transport.http 'handle-xiang)
        request {:request-method :get :uri "/api/alpha/xiang/scopes"
                 :query-string "session=fixture-session&turn=fixture-turn-2"}]
    (with-redefs-fn {#'http/xiang-turn-service (constantly service)}
      (fn []
        (let [body (response-body (route request {}))]
          (is (= 200 (:status (route request {}))))
          (is (= "fixture-turn-2" (:turn body)))
          (is (= scopes/version (:rules-version body)))
          (is (some #(= {:id "fixture-turn-2-r-0" :kind "report"
                         :text "reported progress" :start 0 :end 16 :side "agent"}
                        %)
                    (:acts body))))))
    (let [bad (route (assoc request :query-string
                            "session=fixture-session&turn=missing") {})]
      (is (= 404 (:status bad))))))
