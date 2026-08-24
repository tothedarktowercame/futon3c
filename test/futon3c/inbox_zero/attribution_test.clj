(ns futon3c.inbox-zero.attribution-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.inbox-zero.attribution :as attribution])
  (:import [java.util Date]))

(defn link [id seat at]
  {:record/type :inbox-zero/session-commit-link
   :link/id id :seat/id seat :linked-at (Date. at)})

(defn edge [agent session target clocked-at]
  {:hx/type :clock/clocked-on
   :hx/endpoints [(str "agent:" agent) (str "mission:" target)]
   :hx/props {"agent-id" agent "session-id" session
              "mission-id" target "clocked-at-ms" clocked-at}})

(defn project [links edges at]
  (attribution/attribute-links links {:basis-instant (Date. at) :edges edges}))

(deftest clocked-seat-at-basis-is-attributed
  (is (= {:link/id "link-1" :seat/id "seat:claude-3:session-1"
          :primary "M-inbox-zero"
          :attribution/basis :clock-lineage-as-of
          :attribution/status :attributed}
         (first (project [(link "link-1" "seat:claude-3:session-1" 200)]
                         [(edge "claude-3" "session-1" "M-inbox-zero" 100)]
                         200)))))

(deftest absent-edge-at-basis-is-unattributed
  (testing "absence models no edge, pre-basis retraction, and post-basis creation"
    (is (= :unattributed
           (:attribution/status
            (first (project [(link "link-1" "seat:claude-3:session-1" 200)]
                            [] 200)))))))

(deftest future-clocked-edge-is-defensively-rejected
  (is (= :unattributed
         (:attribution/status
          (first (project [(link "link-1" "seat:claude-3:session-1" 200)]
                          [(edge "claude-3" "session-1" "M-future" 201)]
                          200))))))

(deftest session-mismatch-is-unattributed
  (is (= :unattributed
         (:attribution/status
          (first (project [(link "link-1" "seat:claude-3:session-1" 200)]
                          [(edge "claude-3" "session-2" "M-other" 100)]
                          200))))))

(deftest simultaneous-distinct-targets-are-ambiguous
  (let [result (first (project [(link "link-1" "seat:claude-3:session-1" 200)]
                               [(edge "claude-3" "session-1" "M-one" 100)
                                (edge "claude-3" "session-1" "M-two" 150)]
                               200))]
    (is (= :ambiguous (:attribution/status result)))
    (is (nil? (:primary result)))))

(deftest io-failure-is-group-local-and-never-unattributed
  (let [links [(link "failed" "seat:claude-3:session-a" 200)
               (link "healthy" "seat:claude-4:session-b" 300)]
        calls (atom [])
        http-get
        (fn [url _]
          (swap! calls conj url)
          (if (str/includes? url "agent%3Aclaude-3")
            {:status 503 :body "unavailable"}
            {:status 200
             :body (pr-str {:hyperedges
                            [(edge "claude-4" "session-b" "M-healthy" 250)]})}))
        state {:schema/version 0 :records (into {} (map (juxt :link/id identity)) links)}
        results (attribution/attribute-state
                 state {:base-url "http://substrate" :http-get http-get})]
    (is (= [:unknown :attributed] (mapv :attribution/status results)))
    (is (= [nil "M-healthy"] (mapv :primary results)))
    (is (= 2 (count @calls)))
    (is (every? #(and (str/includes? % "type=clock%2Fclocked-on")
                      (str/includes? % "as-of=")
                      (str/includes? % "limit=1000"))
                @calls))))
