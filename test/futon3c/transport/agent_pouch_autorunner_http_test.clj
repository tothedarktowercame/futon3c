(ns futon3c.transport.agent-pouch-autorunner-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.agent-pouch :as agent-pouch]
            [futon3c.transport.http :as http]))

(defn- prompt [origin]
  (str "--- CURRENT TURN ---\nSurface: bell\nOrigin: " origin "\n---\nwork"))

(defn- pouch-state [handler]
  (-> (handler {:request-method :get
                :uri "/api/alpha/agents/autorunner-http/pouch"
                :query-string "session=autorunner-session"})
      :body
      (json/parse-string true)))

(deftest pouch-endpoint-reports-autorunner-for-requested-session
  (let [handler (http/make-handler {})]
    (dotimes [_ 6]
      (agent-pouch/note-turn-origin!
       "autorunner-http" "autorunner-session" (prompt "harness")))
    (is (= {:turns-without-operator 6 :autorunner true}
           (select-keys (pouch-state handler)
                        [:turns-without-operator :autorunner])))
    (agent-pouch/note-turn-origin!
     "autorunner-http" "autorunner-session" (prompt "operator"))
    (is (= {:turns-without-operator 0 :autorunner false}
           (select-keys (pouch-state handler)
                        [:turns-without-operator :autorunner])))))
