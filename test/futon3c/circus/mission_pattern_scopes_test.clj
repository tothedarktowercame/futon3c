(ns futon3c.circus.mission-pattern-scopes-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.circus.routes :as routes]))

(defn- fake-fetch [calls]
  (fn [mission _url binders]
    (swap! calls conj [mission binders])
    {:scopes [{:id "s1" :type "pattern" :title "pattern: invented/one"
               :passage (apply str (repeat 300 "x")) :anchor_state "resolved" :hx_id "hx-1"
               :mission mission}
              {:id "s2" :type "psr" :title "psr: invented/two" :passage "short" :hx_id "hx-2"}
              {:id "s3" :type "loose-section" :title "Not a pattern" :passage "p" :hx_id "hx-3"}]}))

(deftest mission-pattern-scopes-are-only-pattern-binders
  (let [calls (atom [])
        {:keys [status body]} (routes/mission-pattern-scopes-response
                               {"mission" "M-invented"} (fake-fetch calls))]
    (is (= 200 status))
    (is (= [["M-invented" ["pattern" "psr" "pur"]]] @calls)
        "asks the substrate only for the three pattern binders")
    (is (= ["s1" "s2"] (map #(get % "id") (get body "scopes"))))
    (is (= 200 (count (get-in body ["scopes" 0 "passage"]))) "passages are clipped")
    (is (= "short" (get-in body ["scopes" 1 "passage"])))))

(deftest mission-pattern-scopes-refusals
  (is (= 400 (:status (routes/mission-pattern-scopes-response {} (fn [& _] (throw (ex-info "unused" {})))))))
  (is (= 503 (:status (routes/mission-pattern-scopes-response
                       {"mission" "M-invented"} (fn [& _] (throw (ex-info "substrate down" {})))))))
  (is (= 200 (:status (routes/mission-pattern-scopes-response
                       {"mission" "M-invented"} (fn [& _] {:scopes []}))))))
