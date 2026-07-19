(ns futon3c.live-efe-map-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.live-efe-map :as live]))

(defn- private-var [sym]
  (or (ns-resolve 'futon3c.live-efe-map sym)
      (throw (ex-info "Missing live-map var" {:symbol sym}))))

(deftest capability-zones-static-projection-is-served
  (let [zones ((var-get (private-var 'capability-zones)))]
    (testing "the version-pinned static artifact is complete"
      (is (= "pca3-v1" (get zones "reduction-version")))
      (is (= 14 (count (get zones "legend"))))
      (is (= 269 (count (get zones "items")))))
    (testing "build-response exposes the projection without changing live layers"
      (with-redefs-fn
        {(private-var 'coordinate-map) (constantly {})
         (private-var 'mission-doc-index) (constantly {})
         (private-var 'durable-clock-by-agent) (constantly {})
         (private-var 'wm-ticks) (fn [_ _] [])
         (private-var 'recent-coordination) (constantly {:count 0 :items []})}
        #(let [response (live/build-response
                         {:registry {:agents {}}
                          :invoke-jobs [] :evidence-store nil :wm-limit 0})]
           (is (= zones (:capability-zones response)))
           (is (nil? (:ship response)))
           (is (= [] (get-in response [:agents :items]))))))))
