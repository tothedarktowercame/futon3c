(ns futon3c.circus.navigability-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.circus.navigability :as nav]
            [futon3c.transport.http :as http]))

(deftest lean-missing-edge-tooth-has-closed-witness
  (let [s [{:from :turn :to :mission} {:from :mission :to :pattern}]
        u #{[:mission :pattern]}
        [gap] (nav/analyse [(first s)] u)]
    (is (= :gap (:status gap)))
    (is (= #{:turn} (:witness gap)))
    (is (true? (nav/check-witness! u :turn :mission (:witness gap))))))

(deftest hub-reaches-everything-in-at-most-two
  (let [ks #{:turn :mission :pattern :home}
        u (set (concat (for [k ks] [k :home]) (for [k ks] [:home k])))
        s (for [a ks, b ks] {:from a :to b})
        results (nav/analyse s u)]
    (is (every? #(= :reachable (:status %)) results))
    (is (every? #(<= (:distance %) 2) results))))

(deftest malformed-closure-witness-is-refused
  (testing "a claimed closure omitting a UI successor violates closed_blocks"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"invalid navigability closure witness"
         (nav/check-witness! #{[:turn :pattern]} :turn :mission #{:turn}))))
  (testing "the witness must contain the source and exclude the target"
    (is (thrown? clojure.lang.ExceptionInfo
                 (nav/check-witness! #{} :turn :mission #{:pattern})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (nav/check-witness! #{} :turn :mission #{:turn :mission})))))

(deftest navigability-get-is-wired
  (with-redefs [nav/response (constantly {:status 200 :body {:summary {:edges 2}}})]
    (let [response (http/extra-routes {:request-method :get
                                       :uri "/api/alpha/circus/navigability"}
                                      {})]
      (is (= 200 (:status response)))
      (is (re-find #"\"edges\":2" (:body response))))))
