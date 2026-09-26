(ns futon3c.apm.solver-shelf-canary-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.solver-shelf-canary :as sut]))

(def entries [{:memory-id "e-one" :hook "h" :body "b"}])
(def shelf {:schema/version 1 :canary/id "solver-shelf-c1"
            :eligible/frame-id "f80" :assignment :shelf :matched/size 1
            :shelf/entries entries :shelf/digest (sut/shelf-digest entries)})

(deftest assignment-is-exact-frame-and-content-addressed
  (is (:ok (sut/validate-assignment shelf "f80")))
  (is (= #{:frame-not-eligible}
         (set (:findings (sut/validate-assignment shelf "f81")))))
  (is (= #{:shelf-digest-mismatch}
         (set (:findings
               (sut/validate-assignment (assoc shelf :shelf/digest "wrong") "f80"))))))

(deftest matched-control-declares-size-with-no-exposure
  (let [control (assoc shelf :assignment :control :matched/size 1
                       :shelf/entries [] :shelf/digest (sut/shelf-digest []))]
    (is (:ok (sut/validate-assignment control "f80")))
    (is (= #{:control-match-size-missing}
           (set (:findings
                 (sut/validate-assignment (assoc control :matched/size 0) "f80")))))))

(deftest observation-cannot-manufacture-exposure
  (is (empty? (sut/observation-findings
               shelf {:surfaced-ids ["e-one"] :used-ids ["e-one"]})))
  (testing "extra self-reported ids fail closed"
    (is (= #{:surfaced-ids-authority-mismatch :used-id-not-surfaced}
           (set (sut/observation-findings
                 shelf {:surfaced-ids ["e-one" "invented"]
                        :used-ids ["invented-too"]}))))))
