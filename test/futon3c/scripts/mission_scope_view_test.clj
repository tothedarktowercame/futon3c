(ns futon3c.scripts.mission-scope-view-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.scripts.mission-scope-view :as view]))

(deftest projection-filters-and-normalizes-scope-rows
  (let [hyperedges [{:hx/id "hx|mission-scope|a/identify"
                     :hx/type :mission-scope/eightfold-phase
                     :hx/props {:mission "M-a"
                                :scope/id "a/identify"
                                :scope/name "IDENTIFY"
                                :scope/binder-type "eightfold-phase"
                                :scope/parent "a/root"
                                :scope/parent-state :linked
                                :anchor/state :anchored
                                :anchor/resolve-by :verbatim-search
                                :anchor/passage "## IDENTIFY\nbody"}}
                    {:hx/id "hx|mission-scope|b/identify"
                     :hx/type :mission-scope/eightfold-phase
                     :hx/props {:mission "M-b"
                                :scope/id "b/identify"
                                :scope/binder-type "eightfold-phase"}}]
        projected (view/project-hyperedges "M-a" hyperedges)
        row (first (:scopes projected))]
    (is (= 1 (:scope_count projected)))
    (is (= [{:type "eightfold-phase" :count 1}] (:type_counts projected)))
    (is (= "a/identify" (:id row)))
    (is (= "linked" (:parent_state row)))
    (is (= "anchored" (:anchor_state row)))
    (is (= "verbatim-search" (:anchor_resolve_by row)))
    (is (= "## IDENTIFY" (:passage row)))))

(deftest fetch-view-filters-at-the-server-and-follows-cursors
  (let [urls (atom [])]
    (with-redefs [view/structural-binders ["eightfold-phase"]]
      (with-redefs-fn
        {#'futon3c.scripts.mission-scope-view/http-edn
         (fn [_ url]
           (swap! urls conj url)
           (if (= 1 (count @urls))
             {:hyperedges [{:hx/props {:mission "M-a" :scope/id "one"}}]
              :next-cursor "hx|one"}
             {:hyperedges [{:hx/props {:mission "M-a" :scope/id "two"}}]}))}
        (fn []
          (let [result (view/fetch-view "M-a" "http://substrate/")]
            (is (= 2 (:scope_count result)))
            (is (= "http://substrate/" (:base_url result)))
            (is (= ["http://substrate/api/alpha/hyperedges?type=mission-scope%2Feightfold-phase&mission=M-a&limit=1000&include-total=false"
                    "http://substrate/api/alpha/hyperedges?type=mission-scope%2Feightfold-phase&mission=M-a&limit=1000&include-total=false&after=hx%7Cone"]
                   @urls))))))))

(deftest fetch-view-rejects-incomplete-pagination
  (with-redefs [view/structural-binders ["eightfold-phase"]]
    (doseq [[response message]
            [[{:hyperedges [] :next-cursor "same"} #"repeated a cursor"]
             [{:hyperedges (vec (repeat 1000 {}))} #"full without a cursor"]]]
      (with-redefs-fn
        {#'futon3c.scripts.mission-scope-view/http-edn (fn [& _] response)}
        #(is (thrown-with-msg? clojure.lang.ExceptionInfo message
                              (view/fetch-view "M-a" "http://substrate")))))))
