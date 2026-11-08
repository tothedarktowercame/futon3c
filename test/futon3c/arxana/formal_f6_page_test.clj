(ns futon3c.arxana.formal-f6-page-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.formal-f6-page :as page]))
(defn item [id kind zone order]
  {:record/id id :record {:id id :asserted-content {:item/id (keyword id) :item/kind kind
                                                     :content {:text id}}}
   :placement {:asserted-content {:zone zone :order order}}})
(defn support [id supporting claim current? actor reason]
  {:record/id id :current? current?
   :record {:id id :endpoints [{:role :supporting-content :target {:clusion/id supporting}}
                               {:role :supported-claim :target {:clusion/id claim}}]
            :asserted-content {:actor actor :reason reason}}})
(deftest deterministic-columns-and-support-history
  (let [diagram (item "diagram" :diagram :margin 9) claim (item "claim" :claim :main 1)
        old (support "support-old" "diagram" "old-claim" false "joe" "changed proof target")
        current (support "support-new" "diagram" "claim" true "joe" "cube is current")
        view {:argument/id "argument" :items [diagram claim] :supports [old current]}
        a (page/project view) b (page/project (assoc view :items [claim diagram]))]
    (is (= (:html a) (:html b)))
    (is (re-find #"<aside id=margin.*data-record-id='diagram'" (:html a)))
    (is (re-find #"supports → claim" (:html a)))
    (is (re-find #"formerly supported old-claim, changed by joe: changed proof target" (:html a)))
    (is (not (re-find #"supports → old-claim" (:html a))))
    (doseq [id ["diagram" "claim" "support-old" "support-new"]]
      (is (re-find (re-pattern (str "data-record-id='" id "'")) (:html a))))))
