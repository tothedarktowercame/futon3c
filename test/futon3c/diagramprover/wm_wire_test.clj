(ns futon3c.diagramprover.wm-wire-test
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.target-field :as tf]
            [futon3c.diagramprover.wm-wire :as w]))

(deftest real-overlap-string-keyed-sorted-map-is-a-present-value
  (let [entries (tf/with-pair-overlap
                 [{:target "M-a" :universe #{:shared}
                   :constructed-candidate {:produces #{:shared}}}
                  {:target "M-b" :universe #{:shared}
                   :constructed-candidate {:produces #{:shared}}}])
        overlap (:pair-overlap (first entries))]
    (is (sorted? overlap))
    (is (= {"M-b" {:incommensurable {:shared-tokens [:shared]}}} overlap))
    (is (false? (w/typed-absence? overlap)))
    (is (w/received? {:writer overlap :reader overlap}))
    (is (not (w/received? {:writer overlap :reader {:absent :not-supplied}})))
    (is (not (w/received? {:writer overlap :reader (sorted-map "M-b" {:comparable true})})))))

(deftest absence-markers-keep-their-meaning
  (doseq [v [{:absent :missing} {:absent nil} {:status :absent}
             (sorted-map :absent :missing) (sorted-map :status :absent)]]
    (is (true? (w/typed-absence? v)))
    (is (not (w/received? {:writer v :reader v}))))
  (doseq [v [nil :absent {} {:status :present}
             (sorted-map "status" "absent") (sorted-map "absent" "missing")]]
    (is (false? (w/typed-absence? v)))))
