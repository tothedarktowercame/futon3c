(require '[futon2.aif.mission-registry :as m] '[futon2.aif.action-proposer :as ap])
(let [mission (first (filter #(= "M-u88-contextual-preferences" (:id %)) (m/open-missions)))
      pin {:type :advance-mission :target "M-u88-contextual-preferences"}
      actions (vec (ap/propose m/mission-enumerator-proposer {:missions [mission]}))]
 (assert mission)
 (assert (= 1 (count actions)))
 (assert (not= pin (first actions)))
 (assert (= pin (select-keys (first actions) [:type :target])))
 (prn {:mission-present true :pin pin :actual-proposed-action (first actions)
       :full-map-equality false :same-type-and-target true}))
