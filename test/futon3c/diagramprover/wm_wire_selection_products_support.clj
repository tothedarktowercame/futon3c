(ns futon3c.diagramprover.wm-wire-selection-products-support
  (:require [futon2.aif.efe :as efe]
            [futon2.aif.flight :as flight]
            [futon2.aif.policy :as policy]
            [futon2.aif.cascade-selection :as selection]
            [futon2.aif.order-kernel-test :as order]
            [futon2.aif.policy-prefix-admission :as admission]
            [futon2.aif.policy-prefix-evidence :as prefix]
            [futon2.report.war-machine :as wm]
            [futon3c.diagramprover.wm-wire-rates-support :as rates]))

(def target "selection-wire")
(def ranked
  (delay (let [entries (efe/rank-cascade-actions
                         {:cascade-belief {#{} 1}}
                         (mapv #(assoc % :target target) (filter #(#{:chain :independent} (:id %)) order/candidates))
                         {:horizon-steps 2 :cascade-spec {:want #{:w} :lam 1 :mu 1 :evidence #{} :zeroed #{}}})
               by-id (into {} (map (juxt :cascade-id identity)) entries)]
           [(:chain by-id) (:independent by-id)])))

(defn posterior [entries]
  (get-in (policy/select-action-cascades entries {:beta 1}) [:selection-law :posterior]))

(defn score-pair []
  (let [before @ranked after (update-in before [0 :controller-score] + 2)]
    {:before before :after after :p (posterior before) :p-prime (posterior after)
     :candidate (#'policy/selection-candidate (first before))
     :candidate-prime (#'policy/selection-candidate (first after))}))

(def prefix-family
  (delay
    (let [action (:action (first @ranked))
          interpretations (update-vals order/pat
                            #(assoc % :transition {:status :interpreted :produces (:produces %)}))
          ma (wm/measured-a-version
               [{:target target :cascade-problem {:locators {:w {:class :C3}}}}]
               {target (select-keys @rates/admitted-view [:labels :subjects :prior])})
          record {:decision {:measured-a ma :initial-belief-receipt {:value {#{} 1}}
                             :selection-certificate {:token-belief-stage
                               {:domain-inputs [{:target target :declaration {:interpretations interpretations}}]}}}}
          step (flight/conditioning-step
                 {:run-record record :target target :flight-id "selection-wire" :click-id "1"
                  :policy-key (admission/candidate-key action) :precedence [:A :B] :enactments []
                  :observation {:status :observed :o #{:w} :checked #{:w} :channel {:w :C3}}})
          admitted (admission/admit (admission/candidate-key action) [{:step step}])]
      (assert (= :present (:status step)) (pr-str step))
      (assert (= :admitted (:conditioning-status admitted)))
      {:action action :entries (prefix/production-ranked @ranked nil {(:id action) admitted})})))

(defn prefix-pair []
  (let [{:keys [action entries]} @prefix-family
        idx (first (keep-indexed #(when (= action (:action %2)) %1) entries))
        before entries after (update-in before [idx :f-prefix :f] inc)
        read #(mapv (fn [e] (#'policy/selection-candidate e)) %)
        a (read before) b (read after)
        law #(selection/selection-posterior {:beta 1 :candidates %})]
    {:action action :index idx :before before :after after
     :candidates a :candidates-prime b :p (law a) :p-prime (law b)}))
