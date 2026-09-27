(ns futon3c.diagramprover.wm-wire-construction-assemble-one-r13-family-parameters-beta-test
  (:require [futon2.aif.cascade-problems :as cp]
            [futon2.aif.efe :as efe]
            [futon2.aif.locator-fixtures :as loc]
            [futon2.report.cascade-decision-test :as fixture]
            [futon2.report.war-machine :as wm]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-construction-support :as support]))
(defn observe [mutation] (support/family :beta mutation))
(defn check [] (observe :none))
(def wire {:second-layer
           {:test 'futon3c.diagramprover.wm-wire-construction-assemble-one-r13-family-parameters-beta-test/beta-changes-the-joint-posterior
            :kind :value-varying :product [:posterior] :intervention :before-reader}
           :wire [:construction-assemble-one :r13-family-parameters [:beta {:record :cascade-problem}]]
           :kind :witnessed-hermetically
           :test `the-observed-handoff :check check
           :live-records-read support/live-records-read})
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o))
    (is (= {:beta 1 :horizon-steps 3} (:family o)))))
(deftest absence-before-reader-fails
  (is (not (w/received? (observe :absent)))))
(deftest different-value-before-reader-fails
  (let [o (observe :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))


(defn beta-product [beta]
  (let [interp {:patterns {:p {:guard {:needs #{:open} :forbids #{:done}}
                              :produces #{:done}}}
                :receipts {:p {:receipt "p" :source "fixture"}}}
        assembled (cp/assemble
                    {:targets [:A :B]
                     :sources (loc/locate-all
                                {:universes {:A {:open true :done false}
                                             :B {:open true :done false}}
                                 :interpretations {:A interp :B interp}
                                 :wants {:A [:done] :B [:done]}
                                 :candidates (into {} (for [t [:A :B]]
                                                       [t [{:precedence [:p]
                                                            :construction-receipt fixture/receipt}]]))
                                 :horizon-steps 3 :beta-by-context {:x {:beta 1}}
                                 :context-of (constantly :x)})})
        ;; Change only the assembled carrier, before the real family reader.
        supplied (update assembled :problems
                         #(mapv (fn [p] (assoc-in p [:cascade-problem :beta] beta)) %))
        opts (-> fixture/live-c-opts
                 (update-in [:focus-inputs :relations]
                            #(mapv (fn [r] (if (= :B (:target r))
                                             (assoc r :relation "associated") r)) %))
                 (assoc :live-c {:derived (assoc fixture/live-c-fixture
                                                :want #{[:A :done] [:B :done]}
                                                :weights {[:A :done] 1 [:B :done] 1})}))
        scores (atom nil)
        real-rank efe/rank-actions
        decision (:decision
                   (with-redefs [efe/rank-actions
                                 (fn [state candidates options]
                                   (let [ranked (real-rank state candidates options)]
                                     (reset! scores (into (sorted-map)
                                                         (map (juxt #(get-in % [:action :target]) :controller-score) ranked)))
                                     ranked))]
                     (wm/cascade-decision supplied opts)))
        posterior (get-in decision [:selection-law :posterior])]
    {:scores @scores
     :posterior (into (sorted-map) (map (fn [[c p]] [(:target c) p]) posterior))
     :beta (get-in decision [:selection-law :beta])}))

(deftest beta-changes-the-joint-posterior
  (let [before (beta-product 1) after (beta-product 3)
        p1 (:posterior before) p3 (:posterior after)]
    (prn :wire-2l-3b :before before :after after)
    (is (= [1 3] [(:beta before) (:beta after)]))
    (is (= #{:A :B} (set (keys p1)) (set (keys p3))))
    (is (= (:scores before) (:scores after)))
    (is (< (get-in before [:scores :A]) (get-in before [:scores :B])))
    (is (not= p1 p3))
    ;; beta is temperature: -G/beta, NOT inverse temperature -beta*G.
    (is (< 0.5 (:A p3) (:A p1)))
    (is (< (:B p1) (:B p3) 0.5))))
