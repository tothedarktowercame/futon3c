(ns futon3c.diagramprover.wm-wire-r9-decision-flight-record-click-kind-test
  (:require [futon3c.diagramprover.wm-wire-abstention-kind-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-measured-support :as support]))
(defn check [] (support/kind-observe :none))
(def wire {:wire [:r9-decision :flight-record-click :kind]
           :second-layer {:test `abstention-kind-is-recorded-not-an-ask-status
                          :kind :record :product [:clicks 0 :abstention :kind]
                          :intervention :before-reader}
           :kind :witnessed-hermetically :test `the-observed-handoff :check check
           :live-records-read (conj support/live-records-read support/two-record-live-pair)
           :note support/two-record-live-pair})
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o) (pr-str o))))
(deftest absence-before-reader-fails
  (let [o (support/kind-observe :absent)]
    (is (not (w/received? o)))))
(deftest different-value-before-reader-fails
  (let [o (support/kind-observe :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
(deftest two-live-records-agree-without-becoming-verified
  (let [o (support/live-kind-pair)]
    (is (= :universe-not-admitted (:writer o)))
    (is (w/received? o))))

(deftest abstention-kind-is-recorded-not-an-ask-status
  ;; record-click:425,439 stores the kind. run!:587 consults asked :needs,
  ;; not the abstention kind, when deciding :awaiting-answer.
  (let [[a b] (products/pair)
        clean (fn [f] (-> f
                          (update :clicks #(mapv (fn [c] (update c :abstention dissoc :kind)) %))
                          (update :needs #(mapv (fn [n] (dissoc n :kind)) %))))]
    (is (= :live-c-stale (get-in a [:carrier :abstention :kind])))
    (is (= :pending (get-in b [:carrier :abstention :kind])))
    (is (= (update (:carrier a) :abstention dissoc :kind)
           (update (:carrier b) :abstention dissoc :kind)))
    (doseq [path [:direct :loop]]
      (let [ra (get-in a [:readback path]) rb (get-in b [:readback path])]
        (is (= [:live-c-stale :pending]
               (mapv #(get-in % [:clicks 0 :abstention :kind]) [ra rb])))
        (is (= [:live-c-stale :pending] (mapv #(get-in % [:needs 0 :kind]) [ra rb])))
        (is (= :no-progress (:status ra) (:status rb)))
        (is (= 1 (count (:clicks ra)) (count (:clicks rb))))
        (is (= (clean ra) (clean rb)))
        (println :abstention-kind path [:live-c-stale :pending]
                 :status [(:status ra) (:status rb)])))))
