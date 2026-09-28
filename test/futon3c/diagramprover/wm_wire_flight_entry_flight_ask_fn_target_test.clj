(ns futon3c.diagramprover.wm-wire-flight-entry-flight-ask-fn-target-test
  "Scoped target handoff. Negative controls change the flight before the real reader.

  Converted to read the wire ends and the second-layer products from the
  content-addressed target-observe-g49 producer record; this reader loads
  no product code."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def producer (delay (producer-record/record "target-observe-g49")))

(def ask-pin
  {:path "holes/labs/M-wm-wiring/spike/flight-d00574c8.edn"
   :sha256 "b26d4c3cc98009b1f7a828cd2355f6b01bb03feaeda8f03bf43a8a94f0292e34"
   :why "Plan placement and completed flight target; ask refusals sometimes retain target. Click summaries omit target; enactments do not dispatch; reading entries omit issued target."
   :writer-path [:plan :placement :target]
   :reader-path [:flight :asks 0 :asked 0 :reasons 0 :refusal :target]})

(defn check [] (:live-check @producer))

(def wire {:wire [:flight-entry :flight-ask-fn [:target {:record :flight}]]
           :kind :verified
           :test `the-target-reaches-the-reader :second-layer {:test `target-determines-reader-product
                          :kind :value-varying :product [:outputs]
                          :intervention :before-reader}
           :check check
           :record ask-pin})

(deftest the-target-reaches-the-reader
  (is (w/received? (check)))
  (is (w/received? (:observe @producer))))

(deftest typed-absence-before-reader-fails
  (is (not (w/received? (:observe-absent @producer)))))

(deftest different-target-before-reader-fails
  (let [o (:observe-different @producer)]
    (is (= "M-other-target" (:reader o)) (pr-str o))
    (is (not (w/received? o)))))

(deftest target-determines-reader-product
  (let [r (:products-ask @producer)
        [a b] (:outputs r)]
    (is (= (mapv #(dissoc % :target) (:flights r))
           (repeat 2 (dissoc (first (:flights r)) :target))))
    (is (= {:asked [] :needs []} a))
    (is (= [{:want :done :outcome :no-criterion}] (:asked b)))
    (is (= [{:kind :no-criterion :missing :interpretation :want :done}] (:needs b)))
    (println :ask :products [a b])))
