(ns futon3c.diagramprover.wm-cond-assoc-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wiring :as w]))

(defn proof [body & [cfg]]
  (w/conditional-return-attributions (str "(defn f [m p q k] " body ")") "f" :x
                                    (merge {:returns #{:r}} cfg)))

(deftest conditional-and-nested-keyed-writes
  (doseq [body ["(cond-> {:base 1} p (assoc :x 2))"
                "(cond-> {:base 1} p (assoc-in [:x] 2))"
                "(cond-> {:base 1} p (merge {:x 2}))"]]
    (is (= [{:conditional true :record :r :field :x}]
           (mapv #(dissoc % :key-start) (proof body)))))
  (is (= false (:conditional (first (proof "(assoc {:base 1} :x 2)")))))
  (is (= :nested (:record (first (proof "{:receipt (assoc receipt :x 2)}"
                                         {:returns #{} :return-paths {:nested [:receipt]}
                                          :aliases {:nested #{"receipt"}}}))))))

(deftest unsupported-bases-paths-removals-and-keys-refuse
  (doseq [body ["(cond-> m p (assoc :x 2))"
                "(cond-> (assoc m :x 2) p (dissoc :x))"
                "(cond-> {:base 1} p (assoc :x 2) q (dissoc :x))"
                "(-> (cond-> {:base 1} p (assoc :x 2)) (dissoc :x))"
                "(cond-> {:base 1} p (assoc k 2))"
                "(cond-> {:base 1} p (assoc-in [k] 2))"
                "(cond-> {:base 1} p (assoc-in [:other :x] 2))"]]
    (is (empty? (proof body)) body))
  (is (empty? (proof "{:other (assoc receipt :x 2)}"
                     {:returns #{} :return-paths {:nested [:receipt]}
                      :aliases {:nested #{"receipt"}}})))
  (is (empty? (proof "(let [receipt (unknown)] {:receipt (cond-> receipt p (assoc :x 2))})"
                     {:returns #{} :return-paths {:nested [:receipt]}
                      :aliases {:nested #{"receipt"}}}))))

(deftest real-temporal-forms
  (let [source (slurp (io/file ".." "futon2" "src/futon2/aif/temporal_update.clj"))
        predecessor (slurp (io/file ".." "futon2" "src/futon2/aif/token_belief_predecessor.clj"))]
    (doseq [field [:temporal-posterior :temporal-cursor]]
      (let [p (w/conditional-return-attributions source "finalize-record" field {:returns #{:enactment}})]
        (is (= 1 (count p)))
        (is (true? (:conditional (first p))))))
    (let [p (w/conditional-return-attributions predecessor "inspect-trace" :temporal-previous
                                             {:returns #{:temporal-inspection}})]
      (is (= 1 (count p)) "Only the final re-add survives the intervening removal")
      (is (true? (:conditional (first p)))))
    (doseq [field [:record-path :digest]]
      (is (= 1 (count (w/conditional-return-attributions
                       source "write-once!" field
                       {:return-paths {:temporal-publication [:receipt]}
                        :aliases {:temporal-publication #{"receipt"}}})))))))

(deftest attributed-by-the-real-conformance-check
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "cond-assoc" (make-array java.nio.file.attribute.FileAttribute 0)))
        file (io/file root "a.clj")
        spec {:boxes [{:box/id :a :box/kind :component :site {:file "a.clj" :var "f"}
                       :returns-record :r :writes [[:x {:record :r}]]}]}]
    (try
      (spit file "(defn f [p] (cond-> {:base 1} p (assoc :x 2)))")
      (is (not-any? #(= [:x :r] (:field %)) (w/conformance (str root) spec {:heuristic? true})))
      (spit file "(defn f [p] (cond-> {:base 1} p (assoc :x 2) p (dissoc :x)))")
      (is (some #(= :declaration-without-occurrence (:finding %))
                (w/conformance (str root) spec {:heuristic? true})))
      (spit file "(defn f [m p] (cond-> (assoc m :x 2) p (dissoc :x)))")
      (is (some #(= :declaration-without-occurrence (:finding %))
                (w/conformance (str root) spec {:heuristic? true})))
      (finally (io/delete-file file) (io/delete-file root)))))
