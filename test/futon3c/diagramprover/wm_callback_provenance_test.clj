(ns futon3c.diagramprover.wm-callback-provenance-test
  "CB-ELEM: literal callback elements, without interprocedural parameter or capture inference."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wiring :as wiring]))

(defn- with-source [source f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                      "callback-provenance" (make-array java.nio.file.attribute.FileAttribute 0)))
        path (io/file root "source.clj")]
    (try
      (spit path source)
      (f (str root))
      (finally (.delete path) (.delete root)))))

(defn- result [body]
  (with-source (str "(defn judge [record other] " body ")\n(defn consume [x] (count x))")
    #(wiring/pass-attribution
      % [{:box/id :caller :site {:file "source.clj" :var "judge"}}
         {:box/id :callee :site {:file "source.clj" :var "consume"}}]
      :caller {:value :item :from {:element-of {:keyed-read [:items :record]}}
               :to {:call "consume" :arg 1 :callee-box :callee}})))

(deftest literal-callbacks-carry-elements
  (doseq [head ["map" "mapv" "mapcat" "filter" "filterv" "keep" "remove" "run!"]]
    (is (:ok? (result (str "(" head " (fn [x] (consume x)) (:items record))"))) head))
  (doseq [body ["(mapv #(consume %) (:items record))"
               "(mapv #(consume %1) (:items record))"
               "(reduce (fn [acc x] (consume x)) nil (:items record))"
               "(reduce #(conj %1 (consume %2)) [] (:items record))"
               "(doseq [x (:items record)] (consume x))"
               "(mapv (fn [x] (let [unrelated other] (consume x))) (:items record))"]]
    (is (:ok? (result body)) body)))

(deftest only-permuting-or-subsetting-wrappers-are-transparent
  (doseq [coll ["(sort-by :id (:items record))"
               "(sort-by :id compare (:items record))"
               "(sort (:items record))" "(sort compare (:items record))"
               "(reverse (distinct (:items record)))"]]
    (is (:ok? (result (str "(mapv (fn [x] (consume x)) " coll ")"))) coll))
  (is (:ok? (result "(let [xs (sort-by :id (:items record)) ys xs]
                      (mapv (fn [x] (consume x)) ys))")))
  (is (false? (:ok? (result "(mapv (fn [x] (consume x)) (map identity (:items record)))"))))
  (is (false? (:ok? (result "(let [xs (map identity (:items record))]
                             (mapv (fn [x] (consume x)) xs))")))))

(deftest refused-callback-provenance
  (doseq [[body why]
          [["(mapv (fn [x] (consume x)) other)" :callback-collection-unproven]
           ["(mapv (fn [x] (let [x other] (consume x))) (:items record))"
            :shadowed-callback-parameter]
           ["(mapv (fn [x] ((fn [x] (consume x)) other)) (:items record))"
            :callback-not-a-literal]
           ["(mapv (fn [x] (if-let [x other] (consume x))) (:items record))"
            :callback-binding-scope-not-established]
           ["(reduce (fn [acc x] (consume acc)) nil (:items record))"
            :reduce-accumulator-not-an-element]
           ["(reduce #(consume %1) nil (:items record))" :reduce-accumulator-not-an-element]
           ["(mapv #(do (consume %) %2) (:items record))" :callback-parameter-arity]
           ["(map (fn [x] (consume x)))" :callback-collection-arity]
           ["(mapv (fn [x y] (consume x)) (:items record) other)" :callback-collection-arity]
           ["(let [mapv other] (mapv (fn [x] (consume x)) (:items record)))"
            :shadowed-collection-call]
           ["(mapv (fn [x] (consume x)) (sort))" :callback-collection-unproven]
           ["(doseq [x other] (consume x))" :callback-collection-unproven]
           ["(doseq [x (:items record) :let [x other]] (consume x))"
            :unsupported-doseq-binding]]]
    (let [r (result body)]
      (is (false? (:ok? r)) (str body " " r))
      (is (= why (:why r)) (str body " " r))))
  (testing "a named helper requires the separate parameter-provenance rule"
    (with-source "(defn helper [x] (consume x)) (defn judge [record] (mapv helper (:items record)))
                  (defn consume [x] (count x))"
      (fn [root]
        (is (= :callback-not-a-literal
               (:why (wiring/pass-attribution
                      root [{:box/id :caller :site {:file "source.clj" :var "helper"}}
                            {:box/id :callee :site {:file "source.clj" :var "consume"}}]
                      :caller {:value :item :from {:element-of {:keyed-read [:items :record]}}
                               :to {:call "consume" :arg 1 :callee-box :callee}}))))))))

(deftest real-p6-callback-stops-at-the-collection-claim
  ;; Read the exact production forms used by PROVER-CB-D, not a stand-in.
  (let [r (sh/sh "git" "-C" "/home/joe/code/futon2" "show"
                 "b88599906:src/futon2/aif/interpretation_construction.clj")
        c (sh/sh "git" "-C" "/home/joe/code/futon2" "show"
                 "b88599906:src/futon2/aif/construction.clj")]
    (is (zero? (:exit r)) (:err r))
    (is (zero? (:exit c)) (:err c))
    (with-source (str (:out r) "\n" (:out c))
      (fn [root]
        (let [boxes [{:box/id :caller :site {:file "source.clj" :var "construct"}
                      :record-aliases {:construction-result ["result"]}}
                     {:box/id :callee :site {:file "source.clj" :var "containment-order"}}]
              p {:value :candidate
                 :from {:element-of {:keyed-read [:family :construction-result]}}
                 :to {:call "construction/containment-order" :arg 1 :callee-box :callee}}
              good (wiring/pass-attribution root boxes :caller p)
              unproved (wiring/pass-attribution root boxes :caller
                         (assoc p :from {:element-of {:keyed-read [:interpretations :input]}}))]
          (is (:ok? good) (pr-str good))
          (is (false? (:ok? unproved)))
          (is (= :callback-collection-unproven (:why unproved)) (pr-str unproved))
          (is (= :not-the-keyed-read (:collection-why unproved)) (pr-str unproved)))))))
