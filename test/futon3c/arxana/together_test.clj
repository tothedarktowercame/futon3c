(ns futon3c.arxana.together-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.together :as t]))

(defn fixtures []
  (json/read-str (slurp (io/resource "arxana/together/conformance.json")) :key-fn keyword))

(defn fixture-results [case]
  (let [ops (mapv #(update % :op keyword) (:ops case))]
    (for [order (or (:orders case) [(vec (range (count ops)))])]
      (reduce t/apply-op t/empty-doc (map ops order)))))

(deftest shared-conformance
  (doseq [case (fixtures)]
    (testing (:name case)
      (let [docs (fixture-results case)]
        (is (apply = docs))
        (doseq [doc docs]
          (is (= (:text case) (t/text doc)))
          (is (= (:ids case) (t/ids doc)))
          (is (every? #(= {:author (first (:id %))} (:attrs %)) doc)))))))

(defn insert-op [editor counter after text]
  {:op :insert :id [editor counter] :after after :text text :attrs {:author editor}})

(def base-op (insert-op "joe" 1 nil "a😀𝕏"))
(def base-hub (:hub (t/submit {:doc [] :log []} base-op "joe" 0 "2026-11-20T00:00:00Z")))

(deftest every-refusal-and-unchanged-hub
  (let [good (insert-op "joe" 4 ["joe" 3] "x")
        cases [[:wrong-author good "other" 1]
               [:id-in-use (insert-op "joe" 1 nil "x") "joe" 1]
               ;; Collision in a later character, not only the initial id.
               [:id-in-use (insert-op "joe" 1 nil "xy") "joe" 1]
               [:unknown-after (assoc good :after ["missing" 1]) "joe" 1]
               [:unknown-id {:op :delete :ids [["missing" 1]]} "joe" 1]
               [:empty-text (assoc good :text "") "joe" 1]
               [:invalid-counter (assoc good :id ["joe" 0]) "joe" 1]
               [:invalid-counter (assoc good :id ["joe" -1]) "joe" 1]
               [:invalid-counter (assoc good :id ["joe" 1.5]) "joe" 1]
               [:counter-not-after (insert-op "bob" 3 ["joe" 3] "x") "bob" 1]
               [:invalid-id (assoc good :id ["" 4]) "joe" 1]
               [:invalid-id (assoc good :id 7) "joe" 1]
               [:invalid-text (assoc good :text nil) "joe" 1]
               [:invalid-attrs (assoc good :attrs nil) "joe" 1]
               [:invalid-ids {:op :delete :ids nil} "joe" 1]
               [:invalid-id {:op :delete :ids [["joe" 0]]} "joe" 1]
               [:invalid-op {:op :replace} "joe" 1]
               [:invalid-op nil "joe" 1]
               [:invalid-parents good "joe" 2]
               [:invalid-parents good "joe" -1]]]
    (doseq [[error op identity parent] cases]
      (testing (str error " " op)
        (let [before base-hub
              result (t/submit base-hub op identity parent "fixture-time")]
          (is (= {:error error} result))
          (is (= before base-hub))
          (is (= before (get result :hub before)))
          (when-not (= :invalid-parents error)
            (is (= error (t/check-op (:doc base-hub) op identity)))))))
    (doseq [op [nil {:op :bad} (assoc good :id 7) (assoc good :text "")]]
      (is (keyword? (try (t/apply-op [] op) nil
                        (catch clojure.lang.ExceptionInfo e (:together/error (ex-data e)))))))))

(deftest later-id-collision-and-tombstones
  (let [doc (t/apply-op [] (insert-op "joe" 2 nil "x"))]
    (is (= :id-in-use (t/check-op doc (insert-op "joe" 1 nil "xy") "joe")))
    (let [deleted (t/apply-op doc {:op :delete :ids [["joe" 2]]})]
      (is (= :id-in-use (t/check-op deleted (insert-op "joe" 2 nil "x") "joe")))
      (is (= deleted (t/apply-op deleted {:op :delete :ids [["joe" 2]]})))
      (is (= "" (t/text deleted)))
      (is (= [] (t/ids deleted)))
      (is (= 3 (t/next-counter deleted))))))

(deftest rebuild-mixed-hub-log
  (let [operations [[base-op "joe" 0]
                    [base-op "joe" 1]
                    [(insert-op "rob" 4 ["joe" 2] "Z") "rob" 0]
                    [{:op :delete :ids [["joe" 1]]} "murch" 2]
                    [{:op :delete :ids [["no" 1]]} "murch" 3]
                    [(insert-op "joe" 5 ["joe" 1] "!") "joe" 3]
                    [{:op :delete :ids [["joe" 1]]} "rob" 4]]
        hub (reduce (fn [hub [op editor parent]]
                      (let [result (t/submit hub op editor parent "2026-11-20T00:00:00Z")]
                        (if (:error result) hub
                          (do (is (= (:entry result) (last (get-in result [:hub :log]))))
                              (:hub result)))))
                    {:doc [] :log []} operations)]
    (is (= 5 (count (:log hub))))
    (is (= [1 2 3 4 5] (mapv :rev (:log hub))))
    (is (= (:doc hub) (t/rebuild (reverse (:log hub))))))
  (is (= 1 (t/next-counter [])))
  (is (= 4 (t/next-counter (:doc base-hub))))
  (is (= [97 128512 120143] (mapv :cp (:doc base-hub)))))

(defn choose [^java.util.Random rng xs]
  (nth (vec xs) (.nextInt rng (count xs))))

(defn available [events seen]
  (keep-indexed (fn [i event]
                  (when (and (not (contains? seen i)) (set/subset? (:deps event) seen)) i)) events))

(defn deliver-event [state editor event-id]
  (-> state
      (update-in [:docs editor] t/apply-op (get-in state [:events event-id :op]))
      (update-in [:seen editor] conj event-id)))

(defn generate-op [^java.util.Random rng doc editor]
  (if (and (seq doc) (zero? (.nextInt rng 4)))
    {:op :delete :ids [(:id (choose rng doc))]}
    (insert-op editor (t/next-counter doc)
               (choose rng (conj (mapv :id doc) nil))
               (choose rng ["x" "ab" "😀" "𝕏z" "pqr"]))))

(defn trial [^java.util.Random rng]
  ;; Each event depends on EVERYTHING its author has received, plus its own
  ;; preceding edits. Delivery queues differ; no operation is generated from
  ;; a global document. Each editor creates at least its initial local op.
  (let [state
        (reduce
         (fn [state step]
           (let [editor (if (< step 3) step (.nextInt rng 3))
                 ready (vec (available (:events state) (get-in state [:seen editor])))]
             (if (and (>= step 3) (seq ready) (zero? (.nextInt rng 3)))
               (deliver-event state editor (choose rng ready))
               (let [op (generate-op rng (get-in state [:docs editor]) (str "editor-" editor))
                     event {:op op :deps (get-in state [:seen editor])}
                     i (count (:events state))]
                 (deliver-event (update state :events conj event) editor i)))))
         {:events [] :docs [[] [] []] :seen [#{} #{} #{}]} (range 30))]
    (reduce (fn [state editor]
              (loop [s state]
                (let [ready (vec (available (:events s) (get-in s [:seen editor])))]
                  (if (seq ready) (recur (deliver-event s editor (choose rng ready))) s))))
            state (range 3))))

(deftest randomized-causal-convergence
  (let [rng (java.util.Random. 20261120)]
    (dotimes [i 200]
      (let [{:keys [docs seen events] :as result} (trial rng)]
        (is (every? #(= (count events) (count %)) seen) (str "Undelivered events, trial " i))
        (is (apply = docs) (str "Divergence, trial " i ": " (pr-str result)))))))

(deftest wrong-comparator-is-caught
  (let [caught (with-redefs [t/compare-ids (fn [[a n] [b m]] (compare [a n] [b m]))]
                 (vec (for [case (fixtures)
                            doc (fixture-results case)
                            :when (or (not= (:text case) (t/text doc))
                                      (not= (:ids case) (t/ids doc)))]
                        (:name case))))]
    (is (seq caught))
    (is (some #{"inside-earlier-run"} caught))))

(deftest runs-round-trip
  (let [doc (-> []
                (t/apply-op (insert-op "joe" 1 nil "a😀𝕏bc"))
                (t/apply-op (insert-op "rob" 6 ["joe" 3] "!?"))
                (t/apply-op {:op :delete :ids [["joe" 2]]}))
        rs (t/runs doc)]
    (is (= ["a" "😀" "𝕏" "!?" "bc"] (mapv :text rs)))
    (is (= [false true false false false] (mapv :deleted rs)))
    (is (= doc (t/from-runs rs)))
    (is (= rs (t/runs (t/from-runs rs))))
    (is (= [] (t/from-runs [])))
    (is (= [] (t/runs [])))
    (is (= :id-in-use (try (t/from-runs (conj rs (first rs)))
                          (catch clojure.lang.ExceptionInfo e (:together/error (ex-data e)))))))
  (let [doc (-> [] (t/apply-op (insert-op "joe" 1 nil "a"))
                (t/apply-op (assoc (insert-op "joe" 2 ["joe" 1] "b") :attrs {:style "different"})))]
    (is (= 2 (count (t/runs doc))))))
