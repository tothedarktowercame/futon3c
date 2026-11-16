(ns futon3c.arxana.turn-mention-writer-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.item-inventory :as items]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.turn-mention-writer :as writer]))

(def cfg {:graph-store :stub :item-ids ["M-toy"] :pause-ms 0
          :read-evidence (fn [_] {:evidence/body {:text "🈸 M-toy M-outside"}})
          :turn-store {:dir "/invented/turns"}})
(def turn {:evidence_id "toy-evidence" :id "turn-toy" :created_at "2026-01-01" :source_text "🈸 M-toy M-outside"})

(deftest one-field-batch-and-determinism
  (let [calls (atom [])]
    (with-redefs [http/append-batch! (fn [s mid rs] (swap! calls conj [s mid rs]) {:ok {}})]
      (is (= {:mentions 1 :requests 1} (writer/write-turn-mentions! cfg turn :source_text)))
      (let [rows (nth (first @calls) 2)]
        (is (= [:external-reference/assert :clusion/assert :evidence/append] (mapv :operation rows)))
        (is (= [2 7] (get-in rows [1 :record :asserted-content :mention/range])))
        (is (nil? (get-in rows [1 :record :asserted-content :mention/quote])))
        (is (= {:basis :exact :standing :verified :coverage :complete
                :producer "futon3c/turn-mentions" :method {:name :exact-item-token :version 2}}
               (select-keys (get-in rows [2 :record]) [:basis :standing :coverage :producer :method]))))
      (writer/write-turn-mentions! cfg turn :source_text)
      (is (= (first @calls) (second @calls)))
      (reset! calls [])
      (is (= {:mentions 0 :requests 0}
             (writer/write-turn-mentions! (assoc cfg :read-evidence (fn [_] {:evidence/body {:text "M-outside"}})) (assoc turn :source_text "M-outside") :source_text)))
      (is (empty? @calls)))))

(deftest large-field-splits-and-refusal-stops
  (let [calls (atom []) t (assoc turn :source_text (str/join " " (repeat 251 "M-toy")))]
    (with-redefs [http/append-batch! (fn [_ mid rs] (swap! calls conj [mid rs]) {:ok {}})]
      (is (= {:mentions 251 :requests 2} (writer/write-turn-mentions! (assoc cfg :read-evidence (fn [_] {:evidence/body {:text (:source_text t)}})) t :source_text)))
      (is (= [500 3] (mapv #(count (second %)) @calls)))
      (is (= 2 (count (set (map first @calls))))))
    (with-redefs [http/append-batch! (fn [& _] {:error :refused})]
      (is (thrown? clojure.lang.ExceptionInfo (writer/write-turn-mentions! (assoc cfg :read-evidence (fn [_] {:evidence/body {:text (:source_text t)}})) t :source_text))))))

(deftest dry-run-never-writes
  (with-redefs [http/append-batch! (fn [& _] (throw (AssertionError. "dry run wrote")))]
    (is (= {:turns 1 :fields 2 :mentions 2 :distinct-items 1 :planned-batch-requests 2}
           (writer/backfill! cfg [(assoc turn :reply_text "M-toy")])))))

(deftest inventory-requires-store-membership
  (let [c (records/make-clusion {:type-revision [:workplan/item 1] :endpoints []
                                 :asserted-content {:item/id "M-toy"}})]
    (with-redefs [items/current-items (fn [_] [{:id "M-toy"} {:id "M-absent"}])
                  store/get-entity (fn [_ id] (if (= id (:id c)) {:ok c}
                                                  {:error {:error/category :missing}}))]
      (is (= ["M-toy"] (writer/inventory {:graph-store :stub} "M-toy M-absent"))))))

(deftest evidence-authority-quote-roles-and-unavailable
  (let [text "🈸 猴\n>>> M-toy\n>>>\nM-toy"
        c (assoc cfg :read-evidence (fn [_] {:evidence/body {:text text}}))
        plan (writer/plan-field c (assoc turn :source_text "QUOTE M-toy") :source_text)
        ref (:reference plan)]
    (is (= "futon1b/evidence" (:store/id ref)))
    (is (= "toy-evidence/body/text" (:record/id ref)))
    (is (= text (:text plan)))
    (is (= [:quoted :mentioned] (mapv #(get-in % [:asserted-content :mention/role]) (:mentions plan))))
    (is (= [[8 13] [18 23]] (mapv #(get-in % [:asserted-content :mention/range]) (:mentions plan))))
    (with-redefs [http/append-batch! (fn [& _] (throw (AssertionError. "unavailable evidence wrote")))]
      (is (= {:mentions 0 :requests 0}
             (writer/write-turn-mentions! (assoc cfg :read-evidence (fn [_] (throw (Exception. "missing")))) turn :source_text))))))

(deftest reply-keeps-its-exact-turn-field-reference
  (let [p (writer/plan-field cfg (assoc turn :reply_text "M-toy") :reply_text)]
    (is (= "futon3c/turns" (get-in p [:reference :store/id])))
    (is (= "turn-toy/reply_text" (get-in p [:reference :record/id])))
    (is (= "M-toy" (:text p)))))
