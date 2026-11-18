(ns futon3c.arxana.namespace-types-test
  (:require [clojure.test :refer [deftest is]]
            [org.httpkit.client :as http]
            [futon3c.arxana.namespace-types :as nt]))

(deftest namespace-type-coverage
  (let [source "(ns invented.strand (:require [futon3c.arxana.records :as r] [futon3c.arxana.store :as s] [futon3c.arxana.store-http :as h]))\n(defn plan [] [(r/make-clusion {:type-revision [:ma/strand 1]}) (r/make-clusion {:type-revision [:ma/quote 1]})])\n(defn publish [store] (h/append-batch! store \"message\" (plan)))\n(s/bounded-query! store {:query/type :role-reverse :edge-type [:ma/strand 1]})\n'(r/make-clusion {:type-revision [:fake/quoted 1]})\n(comment (r/make-clusion {:type-revision [:fake/comment 1]}))"
        scan (nt/extract source)
        index [{:hx/id "namespace:invented" :prop/source-file "/invented/strand.clj" :prop/repo "futon3c"}
               {:hx/id "namespace:empty" :prop/source-file "/invented/empty.clj" :prop/repo "futon3c"}]]
    (is (= :ok (:status scan)))
    (is (= #{[:ma/strand 1] [:ma/quote 1]} (set (map :type-revision (filter #(and (= :writes (:direction %)) (:type-revision %)) (:uses scan))))))
    (is (= [[:ma/strand 1]] (mapv :type-revision (filter #(= :reads (:direction %)) (:uses scan)))))
    (is (= [2 2 4] (mapv :line (filter :type-revision (:uses scan)))))
    (is (= [:made/direct 1] (-> (nt/extract "(futon3c.arxana.store-http/append-batch! store id [{:record {:type-revision [:made/direct 1]}}])") :uses first :type-revision)))
    (is (= [:made/referred 1] (-> (nt/extract "(ns example (:require [futon3c.arxana.records :refer [make-clusion]])) (make-clusion {:type-revision [:made/referred 1]})") :uses first :type-revision)))
    (is (= 1 (count (:publication-sites scan))))
    (is (= :dynamic (-> (nt/extract "(futon3c.arxana.records/make-clusion {:type-revision t})") :uses first :status)))
    (with-redefs [slurp (fn [file] (if (= file "/invented/strand.clj") source "(ns invented.empty)"))]
      (is (= :yes (:answer (nt/built-on? index ["/invented/strand.clj"] "ma/"))))
      (let [unknown (nt/built-on? index ["/invented/empty.clj"] "invented-unregistered/")
            unwritten (nt/built-on? [(second index)] ["/invented/empty.clj"] "ma/")]
        (is (= :unknown (:answer unknown)))
        (is (= :unregistered-target (get-in unknown [:target-coverage :reason])))
        (is (= :unknown (:answer unwritten)))
        (is (= :no-indexed-target-writer (get-in unwritten [:target-coverage :reason]))))
      (is (= :unknown (:answer (nt/built-on? index ["/invented/nip_merge.py"] "ma/"))))
      (is (= :not-yet (:answer (nt/built-on? index ["/invented/empty.clj"] "ma/"))))
      (is (= :unknown (:answer (nt/built-on? index ["/invented/strand.clj" "/invented/nip_merge.py"] "ma/"))))
      (let [calls (atom [])]
        (with-redefs [nt/indexed-namespaces (fn [_] index)
                      http/request (fn [req] (swap! calls conj req) (delay {:status 200 :body "{:ok true}"}))]
          (is (= :dry-run (:mode (nt/backfill! {}))))
          (is (empty? @calls))
          (is (= :write (:mode (nt/backfill! {:write? true}))))
          (is (= 1 (count @calls)))
          (is (= :post (:method (first @calls))))))
      (let [a (nt/edges (nt/inspect (first index)))]
        (is (= a (nt/edges (nt/inspect (first index)))))
        (is (= 3 (count a)))
        (is (= ["namespace:invented" "arxana-type:ma/strand@1"] (:hx/endpoints (first a))))))))

(deftest built-on-arxana-means-any-registered-type
  (let [index [{:hx/id "namespace:writer" :prop/source-file "/invented/writer.clj" :prop/repo "futon3c"}
               {:hx/id "namespace:empty" :prop/source-file "/invented/empty.clj" :prop/repo "futon3c"}
               {:hx/id "namespace:fake" :prop/source-file "/invented/fake.clj" :prop/repo "futon3c"}]]
    (with-redefs [slurp (fn [file]
                          (case file
                            "/invented/writer.clj" "(ns invented.writer (:require [futon3c.arxana.records :as r]))\n(r/make-clusion {:type-revision [:ma/strand 1]})"
                            "/invented/fake.clj" "(ns invented.fake (:require [futon3c.arxana.records :as r]))\n(r/make-clusion {:type-revision [:not-arxana/thing 1]})"
                            "(ns invented.empty)"))]
      (is (= :yes (:answer (nt/built-on? index ["/invented/writer.clj"] :arxana))))
      (is (= :not-yet (:answer (nt/built-on? index ["/invented/empty.clj"] :arxana))))
      (is (= :not-yet (:answer (nt/built-on? index ["/invented/fake.clj"] :arxana)))
          "a literal type not in Arxana's registry is not evidence"))))

