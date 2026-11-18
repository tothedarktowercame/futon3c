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

(deftest indirect-use-is-unknown-not-not-yet
  ;; claude-cs review of CL-76: a namespace that uses Arxana only through an
  ;; Arxana namespace's functions has no literal site, but it is not a
  ;; visible "no use" either.
  (let [index [{:hx/id "namespace:writer" :prop/source-file "/invented/writer.clj" :prop/repo "futon3c"}
               {:hx/id "namespace:indirect" :prop/source-file "/invented/indirect.clj" :prop/repo "futon3c"}
               {:hx/id "namespace:bare" :prop/source-file "/invented/bare.clj" :prop/repo "futon3c"}
               {:hx/id "namespace:plain" :prop/source-file "/invented/plain.clj" :prop/repo "futon3c"}]]
    (with-redefs [slurp (fn [file]
                          (case file
                            "/invented/writer.clj" "(ns invented.writer (:require [futon3c.arxana.records :as r]))\n(r/make-clusion {:type-revision [:ma/strand 1]})"
                            "/invented/indirect.clj" "(ns invented.indirect (:require [clojure.string :as str] [futon3c.arxana.invented-helper :as h]))\n(defn go [x] (h/append! x))"
                            "/invented/bare.clj" "(ns invented.bare (:require futon3c.arxana.invented-helper))\n(defn go [x] (futon3c.arxana.invented-helper/append! x))"
                            "/invented/plain.clj" "(ns invented.plain (:require [clojure.string :as str]))\n(defn go [x] (str/trim x))"))]
      (let [r (nt/built-on? index ["/invented/indirect.clj"] :arxana)]
        (is (= :unknown (:answer r)))
        (is (= :indirect-use (:reason r)))
        (is (= ["futon3c.arxana.invented-helper"] (mapv :requires (:indirect r)))))
      (is (= :unknown (:answer (nt/built-on? index ["/invented/bare.clj"] :arxana))) "a bare lib symbol in :require counts too")
      (is (= :not-yet (:answer (nt/built-on? index ["/invented/plain.clj"] :arxana))) "no Arxana require: still :not-yet")
      (let [r (nt/built-on? index ["/invented/writer.clj"] :arxana)]
        (is (= :yes (:answer r)) "a literal site still answers :yes")
        (is (nil? (:reason r)))))))

(deftest repeated-built-on-reads-only-changed-files
  ;; Finding 2 of the CL-76 review: a second call with unchanged files reads
  ;; none of them; a changed file is read again.
  (let [dir (doto (java.io.File/createTempFile "nt-cache" "") (.delete) (.mkdirs))
        writer (java.io.File. dir "writer.clj")
        subject (java.io.File. dir "subject.clj")
        _ (spit writer "(ns invented.cwriter (:require [futon3c.arxana.records :as r]))\n(r/make-clusion {:type-revision [:ma/strand 1]})")
        _ (spit subject "(ns invented.csubject)")
        index [{:hx/id "namespace:cwriter" :prop/source-file (.getPath writer) :prop/repo "futon3c"}
               {:hx/id "namespace:csubject" :prop/source-file (.getPath subject) :prop/repo "futon3c"}]
        reads (atom [])
        real-slurp slurp]
    (try
      (with-redefs [slurp (fn [f & opts] (swap! reads conj (str f)) (apply real-slurp f opts))]
        (is (= :not-yet (:answer (nt/built-on? index [(.getPath subject)] :arxana))))
        (reset! reads [])
        (is (= :not-yet (:answer (nt/built-on? index [(.getPath subject)] :arxana))))
        (is (empty? @reads) "unchanged files are not read again")
        (Thread/sleep 1100)
        (spit subject "(ns invented.csubject (:require [futon3c.arxana.records :as r]))\n(r/make-clusion {:type-revision [:ma/strand 1]})")
        (reset! reads [])
        (is (= :yes (:answer (nt/built-on? index [(.getPath subject)] :arxana))))
        (is (= [(.getPath subject)] @reads) "only the changed file is read"))
      (finally (.delete writer) (.delete subject) (.delete dir)))))
