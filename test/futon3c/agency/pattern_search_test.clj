(ns futon3c.agency.pattern-search-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.pattern-search :as search])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def script-source
  (str "import json,sys,time\n"
       "mode=sys.argv[1]\n"
       "print(json.dumps({'resident':'ready'}),flush=True)\n"
       "for line in sys.stdin:\n"
       " r=json.loads(line)\n"
       " if mode=='hang': time.sleep(10)\n"
       " elif mode=='malformed': print('not-json',flush=True)\n"
       " else: print(json.dumps([{'id':r['query'],'title':'ok','score':1.0,'rank':1}]),flush=True)\n"))

(defn temp-script []
  (let [root (Files/createTempDirectory "pattern-search-test-"
                                        (make-array FileAttribute 0))
        path (.resolve root "fake.py")]
    (spit (.toFile path) script-source)
    {:root root :path (str path)}))

(defn delete-tree! [root]
  (doseq [file (reverse (file-seq (.toFile root)))]
    (io/delete-file file true)))

(use-fixtures :each
  (fn [f]
    (search/reset-state!)
    (try (f) (finally (search/reset-state!)))))

(deftest resident-answers-and-restarts-after-death
  (let [{:keys [root path]} (temp-script)]
    (try
      (binding [search/*resident-command* ["python3" "-u" path "normal"]
                search/*fallback-search* (fn [& _] (throw (ex-info "unexpected" {})))]
        (is (= "first" (:id (first (search/search "first" 1)))) (pr-str (search/stats)))
        (search/stop!)
        (is (= "second" (:id (first (search/search "second" 1)))) (pr-str (search/stats)))
        (is (= 1 (:restarts (search/stats))))
        (is (= 2 (:resident-hits (search/stats)))))
      (finally (delete-tree! root)))))

(deftest hung-resident-times-out-then-falls-back
  (let [{:keys [root path]} (temp-script)
        fallback-calls (atom [])]
    (try
      (binding [search/*resident-command* ["python3" "-u" path "hang"]
                search/*query-timeout-ms* 50
                search/*fallback-search*
                (fn [query top]
                  (swap! fallback-calls conj [query top])
                  [{:id "fallback" :score 0.5 :rank 1}])]
        (is (= "fallback" (:id (first (search/search "slow" 3)))))
        (is (= [["slow" 3]] @fallback-calls))
        (is (= 1 (:fallbacks (search/stats)))))
      (finally (delete-tree! root)))))

(deftest malformed-resident-output-falls-back
  (let [{:keys [root path]} (temp-script)]
    (try
      (binding [search/*resident-command* ["python3" "-u" path "malformed"]
                search/*fallback-search*
                (fn [& _] [{:id "fallback" :score 0.5 :rank 1}])]
        (is (= "fallback" (:id (first (search/search "bad" 1)))))
        (is (= 1 (:fallbacks (search/stats)))))
      (finally (delete-tree! root)))))
