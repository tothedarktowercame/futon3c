(ns futon3c.arxana.formal-f3-correspond-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f3 :as f3]
            [futon3c.arxana.formal-f3-correspond :as f3b]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(def alignment-path
  "/home/joe/code/storage/apm/mining/alignment/r2-judged/a93J02.judged.json")

(defn with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f3b-" ".db") _ (.delete db)]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server!
                      {:node node :port 0 :bind-host "127.0.0.1"
                       :arxana-index-path (.getPath db)})
              cfg {:graph-store
                   (store-http/http-store
                    (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                    f1/service-principal)
                   :artifact-provider (artifact/in-memory-provider)}]
          (try (f cfg) (finally (f1-server/stop-server! server)))))
      (finally (.delete db)))))

(def expected-judge
  {"e1" "integral_inv_one_plus_x"
   "e2" nil "e3" nil "e4" nil "e5" nil "e6" nil "e7" nil
   "e8" "integral_inv_one_plus_x"
   "e9" "alternating_harmonic_eq_log_two"})

(deftest f3b-candidates-judgements-and-divergence
  (with-stack
    (fn [cfg]
      (let [alignment (f3b/read-alignment alignment-path)
            informal-bytes (f3/pinned-bytes "/home/joe/code/apm-lean"
                                              f3/historical-informal-commit
                                              f3/informal-path)
            ingest (f3/ingest-manifestations!
                    cfg [{:artifact/id "formal-f3:a93J02/authored-informal"
                          :role :authored-informal
                          :media-type "text/markdown; charset=utf-8"
                          :bytes informal-bytes}])
            revision (:artifact/revision-id (first ingest))
            plan (f3b/plan alignment revision)
            rows (f3b/materialise! cfg plan)
            ordinary (remove :divergence? rows)
            divergence (first (filter :divergence? rows))]
        (testing "all nine steps retain every candidate with stable distinct ids"
          (is (= (set (keys expected-judge)) (set (:steps plan))))
          (is (= 27 (count ordinary)))
          (is (= 27 (count (set (map #(get-in % [:claim :id]) ordinary)))))
          (is (every? #(every? :ok (:writes %)) rows)))
        (testing "model evidence is candidate evidence, never acceptance"
          (is (every? #(= :model (get-in % [:evidence :basis])) ordinary))
          (is (not-any? #(= :accepted (get-in % [:model-assessment :verdict]))
                        ordinary))
          (is (not-any? #(= :contradicted (get-in % [:evidence :standing])) rows)))
        (testing "Joe's verdict is pinned per step and keeps overruled links"
          (doseq [[step-id endorsed] expected-judge
                  :let [step-rows (filter #(= step-id (:step/id %)) ordinary)]]
            (is (= 3 (count step-rows)) step-id)
            (doseq [row step-rows]
              (let [decl (get-in row [:candidate :decl])
                    assessment (:human-assessment row)]
                (is (= (if (= decl endorsed) :accepted :rejected)
                       (:verdict assessment)) (str step-id " " decl))
                (is (= f3b/joe-quote (:quote assessment)))
                (is (= f3b/joe-recorded-at (:time assessment)))
                (is (= [{:turn/id f3b/joe-turn-id
                         :evidence/id f3b/joe-evidence-id}]
                       (:source-refs assessment))))))
        (testing "partial, ambiguous and methodological divergence stay typed"
          (doseq [row (filter #(nil? (expected-judge (:step/id %))) ordinary)]
            (is (= :partial (get-in row [:evidence :coverage])))
            (is (= :ambiguous (get-in row [:evidence :content :resolution/state]))))
          (is (= :methodological-divergence
                 (get-in divergence [:claim :asserted-content :relation/kind])))
          (is (= false (get-in divergence [:evidence :content :contradiction?])))
          (is (= :disputed (get-in divergence [:evidence :standing])))
          (is (= :accepted (get-in divergence [:human-assessment :verdict]))))
        (testing "the unused helper is explicit rather than dropped"
          (is (= ["tsum_alternating_harmonic_mul_pow"]
                 (:unaligned-declarations plan)))))))))
