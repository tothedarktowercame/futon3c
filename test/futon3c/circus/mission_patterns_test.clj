(ns futon3c.circus.mission-patterns-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [futon3c.xiang.turn-store :as turns]
            [futon3c.circus.routes :as routes]))

(defn- store []
  (turns/store (str (java.nio.file.Files/createTempDirectory
                    "mission-index-test-" (make-array java.nio.file.attribute.FileAttribute 0)))))

(deftest mission-write-paths-and-later-assignment
  (let [s (store)
        a (turns/write-record! s {:mission_id "M-demo" :created_at "2026-01-01"})
        b (turns/write-record-id! s "turn-demo" {:created_at "2026-01-02"})]
    (is (= [{:id (:id a) :created_at "2026-01-01"}] (:turns (turns/mission-index s "M-demo"))))
    (turns/update-record! s (:id b) #(assoc % :mission_id "M-demo"))
    (turns/update-record! s (:id b) identity)
    (is (= [(:id a) (:id b)] (mapv :id (:turns (turns/mission-index s "M-demo")))))
    (turns/write-record-id! s "turn-fixed" {:mission_id "M-other"})
    (is (= ["turn-fixed"] (mapv :id (:turns (turns/mission-index s "M-other")))))
    (is (nil? (turns/mission-index s "M-missing")))))

(deftest backfill-is-dry-by-default-and-second-write-does-nothing
  (let [s (store)
        a (turns/write-record! s {:origin "operator" :session_id "invented-session" :mission_id "M-demo"})
        _ (turns/write-record! s {:origin "operator" :session_id "invented-session" :mission_id "M-demo"})
        f (io/file (:dir s) "mission-index" "M-demo.json")]
    (is (.delete f))
    (with-redefs [turns/list-records (fn [& _] (throw (ex-info "scan forbidden" {})))]
      (is (= 2 (:missing (turns/backfill-mission-index! s))))
      (is (not (.exists f)))
      (is (= 2 (:missing (turns/backfill-mission-index! s :write? true))))
      (is (some #{(:id a)} (map :id (:turns (turns/mission-index s "M-demo")))))
      (.setLastModified f 1000)
      (let [body (slurp f)]
        (is (= 0 (:missing (turns/backfill-mission-index! s :write? true))))
        (is (= 1000 (.lastModified f)))
        (is (= body (slurp f)))))))

(deftest mission-patterns-counts-bounds-and-reuses-cache
  (let [s (store)
        a (turns/write-record! s {:mission_id "M-demo" :session_id "s-a" :created_at "2026-01-01T00:00:00Z"
                                 :lu_ranked {:ranked [{:id "p-a"} {:id "p-a"} {:id "p-b"}]}})
        b (turns/write-record! s {:mission_id "M-demo" :session_id "s-b" :created_at "2026-01-02T00:00:00Z"
                                 :lu_ranked {:ranked [{:id "p-a"}]}
                                 :pattern_verdicts {:patterns [{:pattern "p-a" :status "partial"}
                                                               {:pattern "p-c" :status "occurrence"}]}})
        calls (atom [])
        cfg {:turn-store s :strand-cache (routes/strand-cache)
             :turn-gcc-fn (fn [{:keys [turn-id]}]
                            (swap! calls conj turn-id)
                            (let [r (turns/read-record s turn-id)]
                              {:turn r :lu-ranked (:lu_ranked r) :pattern-verdicts (:pattern_verdicts r)}))}]
    (with-redefs [turns/list-records (fn [& _] (throw (ex-info "scan forbidden" {})))]
      (is (= {"mission" "M-absent" "turns_read" 0 "patterns" [] "unindexed" true}
             (:body (routes/mission-patterns-response cfg {"mission" "M-absent"}))))
      (let [body (:body (routes/mission-patterns-response cfg {"mission" "M-demo" "n" "1"}))]
        (is (= 1 (get body "turns_read")))
        (is (= [(:id b)] @calls))
        (is (= ["p-a" "p-c"] (mapv #(get % "id") (get body "patterns")))))
      (let [body (:body (routes/mission-patterns-response cfg {"mission" "M-demo"}))
            p (first (get body "patterns"))]
        (is (= 2 (get body "turns_read")))
        (is (= {"id" "p-a" "proposed" 2 "occurs" 1 "sessions" ["s-b" "s-a"]
                "latest_at" "2026-01-02T00:00:00Z" "latest_turn" (:id b)} p))
        (is (= [(:id b) (:id a)] @calls)))
      (with-redefs [turns/read-record (fn [& _] (throw (ex-info "cache missed" {})))]
        (is (= 200 (:status (routes/mission-patterns-response cfg {"mission" "M-demo"}))))))))

(deftest mission-patterns-cap-and-missing-parameter
  (let [s (store)
        record {:mission_id "M-many" :created_at "2026-01-01T00:00:00Z"
                :lu_ranked {:ranked (mapv #(hash-map :id (str "invented-" %)) (range 105))}}
        _ (turns/write-record! s record)
        cfg {:turn-store s :strand-cache (routes/strand-cache)
             :turn-gcc-fn (fn [_] {:turn record :lu-ranked (:lu_ranked record)})}]
    (is (= 100 (count (get-in (routes/mission-patterns-response cfg {"mission" "M-many"}) [:body "patterns"]))))
    (is (= 400 (:status (routes/mission-patterns-response cfg {}))))))
