(ns futon3c.mission-control.service-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.mission-control.service :as mcs])
  (:import [java.io File]
           [java.util UUID]))

(defn- temp-dir! []
  (let [d (File. "/tmp" (str "futon3c-mc-service-test-" (UUID/randomUUID)))]
    (.mkdirs d)
    (.getAbsolutePath d)))

(defn- rm-rf! [^String path]
  (let [root (File. path)]
    (when (.exists root)
      (doseq [f (reverse (file-seq root))]
        (.delete ^File f)))))

(use-fixtures
  :each
  (fn [f]
    (let [tmp (temp-dir!)
          snapshot (str tmp "/sessions.edn")]
      (mcs/reset-service!)
      (mcs/configure! {:snapshot-path snapshot
                       :repos {:futon3c "/home/joe/code/futon3c"
                               :futon3b "/home/joe/code/futon3b"
                               :futon3a "/home/joe/code/futon3a"
                               :futon5 "/home/joe/code/futon5"}})
      (try
        (f)
        (finally
          (mcs/reset-service!)
          (rm-rf! tmp))))))

(deftest session-lifecycle-start-step-stop
  (testing "start/list/step/stop works for mission-control sessions"
    (let [start (mcs/start-session! {:author "joe"})
          sid (:session-id start)]
      (is (:ok start))
      (is (string? sid))

      (let [inv (mcs/step! sid :mc-inventory)
            rev (mcs/step! sid :mc-review)
            stop (mcs/stop-session! sid "done")
            sessions (mcs/list-sessions)]
        (is (:ok inv))
        (is (:ok rev))
        (is (:ok stop))
        (is (seq sessions))
        (is (= sid (:session-id (first sessions))))
        (is (= :stopped (:status (first sessions))))
        (is (>= (:step-count (first sessions)) 2))))))

(deftest run-review-and-resume
  (testing "run-review! leaves an active resumable session by default"
    (let [run (mcs/run-review! {:author "joe"})
          sid (:session-id run)]
      (is (:ok run))
      (is (string? sid))
      (is (= :active (get-in (mcs/get-session sid) [:session/status])))

      (is (:ok (mcs/stop-session! sid "pause")))
      (is (:ok (mcs/resume-session! sid)))
      (is (:ok (mcs/step! sid :mc-mana)))
      (is (= :active (get-in (mcs/get-session sid) [:session/status]))))))

(deftest snapshots-reload-on-service-restart
  (testing "sessions are restored from snapshot file after reset/configure"
    (let [run (mcs/run-review! {:author "joe"})
          sid (:session-id run)
          snapshot-path (get-in (mcs/status) [:config :snapshot-path])]
      (is (:ok run))
      (is (.exists (File. snapshot-path)))

      ;; Simulate process-local restart.
      (mcs/reset-service!)
      (mcs/configure! {:snapshot-path snapshot-path})

      (let [sessions (mcs/list-sessions)]
        (is (some #(= sid (:session-id %)) sessions))
        (is (= sid (-> sessions first :session-id)))))))
