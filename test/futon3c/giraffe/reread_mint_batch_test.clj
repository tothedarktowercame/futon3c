(ns futon3c.giraffe.reread-mint-batch-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.giraffe.reread-mint-batch :as batch])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- state-file []
  (io/file (.toFile (Files/createTempDirectory
                     "reread-mint-batch-" (make-array FileAttribute 0)))
           "state.edn"))

(deftest landings-coalesce-for-window-and-idle-forces-final-mint
  (let [path (state-file) calls (atom 0)
        mint #(do (swap! calls inc) {:status :minted :run-id (str "run-" @calls)})
        opts {:path path :window-ms 1800000 :mint-fn mint}]
    (is (= :minted (:status (batch/step! (assoc opts :now-ms 0 :landed-count 1
                                                 :queue-idle? false)))))
    (is (= :deferred (:status (batch/step! (assoc opts :now-ms 100 :landed-count 1
                                                   :queue-idle? false)))))
    (is (= :deferred (:status (batch/step! (assoc opts :now-ms 200 :landed-count 2
                                                   :queue-idle? false)))))
    (is (= 1 @calls))
    (is (= :minted (:status (batch/step! (assoc opts :now-ms 300 :landed-count 0
                                                 :queue-idle? true)))))
    (is (= 2 @calls))
    (is (= 0 (:pending-landings (batch/read-state path))))))

(deftest concurrent-steps-serialize-mints-and-preserve-landings
  ;; Hold the first read open while a second caller attempts step!. Without
  ;; the locks both read the old state: duplicate mint / lost landing.
  (doseq [due? [true false]]
    (let [path (state-file)
          first-read (promise) second-read (promise) second-started (promise)
          release (promise) reads (atom 0) mints (atom 0)
          original-read batch/read-state
          opts {:path path :window-ms 100 :now-ms (if due? 1000 1)
                :landed-count (if due? 0 1) :queue-idle? false
                :mint-fn #(do (swap! mints inc) {:status :minted :run-id "invented-run"})}]
      (spit path (pr-str {:pending-landings (if due? 1 0)
                         :last-mint-at-ms 0 :last-result nil}))
      (with-redefs [batch/read-state
                    (fn [p]
                      (let [state (original-read p)]
                        (if (= 1 (swap! reads inc))
                          (do (deliver first-read true) @release)
                          (deliver second-read true))
                        state))]
        (let [a (future (batch/step! opts))]
          (try
            (is (= true (deref first-read 2000 :timeout)))
            (let [b (future (deliver second-started true) (batch/step! opts))]
              (is (= true (deref second-started 2000 :timeout)))
              (is (= :blocked (deref second-read 100 :blocked))
                  "the second caller cannot read until the first writes")
              (deliver release true)
              (is (map? (deref a 2000 :timeout)))
              (is (map? (deref b 2000 :timeout))))
            (finally (deliver release true)))))
      (is (= (if due? 1 0) @mints))
      (is (= (if due? 0 2) (:pending-landings (original-read path)))))))

(deftest cl60-unchanged-is-a-successful-batch
  (let [path (state-file)
        result (batch/step! {:path path :window-ms 100 :now-ms 500
                             :landed-count 3 :queue-idle? true
                             :mint-fn (constantly {:status :unchanged :run-id "invented-existing"})})]
    (is (= :unchanged (:status result)))
    (is (= 0 (:pending-landings result)))
    (is (= 500 (:last-mint-at-ms result)))
    (is (= {:status :unchanged :run-id "invented-existing"} (:last-result result)))
    (is (= 0 (:pending-landings (batch/read-state path))))))
