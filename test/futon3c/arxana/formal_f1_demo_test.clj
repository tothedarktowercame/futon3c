(ns futon3c.arxana.formal-f1-demo-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.formal-f1-demo :as demo]
            [futon3c.arxana.http :as http]))

(defn- free-port []
  (with-open [s (java.net.ServerSocket. 0)] (.getLocalPort s)))

(deftest private-stack-is-deterministic-and-public
  (let [port (free-port)
        first-handle (demo/start! port)]
    (try
      (let [first-recording (demo/recording first-handle)
            ids (fn [view] (mapv :interpretation/id (:projection/fragments view)))]
        (is (http/port-open? "127.0.0.1" port))
        (is (seq (ids (:outline first-recording))))
        (is (= :truncated (get-in first-recording [:gcc :result/status])))
        (is (= :limit (get-in first-recording [:gcc :result/truncation])))
        (demo/stop! first-handle)
        (let [second-handle (demo/start! port)]
          (try
            (let [second-recording (demo/recording second-handle)]
              (is (= (:manifest/revision-id first-recording)
                     (:manifest/revision-id second-recording)))
              (is (= (ids (:outline first-recording))
                     (ids (:outline second-recording)))))
            (finally (demo/stop! second-handle)))))
      (finally
        ;; stop! is idempotent enough for a partially completed assertion path.
        (when (http/port-open? "127.0.0.1" port)
          (demo/stop! first-handle))))))

(deftest occupied-public-port-is-refused-before-store-start
  (with-open [socket (java.net.ServerSocket. 0)]
    (let [port (.getLocalPort socket)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"port-in-use"
                            (demo/start! port))))))
