(ns futon3c.test-registry.local-port-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.increment-attestation :as increment]
            [futon2.aif.observation-checks :as checks]
            [futon2.aif.registry-port :as port]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.local-port :as local-port]
            [futon3c.test-registry.sqlite-backend :as sqlite]))

(defn temp-db []
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "registry-port" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (.getPath (io/file dir "registry.sqlite"))))

(defn append-run! [store payload]
  (:evidence/id (registry/append-record! store payload nil)))

(deftest real-sqlite-implementation-supplies-both-futon2-readers
  (let [path (temp-db)
        store (sqlite/sqlite-backend path)
        namespace "futon3c.test-registry.local-port-test"
        test-path "test/futon3c/test_registry/local_port_test.clj"
        common {:kind :run :author "wm-author" :repo/root "/home/joe/code/futon3c"
                :ran-at "2026-09-27T23:30:00Z" :finished-at "2026-09-27T23:31:00Z"
                :warrant? true :postcheck {:status :matched}
                :results {:tests 1 :assertions 1 :failures 0 :errors 0}
                :code-files {} :test-files {test-path (checks/content-sha test-path)}}
        c8-id (append-run! store
                (assoc common :run/id "c8" :command ["clojure" "-M:test" "-n" namespace]))
        increment-id (append-run! store
                       (assoc common :run/id "increment"
                              :repo/root "/home/joe/code/futon2"
                              :command (increment/registration-command
                                        (increment/declarations))))]
    (local-port/install! path)
    (is (port/installed?))
    (let [result (checks/check-registered-run
                  {:repo "futon3c" :namespace namespace
                   :agency-base "http://127.0.0.1:9"})]
      (is (true? (:observed result)) (pr-str result))
      (is (= c8-id (get-in result [:evidence :warrant-id]))))
    (let [result (increment/increment-evidence
                  {:agency-base "http://127.0.0.1:9"}
                  {:author "wm-author" :since "2026-09-27T23:29:00Z"})]
      ;; The C8 run does not cover the increment scope; the increment run is
      ;; the only qualifying local record and is returned through the port.
      (is (= increment-id (:warrant-id result)) (pr-str result)))))
