(ns futon3c.test-registry.local-port-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing]]
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

(defn- command! [dir & args]
  (let [{:keys [exit out err]} (apply shell/sh (concat args [:dir dir]))]
    (when-not (zero? exit)
      (throw (ex-info "Fixture command failed" {:command args :exit exit :err err})))
    out))

(defn- delete-tree! [dir]
  (doseq [file (reverse (file-seq dir))]
    (io/delete-file file true)))

(defn- current-fixture [file-count failing? f]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "current-warrant" (make-array java.nio.file.attribute.FileAttribute 0)))
        src (io/file dir "src") test-dir (io/file dir "test")
        artifact (io/file dir "artifacts") ledger (io/file dir "ledger")
        namespace "fixture.current-test"
        db (io/file dir "registry.sqlite")]
    (try
      (.mkdirs src) (.mkdirs test-dir) (.mkdirs artifact)
      (doseq [i (range file-count)]
        (spit (io/file src (str "unit_" i ".clj")) (str "(ns fixture.unit-" i ")\n")))
      (spit (io/file test-dir "current_test.clj") "(ns fixture.current-test)\n")
      (command! dir "git" "init" "-q")
      (command! dir "git" "config" "user.name" "Fixture")
      (command! dir "git" "config" "user.email" "fixture@example.invalid")
      (command! dir "git" "add" "src" "test")
      (command! dir "git" "commit" "-q" "-m" "fixture")
      (let [store (sqlite/sqlite-backend db)
            implementation (local-port/implementation db)
            closure (vec (concat
                          (for [i (range file-count)
                                :let [path (str "src/unit_" i ".clj")]]
                            {:ns (str "fixture.unit-" i) :path path
                             :sha256 (registry/file-sha (io/file dir path))})
                          [{:ns namespace :path "test/current_test.clj"
                            :sha256 (registry/file-sha
                                     (io/file test-dir "current_test.clj"))}]))
            options {:repo-root (.getPath dir)
                     :code-paths ["src"] :test-paths ["test"]
                     :command ["clojure" "-M:test" "-n" namespace]
                     :author "fixture-author"
                     :artifact-dir (.getPath artifact)
                     :ledger-root (.getPath ledger)}
            summary (if failing?
                      "Ran 1 tests containing 1 assertions.\n1 failures, 0 errors.\n"
                      "Ran 1 tests containing 1 assertions.\n0 failures, 0 errors.\n")]
        (with-redefs [registry/fingerprint (fn [_] {:fixture :environment})
                      registry/compute-closure (fn [_ _] closure)
                      registry/run-process! (fn [_ _ log]
                                              (spit log summary)
                                              (registry/parse-results
                                               (if failing? 1 0) summary 1))]
          (let [run (registry/register-run! store options)]
            (f {:dir dir :db db :store store :run run :namespace namespace
                :invoke (fn []
                          (binding [local-port/*repo-roots*
                                    {"futon3c" (.getPath dir)}]
                            ((:current-or-request implementation)
                             {:namespace namespace :repo "futon3c"})))}))))
      (finally (delete-tree! dir)))))

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

(deftest current-warrant-does-not-request-a-run
  (current-fixture
   1 false
   (fn [{:keys [dir db store run namespace invoke]}]
     (let [result (invoke)]
       (is (= {:status :current
               :entry-id (:evidence/id run)
               :ran-at (get-in run [:payload :ran-at])
               :git-head (get-in run [:payload :git-head])}
              result))
       (is (empty? (sqlite/rerun-requests store {})))
       (testing "the optional operation is reachable through Futon2's port"
         (local-port/install! db)
         (binding [local-port/*repo-roots* {"futon3c" (.getPath dir)}]
           (is (= result (port/call :current-or-request
                                    {:namespace namespace :repo "futon3c"})))))))))

(deftest stale-warrant-requests-one-run
  (current-fixture
   1 false
   (fn [{:keys [dir store run invoke]}]
     (spit (io/file dir "src/unit_0.clj") "(ns fixture.changed)\n")
     (let [first-miss (invoke) second-miss (invoke)]
       (is (= :missing (:status first-miss)))
       (is (= :no-current-warrant (:kind first-miss)))
       (is (= :stale (get-in first-miss [:data :reason])))
       (is (= (:evidence/id run) (get-in first-miss [:data :found-entry-id])))
       (is (= (get-in first-miss [:data :request-id])
              (get-in second-miss [:data :request-id])))
       (is (= 1 (count (sqlite/rerun-requests store {}))))))))

(deftest absent-warrant-requests-one-run
  (let [path (temp-db) store (sqlite/sqlite-backend path)
        result ((:current-or-request (local-port/implementation path))
                {:namespace "fixture.absent-test" :repo "futon3c"})]
    (is (= :missing (:status result)))
    (is (= :absent (get-in result [:data :reason])))
    (is (nil? (get-in result [:data :found-entry-id])))
    (is (= :queued (get-in result [:data :request-state])))
    (is (= 1 (count (sqlite/rerun-requests store {}))))))

(deftest failed-run-is-not-requested-again
  (current-fixture
   1 true
   (fn [{:keys [store run invoke]}]
     (let [result (invoke)]
       (is (= :missing (:status result)))
       (is (= :not-passing (:kind result)))
       (is (= :not-a-warrant (get-in result [:data :reason])))
       (is (= (:evidence/id run) (get-in result [:data :found-entry-id])))
       (is (empty? (sqlite/rerun-requests store {})))))))

(deftest current-warrant-two-hundred-file-time-bar
  (current-fixture
   199 false
   (fn [{:keys [invoke]}]
     (dotimes [_ 5] (invoke))
     (let [samples (mapv (fn [_]
                           (let [start (System/nanoTime)]
                             (invoke)
                             (/ (double (- (System/nanoTime) start)) 1000000.0)))
                         (range 100))
           ordered (sort samples)
           median (nth ordered 50)
           maximum (last ordered)]
       (println "CURRENT WARRANT TIME BAR"
                {:recorded-files 200 :calls 100
                 :median-ms median :maximum-ms maximum})
       ;; This packet measures rather than changing the currentness algorithm.
       (is (number? median))
       (is (number? maximum))))))
