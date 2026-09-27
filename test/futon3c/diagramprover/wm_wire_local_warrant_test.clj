(ns futon3c.diagramprover.wm-wire-local-warrant-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.evidence.http-backend :as http-backend]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.sqlite-backend :as sqlite])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- append-run! [backend id namespace ran-at warrant?]
  (let [intent (registry/append-record!
                 backend
                 {:kind :intent :author "wm-wire-local-test" :run/id id
                  :namespace namespace :command ["clojure" "-M:test" "-n" namespace]
                  :ran-at ran-at}
                 nil)]
    (registry/append-record!
      backend
      {:kind :run :author "wm-wire-local-test" :run/id id
       :namespace namespace :command ["clojure" "-M:test" "-n" namespace]
       :ran-at ran-at :finished-at ran-at :warrant? warrant? :git-head id}
      (:evidence/id intent))))

(deftest local-warrant-resolution
  (let [dir (.toFile (Files/createTempDirectory
                       "wm-wire-warrant-" (make-array FileAttribute 0)))
        db (io/file dir "registry.sqlite")
        backend (sqlite/sqlite-backend db)]
    (try
      (append-run! backend "passing-commit" "wire.passing-test"
                   "2026-09-27T01:00:00Z" true)
      (append-run! backend "older-pass" "wire.latest-test"
                   "2026-09-27T02:00:00Z" true)
      (append-run! backend "newer-fail" "wire.latest-test"
                   "2026-09-27T03:00:00Z" false)
      (binding [w/*warrant-store-path* (str db)]
        (testing "a passing local run resolves with its unchanged evidence"
          (let [found (w/latest-local-run "wire.passing-test")]
            (is (true? (get-in found [:payload :warrant?])))
            (is (= "passing-commit" (get-in found [:payload :git-head])))
            (is (= (:evidence/id (sqlite/latest-run-for-namespace
                                   backend "wire.passing-test"))
                   (:evidence/id found)))))
        (testing "absence is local and typed"
          (is (= {:record/type :absent :reason :no-local-record
                  :namespace "wire.missing-test"}
                 (w/latest-local-run "wire.missing-test"))))
        (testing "the newest run decides even when it failed"
          (let [found (w/latest-local-run "wire.latest-test")]
            (is (false? (get-in found [:payload :warrant?])))
            (is (= "newer-fail" (get-in found [:payload :git-head])))))
        (testing "an unreachable legacy HTTP setting cannot affect resolution"
          (with-redefs [http-backend/make-http-backend
                        (fn [_]
                          (throw (ex-info "HTTP must not be constructed"
                                          {:url "http://127.0.0.1:9"})))]
            (is (= "passing-commit"
                   (get-in (w/latest-local-run "wire.passing-test")
                           [:payload :git-head])))
            (is (= :no-local-record
                   (:reason (w/latest-local-run "wire.still-missing-test")))))))
      (finally
        (doseq [file (reverse (file-seq dir))]
          (io/delete-file file true))))))
