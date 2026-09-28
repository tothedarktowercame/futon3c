(ns futon3c.diagramprover.wm-wire-local-warrant-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.local-port :as local-port]
            [futon3c.test-registry.sqlite-backend :as sqlite])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- append-current-run! [store namespace root file]
  (registry/append-record!
   store
   {:kind :run :author "wm-wire-local-test" :run/id (str (random-uuid))
    :namespace namespace :command ["clojure" "-M:test" "-n" namespace]
    :ran-at "2026-09-28T00:00:00Z" :finished-at "2026-09-28T00:00:01Z"
    :warrant? true :git-head "fixture-head" :repo/root (.getPath root)
    :load-closure [] :test-files {(.getName file) (registry/file-sha file)}}
   nil))

(defn- declaration []
  {:wire [:writer :reader :value]
   :second-layer
   {:test `local-warrant-resolution
    :kind :value-varying :product [:derived] :intervention :before-reader}})

(defn- context []
  {:allowed-nses #{'futon3c.diagramprover.wm-wire-local-warrant-test}
   :lookup (memoize w/latest-local-run)
   :record-only? (constantly false)})

(deftest local-warrant-resolution
  (let [dir (.toFile (Files/createTempDirectory
                      "wm-wire-warrant-" (make-array FileAttribute 0)))
        file (io/file dir "fixture.clj")
        db (io/file dir "registry.sqlite")
        store (sqlite/sqlite-backend db)
        namespace "futon3c.diagramprover.wm-wire-local-warrant-test"]
    (try
      (spit file "(ns fixture)\n")
      (append-current-run! store namespace dir file)
      (binding [w/*warrant-store-path* (str db)
                local-port/*repo-roots* {"futon3c" (.getPath dir)}]
        (testing "an unchanged warrant is current"
          (is (= {:warrant-id (:evidence/id (sqlite/latest-run-for-namespace store namespace))
                  :git-head "fixture-head" :ran-at "2026-09-28T00:00:00Z"}
                 (:evidence (w/second-layer (declaration) (context))))))
        (testing "a changed recorded file requests exactly one rerun"
          (spit file "(ns changed)\n")
          (let [first-evidence (:evidence (w/second-layer (declaration) (context)))
                second-evidence (:evidence (w/second-layer (declaration) (context)))
                requests (sqlite/rerun-requests store {:namespace namespace})]
            (is (= :stale-warrant (:absent first-evidence)))
            (is (= :queued (:request-state first-evidence)))
            (is (integer? (:request-id first-evidence)))
            (is (= (:request-id first-evidence) (:request-id second-evidence)))
            (is (= 1 (count requests)))))
        (testing "an absent namespace requests one run and remains absent"
          (let [missing-db (io/file dir "missing.sqlite")]
            (binding [w/*warrant-store-path* (str missing-db)]
              (let [evidence (:evidence (w/second-layer (declaration) (context)))
                    missing-store (sqlite/sqlite-backend missing-db)]
                (is (= :no-warrant (:absent evidence)))
                (is (= :queued (:request-state evidence)))
                (is (= 1 (count (sqlite/rerun-requests
                                 missing-store {:namespace namespace})))))))))
      (finally
        (doseq [entry (reverse (file-seq dir))]
          (io/delete-file entry true))))))
